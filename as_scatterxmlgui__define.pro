PRO as_scatterXMLGUI_event, event
  
  @as_scatterheader.macro
  
  Widget_Control, event.handler, GET_UVALUE = scatterXMLGUI
  scatterXMLGUI.event, event
END

FUNCTION as_scatterXMLGUI::INIT, GROUP_LEADER = groupLeader, DOCK=dock, _REF_Extra=extra

  @as_scatterheader.macro

  IF KeyWord_Set(groupLeader) THEN BEGIN
    self.groupLeader = groupLeader
    IF KeyWord_Set(dock) THEN BEGIN
      self.logBase   = WIDGET_BASE(groupLeader, /TLB_SIZE_EVENTS, TLB_FRAME_ATTR = 2, UNAME='file tree base', UVALUE = self, /ROW)
    ENDIF ELSE BEGIN
      geom = Widget_Info(groupLeader, /GEOMETRY)
      self.logBase   = WIDGET_BASE(GROUP_LEADER=groupLeader, XOFFSET = geom.xoffset + geom.xsize + 2*geom.margin, /FLOATING, /TLB_SIZE_EVENTS, TLB_FRAME_ATTR = 2, UNAME='file tree base', UVALUE = self, /ROW) 
    ENDELSE
  ENDIF ELSE self.logBase   = WIDGET_BASE(/TLB_SIZE_EVENTS, TLB_FRAME_ATTR = 2, UNAME='file tree base', XOFFSET = 1, YOFFSET = 0, UVALUE = self)

  treeBase = Widget_Base(self.logBase, /COLUMN)
  searchFile = Widget_Text(treeBase, UNAME='Search File', /ALL_EVENTS, /EDITABLE)
  IF N_Elements(height) EQ 0 THEN BEGIN
    geom = Widget_Info(groupLeader, /GEOM)
    height = geom.scr_Ysize*.9
  ENDIF
  self.fileTreeParent = Widget_Tree(treeBase, /MULTIPLE, /CONTEXT, /DRAGGABLE, XSIZE = 200, YSIZE = height)
  self.fileTreeSearch = Widget_Tree(self.fileTreeParent, VALUE = 'Search Results', /FOLDER)

  self.wContextBase = WIDGET_BASE(treeBase, /CONTEXT_MENU, UNAME = 'TreeContextBase' )
  wDeselect = Widget_Button(self.wContextBase, VALUE = 'Deselect All', UNAME = 'Deselect')
  wExportProfiles = Widget_Button(self.wContextBase, VALUE = 'Save profiles to individual files', UNAME = 'Export Profiles')
  wExportProfilesSingle = Widget_Button(self.wContextBase, VALUE = 'Save profiles to one large file', UNAME = 'Export Profiles Single')
  wSum = Widget_Button(self.wContextBase, VALUE = 'Sum Images', UNAME = 'Sum Images')
  wContour = Widget_Button(self.wContextBase, VALUE = 'Contour', UNAME = 'Contour')
  wMovie = Widget_Button(self.wContextBase, VALUE = 'Movie', UNAME = 'Movie')
  wMosaic = Widget_Button(self.wContextBase, VALUE = 'Mosaic', UNAME = 'Mosaic')
  wSector = Widget_Button(self.wContextBase, VALUE = 'Sector', UNAME = 'Sector')
    
  Widget_Control, self.logBase, /REALIZE
  Widget_Control, self.logBase, SET_UVALUE = self
      
  Xmanager, 'as_scatterXMLGUI', self.logBase, EVENT_HANDLER = 'as_scatterXMLGUI_event', /NO_BLOCK
    
  RETURN, self->as_scatterXMLFile::INIT(_EXTRA = extra)

END

PRO as_scatterXMLGUI::event, event

  @as_scatterheader.macro

  CASE Tag_Names(event, /STRUCTURE) OF
   'WIDGET_TREE_SEL' : BEGIN
                        selected = Widget_Info(Widget_Info(event.id, /TREE_ROOT),/TREE_SELECT)
;                        whereClicked = Where(selected EQ event.id)
;                        IF whereClicked GT 0 AND whereClicked NE (N_Elements(selected) - 1) THEN BEGIN
;                          Widget_Control, Widget_Info(event.id, /TREE_ROOT), SET_TREE_SELECT=0
;                          Widget_Control, Widget_Info(event.id, /TREE_ROOT), SET_TREE_SELECT=event.id
;                          selected = event.id
;                        ENDIF
                        nameList = list()
                        FOREACH s, selected DO BEGIN
                          IF s GE 0 AND s NE self.filetree THEN BEGIN 
                            Widget_Control, s, GET_VALUE = name
                            nameList.add, name
                          ENDIF
                        ENDFOREACH
                        FOREACH n, self.notifyObject DO BEGIN
                          IF Obj_Valid(n) THEN n.notify, {Type: 'SELECT', Name: nameList, Clicks: event.clicks}
                        ENDFOREACH
                      END
   'WIDGET_CONTEXT' : BEGIN
                        WIDGET_DISPLAYCONTEXTMENU, event.id, event.x, event.y, self.wContextBase
                      END
    ELSE :
  ENDCASE

  widgetName = Widget_Info(event.id, /UNAME)

  CASE widgetName OF

    'Search File' : BEGIN
                      Widget_Control, self.fileTreeParent, SET_TREE_SELECT = 0
                      allSearchLeaves = Widget_Info(self.fileTreeSearch,/ALL_CHILDREN)
                      Widget_Control, self.fileTreeSearch, /DESTROY
                      ;IF allSearchLeaves[0] NE 0 THEN FOREACH leaf, allSearchLeaves DO Widget_Control, leaf, /DESTROY
                      self.fileTreeSearch = Widget_Tree(self.fileTreeParent, VALUE = 'Search Results', INDEX = 0, /FOLDER)
                      Widget_Control, event.id, GET_VALUE = search
                      IF search NE '' THEN BEGIN
                        fileNum = Where(StrMatch(File_Basename((*self.loglines).logline), '*'+search[0]+'*'), COMPLEMENT = unMatchedFiles) 
                        IF StrUpCase(search) EQ 'NIGEL' THEN BEGIN
                          self.notify, {Type : 'NIGEL'}
                        ENDIF 
                      ENDIF ELSE fileNum = -1
                      IF fileNum[0] NE -1 THEN BEGIN 
                        allLeaves = Widget_Info(self.fileTree,/ALL_CHILDREN)
                        numLeaves = N_Elements(allLeaves)
                        ;Widget_Control, allLeaves[numLeaves - 1 - fileNum[-1]], /SET_TREE_VISIBLE
                        ;Widget_Control, self.fileTreeParent, SET_TREE_SELECT = allLeaves[numLeaves - 1 - fileNum]
                        Widget_Tree_Move, Reverse(allLeaves[numLeaves - 1 - fileNum]), self.fileTreeSearch, /COPY
                                                
                        Widget_Control, self.fileTreeSearch, SET_TREE_EXPANDED = 1
                        Widget_Control, self.fileTreeParent, SET_TREE_SELECT = Widget_Info(self.fileTreeSearch,/ALL_CHILDREN)
                        Widget_Control, self.fileTreeSearch, SET_TREE_SELECT = 0
                        
                        nameList = list()
                        FOREACH s, Widget_Info(self.fileTreeSearch,/ALL_CHILDREN) DO BEGIN
                          IF s GE 0 THEN BEGIN 
                            Widget_Control, s, GET_VALUE = name
                            nameList.add, name
                          ENDIF
                        ENDFOREACH
                        
                        FOREACH n, self.notifyObject DO IF Obj_Valid(n) THEN n.notify, {Type: 'SELECT', Name: nameList, Clicks: 1}
                        
                      ENDIF
                    END
    'Export Profiles' : BEGIN
                          selected = Widget_Info(self.fileTreeParent,/TREE_SELECT)
                          nameList = list()
                          FOREACH s, selected DO BEGIN
                            Widget_Control, s, GET_VALUE = name
                            nameList.add, name
                          ENDFOREACH
                          self.notify, {Type: 'EXPORT PROFILES', Name: nameList, Clicks: -1}
                        END
    'Export Profiles Single' : BEGIN
                                 selected = Widget_Info(self.fileTreeParent,/TREE_SELECT)
                                 nameList = list()
                                 FOREACH s, selected DO BEGIN
                                   Widget_Control, s, GET_VALUE = name
                                   nameList.add, name
                                 ENDFOREACH
                                 self.notify, {Type: 'EXPORT PROFILES SINGLE FILE', Name: nameList, Clicks: -1}
                    END
    'Sum Images'  : BEGIN
                      selected = Widget_Info(self.fileTreeParent,/TREE_SELECT)
                      nameList = list()
                      FOREACH s, selected DO BEGIN
                        Widget_Control, s, GET_VALUE = name
                        nameList.add, name
                      ENDFOREACH
                      self.notify, {Type: 'SUM', Name: nameList, Clicks: -1}
                                            
                    END
    'Contour'     : BEGIN
                      
                      COMPILE_OPT idl2
                      ON_ERROR, 2
                      
                      selected = Widget_Info(self.fileTreeParent,/TREE_SELECT)
                      nameList = list()
                      FOREACH s, selected DO BEGIN
                        Widget_Control, s, GET_VALUE = name
                        nameList.add, name
                      ENDFOREACH
                      self.notify, {Type: 'CONTOUR', Name: nameList, Clicks: -1}
                      
                    END
    'Movie'       : BEGIN
                      selected = Widget_Info(self.fileTreeParent,/TREE_SELECT)
                      nameList = list()
                      FOREACH s, selected DO BEGIN
                        Widget_Control, s, GET_VALUE = name
                        nameList.add, name
                      ENDFOREACH
                      namelist.reverse
                      self.notify, {Type: 'MOVIE', Name: nameList, Clicks: -1}
                    END
    'Mosaic'      : BEGIN
                      selected = Widget_Info(self.fileTreeParent,/TREE_SELECT)
                      nameList = list()
                      FOREACH s, selected DO BEGIN
                        Widget_Control, s, GET_VALUE = name
                        nameList.add, name
                      ENDFOREACH
                      self.notify, {Type: 'MOSAIC', Name: nameList, Clicks: -1}
                      
                    END
    'Sector'      : BEGIN
                      selected = Widget_Info(self.fileTreeParent,/TREE_SELECT)
                      nameList = list()
                      FOREACH s, selected DO BEGIN
                        Widget_Control, s, GET_VALUE = name
                        nameList.add, name
                      ENDFOREACH
                      self.notify, {Type: 'SECTOR', Name: nameList, Clicks: -1}
                    END
    'Deselect'    : BEGIN
                      Widget_Control, self.fileTreeParent, SET_TREE_SELECT = 0
                      self.notify, {Type: 'DESELECT'}
                    END
    ELSE:
  
  ENDCASE

END

PRO as_scatterXMLGUI::NewLogLine, fname, exptime, i0, it, ibs, _REF_EXTRA = extra

  @as_scatterheader.macro

  self.as_scatterXMLFile::NewLogLine, fname, exptime, i0, it, ibs, _extra = extra
  fname = File_Basename(fname)
  void = Widget_Tree(self.fileTree, VALUE = fname, INDEX = 0)

END

PRO as_scatterXMLGUI::clear

  @as_scatterheader.macro

  IF self.fileTree NE 0 THEN BEGIN
    allLeaves =  Widget_Info(self.filetree,/ALL_CHILDREN)
    IF (allLeaves)[0] NE -1 THEN FOREACH leaf, allLeaves DO Widget_Control, leaf, /DESTROY
  ENDIF
  
  self.as_scatterXMLFile::clear

END

PRO as_scatterXMLGUI::SetProperty, HEIGHT = height

  @as_scatterheader.macro

  IF N_Elements(height) NE 0 THEN Widget_Control, self.fileTreeParent, YSIZE=height
  
END

PRO as_scatterXMLGUI::ParseFile, FILENAME=filename, LOGONLY=logOnly, UPDATE=update

  @as_scatterheader.macro

  self->as_scatterxmlfile::ParseFile, filename, LOGONLY=logOnly, UPDATE=update
  IF fileName EQ 'error' THEN RETURN
  startingNum = 0
  
  IF KeyWord_Set(update) THEN BEGIN
    IF Widget_Info(self.fileTree, /VALID_ID) THEN startingNum = Widget_Info(self.fileTree, /N_CHILDREN) ELSE self.fileTree = Widget_Tree(self.fileTreeParent, VALUE = filename, /FOLDER, /EXPANDED)
  ENDIF ELSE BEGIN
    IF Widget_Info(self.fileTree, /VALID_ID) THEN Widget_Control, self.fileTree, /DESTROY
      self.fileTree = Widget_Tree(self.fileTreeParent, VALUE = filename, /FOLDER, /EXPANDED)
  ENDELSE

  IF Size(*self.loglines, /TYPE) EQ 8 THEN numFiles = N_Elements((*self.loglines).logline) ELSE numFiles = 0 
  IF numFiles GT startingNum THEN BEGIN
    IF numFiles-startingNum GT 100 THEN progressBarObj = Obj_New('progressbar', /START, /NOCANCEL, TEXT='Updating File List')
    names = ((*self.loglines).logline)[startingNum:numFiles-1]
    name = File_Basename(names[0])
    void = Widget_Tree(self.fileTree, VALUE = name, INDEX = 0)
    count = 0
    IF N_Elements(names) GT 1 THEN BEGIN
      Widget_Control, self.fileTree, MAP = 0
      FOREACH name, names[1:*] DO BEGIN
        name = File_Basename(name)
        void = Widget_Tree(self.fileTree, VALUE = name, INDEX = 0)
        count += 1
        IF count mod 100 EQ 0 THEN IF Obj_Valid(progressBarObj) THEN progressBarObj->Update, (Float(count)/(numfiles-startingNum))*100
      ENDFOREACH
      Widget_Control, self.fileTree, MAP = 1
    ENDIF
  ENDIF
  IF Obj_Valid(progressBarObj) THEN progressBarObj->Destroy
END

PRO as_scatterXMLGUI__define

  void = {as_scatterXMLGUI, $
          INHERITS as_scatterxmlfile, $
          logBase            : 0L,    $
          groupLeader        : 0L,    $
          fileTreeParent     : 0L,    $
          fileTreeSearch     : 0L,    $
          fileTree           : 0L,    $
          wContextBase       : 0L     }

END