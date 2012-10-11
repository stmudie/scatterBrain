PRO as_datfileloader_selectfile, event
  
  @as_scatterheader.macro
  
  Widget_Control, event.top, GET_UVALUE = as_datfileloader
  as_datfileloader.selectFile, event
END

PRO as_datfileloader_event, event
  
  @as_scatterheader.macro
  
  Widget_Control, event.top, GET_UVALUE = as_datfileloader
  as_datfileloader.event, event
END

PRO as_datfileloader::selectfile, event

  @as_scatterheader.macro
  
  IF event.clicks EQ 2 THEN BEGIN
  
    selected = Widget_Info(Widget_Info(event.id, /TREE_ROOT),/TREE_SELECT)
    nameList = list()
    treeIDs = list()
    FOREACH name, self.names DO treeIDs.add, Widget_Info(event.top, FIND_BY_UNAME = name)
    FOREACH s, selected DO BEGIN
      IF s GE 0 AND Where(s EQ treeIDs,/NULL) EQ !NULL THEN BEGIN 
        Widget_Control, s, GET_UVALUE = name
        nameList.add, name.filename
      ENDIF
    ENDFOREACH
    self.notify, {SELECT, filenames: nameList, Clicks: event.clicks}
    
  ENDIF
  
END

PRO as_datfileloader::event, event

  @as_scatterheader.macro

  CASE Tag_Names(event, /STRUCTURE) OF
    'WIDGET_BASE'  : BEGIN
                       baseGeom = Widget_Info(event.id, /GEOMETRY)
                       wTabID = Widget_Info(event.top, FIND_BY_UNAME = 'TAB CONTAINER')
                       FOREACH name, self.names DO BEGIN
                         treeID = Widget_Info(event.top, FIND_BY_UNAME = name)
                         treeGeom = Widget_Info(treeID, /GEOMETRY)
                         Widget_Control, treeID, XSIZE = baseGeom.XSize - self.diffXSize-2, YSIZE = baseGeom.YSize - self.diffYSize - 2
                       ENDFOREACH
                       Widget_Control, wTabID, XSIZE = baseGeom.XSize - self.diffXSize, YSIZE = baseGeom.YSize - self.diffYSize
                     END
    'WIDGET_KILL_REQUEST' : BEGIN
                              Obj_Destroy, self
                              RETURN
                            END
    'WIDGET_CONTEXT' : BEGIN
                        Widget_Control, event.id, GET_UVALUE = wContextBase
                        WIDGET_DISPLAYCONTEXTMENU, event.id, event.x, event.y, wContextBase
                        RETURN
                       END
    ELSE:
  ENDCASE

  widgetName = Widget_Info(event.id, /UNAME)
  
  Widget_Control, event.id, GET_UVALUE = uvalue
  
  CASE widgetName OF
      'REFRESH' : self.refresh
      'DESELECT' : Widget_Control, uvalue, SET_TREE_SELECT = 0
      'CONTOUR' : BEGIN
                    selected = Widget_Info(uvalue, /TREE_SELECT)
                    filenames = list()
                    FOREACH select, selected DO BEGIN
                      Widget_Control, select, GET_UVALUE = filename
                      filenames.add, filename.filename
                    ENDFOREACH
                    event = {CONTOUR, filenames:filenames}
                    self.notify, event
                  END
      'MOVIE' :
    ELSE :  
  ENDCASE

END

FUNCTION as_datfileloader::init, names, folders, GROUPLEADER = groupLeader, EXTENSION = extension, NOTIFYOBJECT = notifyObject 

  @as_scatterheader.macro

  IF Keyword_Set(notifyObject) THEN $
    IF TypeName(notifyObject[0]) EQ 'NOTIFY' $
      THEN self.notifyObject = List(notifyObject,/EXTRACT)

  IF ~KeyWord_Set(extension) THEN extension = 'dat'

  IF KeyWord_Set(groupLeader) THEN wBase = Widget_Base(GROUP_LEADER = groupLeader, TITLE = 'Dat Files', /TLB_KILL_REQUEST_EVENTS, /COLUMN, /TLB_SIZE_EVENTS, UNAME = 'BASE') ELSE wBase = Widget_Base(TITLE = 'Dat Files', /TLB_KILL_REQUEST_EVENTS, /COLUMN, /TLB_SIZE_EVENTS, UNAME = 'BASE')
  wTab = Widget_Tab(wBase, UNAME = 'TAB CONTAINER')
 
  refresh = Widget_Button(wBase, VALUE = 'Refresh', SCR_XSIZE = 100, UNAME = 'REFRESH')
 
  Widget_Control, wBase, /REALIZE
  Widget_Control, wBase, SET_UVALUE = self
 
  self.wBase = wBase
 
  IF N_Elements(names) GT 0 THEN self.addTab, names, folders, EXTENSION = extension

  XManager, 'as_datfileloader', wBase, /NO_BLOCK

  RETURN, 1

END

PRO as_datfileloader::notify, event
  
  @as_scatterheader.macro
  
  FOREACH notify, self.notifyObject DO IF Obj_Valid(notify) THEN notify.notify, event

END

PRO as_datfileloader::contour

  

END

PRO as_datfileloader::addTab, names, folders, EXTENSION = extension

  @as_scatterheader.macro  

  IF ~KeyWord_Set(extension) THEN self.extension = 'dat' ELSE self.extension = StrCompress(extension, /REMOVE_ALL)

  IF N_Elements(names) EQ 0 THEN RETURN
  IF N_Elements(names) NE N_Elements(folders) THEN RETURN

  IF ISA(names, 'LIST') THEN self.names = names ELSE self.names = List(names, /EXTRACT)
  IF ISA(folders, 'LIST') THEN self.folders = folders ELSE self.folders = List(folders, /EXTRACT)

  wTab = Widget_Info(self.wBase, FIND_BY_UNAME = 'TAB CONTAINER')

  Widget_Control, self.wBase, UPDATE = 0

  FOREACH name, self.names, key DO BEGIN
    wTabBase = Widget_Base(wTab, TITLE = name, /COLUMN)
    wTree = Widget_Tree(wTabBase, /MULTIPLE, /CONTEXT_EVENTS, /DRAGGABLE, UNAME = name)
    files = File_Search((self.folders)[key] + Path_Sep() + '*.' + self.extension, /NOSORT)
    
    FOREACH file, files DO leaf = Widget_Tree(wTree, VALUE = File_Basename(file), UVALUE = {DAT, filename : file}, EVENT_PRO = 'as_datfileloader_selectfile')
  
    wContextBase = Widget_Base(wTree, /CONTEXT_MENU, UNAME = 'TreeContextBase')
    
    Widget_Control, wTree, SET_UVALUE = wContextBase
     
    wDeselect = Widget_Button(wContextBase, VALUE = 'Deselect All', UVALUE = wTree, UNAME = 'DESELECT')
    wContour = Widget_Button(wContextBase, VALUE = 'Contour', UVALUE = wTree, UNAME = 'CONTOUR')
    wMovie = Widget_Button(wContextBase, VALUE = 'Movie', UVALUE = wTree, UNAME = 'MOVIE')
    
  ENDFOREACH

  Widget_Control, self.wBase, /UPDATE

  tabGeom = Widget_Info(wTab, /GEOMETRY)
  baseGeom = Widget_Info(self.wBase, /GEOMETRY)
 
  self.diffXSize = baseGeom.Scr_XSize - tabGeom.Scr_XSize
  self.diffYSize = baseGeom.Scr_YSize - tabGeom.Scr_YSize
 
  self.refresh
 
END

PRO as_datfileloader::refresh

  FOREACH name, self.names, key DO BEGIN
    files = File_Search((self.folders)[key] + Path_Sep() + '*.' + self.extension, /NOSORT)
    treeID = Widget_Info(self.wBase, FIND_BY_UNAME = name)
    children = Widget_Info(treeID, /ALL_CHILDREN)
    leaf = List()
    FOREACH child, children DO BEGIN
      IF Widget_Info(child,/TYPE) EQ 11 THEN Widget_Control, child, GET_VALUE = VALUE
      leaf.add, value 
    ENDFOREACH
    leaf = leaf.toArray()
    FOREACH file, files DO BEGIN
      IF Where(leaf EQ File_Basename(file), /NULL) EQ !NULL THEN leafID = Widget_Tree(treeID, VALUE = File_Basename(file), UVALUE = {DAT, filename : file}, EVENT_PRO = 'as_datfileloader_selectfile')
    ENDFOREACH
  ENDFOREACH
  
END

PRO as_datfileloader::Cleanup

  IF Widget_Info(self.wBase, /VALID) THEN Widget_Control, self.wBase, /DESTROY
  

END

PRO as_datfileloader__define

  void = {as_datfileloader, $
          wBase : 0L, $
          names : List(), $
          folders : List (), $
          extension : '', $
          notifyObject : List(), $
          diffXSize : 0, $
          diffYSize : 0 }

END