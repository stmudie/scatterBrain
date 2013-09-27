PRO as_twodsubtract_event, event
  Widget_Control, event.top, GET_UVALUE=as_twodsubract
  as_twodsubract.event, event

END

PRO as_twodsubtract::event, event

  widgetName = Widget_Info(event.id, /UNAME)
 
  CASE Tag_Names(event, /STRUCTURE_NAME) OF
    
    'WIDGET_DROP' : BEGIN
                      draggedWidgets = Widget_Info(event.drag_id, /TREE_DRAG_SELECT)
                      ID = event.ID
                      IF event.position NE 2 THEN BEGIN
                        ID = Widget_Info(event.ID, /PARENT)
                      ENDIF
                      IF Widget_Info(ID,/UNAME) EQ 'TREE BLANK' THEN BEGIN
                        leaves = Widget_Info(ID, /ALL_CHILDREN)
                        IF leaves[0] NE 0 THEN Widget_Control, leaves, /DESTROY
                        draggedWidgets = draggedWidgets[0]
                      ENDIF
                      widget_tree_move, draggedWidgets, ID, /COPY
                    END
    'WIDGET_BASE' : BEGIN
                      y = 180 + event.y - self.geom.ysize
                      IF y LT 50 THEN Y = 50
                      Widget_Control, Widget_Info(event.top, FIND_BY_UNAME = 'TREE'), YSIZE = y
                    END
    ELSE:
  ENDCASE
 
  CASE widgetName OF
    'SAVE' : self.save
    'CLEAR' : BEGIN
                leaves = Widget_Info(Widget_Info(self.twoDBase, FIND_BY_UNAME = 'TREE'),/ALL_CHILDREN)
                IF leaves[0] NE 0 THEN Widget_Control, leaves, /DESTROY
              END
    ELSE :
  ENDCASE

END

PRO as_twodsubtract::save

  blank = Widget_Info(Widget_Info(self.twoDBase, FIND_BY_UNAME='TREE BLANK'),/CHILD)
  samples = List(Widget_Info(Widget_Info(self.twoDBase, FIND_BY_UNAME='TREE'),/ALL_CHILDREN),/EXTRACT)
  
  Widget_Control, blank, GET_VALUE = blankName
  
  sampleNames = List()
  FOREACH sample, samples DO BEGIN
    Widget_Control, sample, GET_VALUE = tempName
    sampleNames.add, tempName
  ENDFOREACH
  
  Widget_Control, Widget_Info(self.twoDBase, FIND_BY_UNAME='PREFIX'), GET_VALUE = prefix
  
  prefix = StrJoin(strsplit(prefix, '[[:punct:]]',/REGEX,/EXTRACT))
  prefix = StrJoin(StrSplit(prefix, ' ',/EXTRACT),'_')

  Widget_Control, Widget_Info(self.twoDBase, FIND_BY_UNAME='PREFIX'), SET_VALUE = prefix
  
  self.notify, {TWODSUBTRACT, blank:blankName, samples:sampleNames, prefix:prefix}

END

FUNCTION as_twodsubtract::init, GROUP_LEADER = groupLeader, NOTIFYOBJECT = notifyObject

  IF Keyword_Set(notifyObject) THEN $
  IF TypeName(notifyObject[0]) EQ 'NOTIFY' $
  THEN self.notifyObject = List(notifyObject,/EXTRACT)

  twoDBase = Widget_Base(GROUP_LEADER = groupLeader,/COLUMN,/FLOATING, XSIZE = 200, /TLB_SIZE_EVENTS)
  self.twoDBase = twoDBase
  wTreeBlankLabel = Widget_Label(twoDBase, VALUE = 'Blank')
  wTreeBlank = Widget_Tree(twoDBase, /DRAGGABLE, /DROP_EVENTS, YSIZE = 40, UNAME = 'TREE BLANK')
  wTreeLabel = Widget_Label(twoDBase, VALUE = 'Samples')
  wTree = Widget_Tree(twoDBase, /DRAGGABLE, /DROP_EVENTS, YSIZE = 180, UNAME = 'TREE')
  
  wClear = Widget_Button(twoDBase, VALUE = 'Clear Samples', UNAME = 'CLEAR')
  wPrefix = CW_FIELD(twoDBase, TITLE = 'Prefix', UNAME = 'PREFIX')
  wSave = Widget_Button(twoDBase, VALUE = 'Save', UNAME = 'SAVE')
  
  Widget_Control, twoDBase, /REALIZE
  Widget_Control, twoDBase, SET_UVALUE = self
  
  self.geom = Widget_Info(twoDBase, /GEOMETRY)
  
  XManager, 'as_twodsubtract', twoDBase, /NO_BLOCK 
  
  RETURN, 1

END

PRO as_twodsubtract::notify, event

  @as_scatterheader.macro
  
  FOREACH notify, self.notifyObject DO IF Obj_Valid(notify) THEN notify.notify, event
  
END


PRO as_twodsubtract__define

  void = {as_twodsubtract, $
          twoDBase : 0L, $
          geom : {WIDGET_GEOMETRY}, $
          notifyObject : List() }

END