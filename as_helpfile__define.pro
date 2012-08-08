PRO AS_HelpFile_Event, event
  Widget_Control, event.top, GET_UVALUE = AS_HelpFile
  AS_HelpFile.event, event 
  
END

FUNCTION AS_HelpFile::INIT, TITLE=title, GROUP_LEADER = groupLeader

  IF self.HelpList EQ '' THEN self.HelpList = 'About'
  IF KeyWord_Set(groupLeader) THEN self.groupLeader = groupLeader
  IF KeyWord_Set(title) THEN self.title = title ELSE self.title = 'Help'

  ;self.showGui

  RETURN, 1

END

PRO AS_HelpFile::ShowGui

  IF self.groupLeader NE 0 THEN wHelpBase = Widget_Base(GROUP_LEADER = groupLeader, TITLE = self.title, XSIZE = 730, YSIZE = 800, /ROW) $ 
                           ELSE wHelpBase = Widget_Base(TITLE = self.title, XSIZE = 730, YSIZE = 800, /ROW)
  
  wHelpChoose = Widget_Tree(wHelpBase, XSIZE = 150,YSIZE = 790)
  self.wHelpText = Widget_Text(wHelpBase, SCR_XSIZE = 560, SCR_YSIZE = 790, FONT = 'Arial', /EDITABLE, /WRAP, /SCROLL)

  UnderscorePos = StrSplit(self.HelpList, '_')
  
  helpTopics = self.HelpList
  
  FOREACH u, UnderscorePos DO IF u NE 0 THEN StrPut, helpTopics, ' ', u-1

  helpTopics = StrSplit(helpTopics, ',',/EXTRACT)

  FOREACH h, helpTopics DO void = Widget_Tree(wHelpChoose, VALUE = h)
  
  Widget_Control, wHelpBase, /REALIZE
  Widget_Control, wHelpBase, SET_UVALUE = self
  
  XManager, 'AS_HelpFile', wHelpBase, /NO_BLOCK
  
END

FUNCTION AS_HelpFile::About

  RETURN, 'Australian Synchrotron Help Object Base Class.'

END

PRO AS_HelpFile::Display, text

  IF ~Widget_Info(self.wHelpText,/VALID) THEN self.showGui
  Widget_Control, self.wHelpText, SET_VALUE = text
  
END

PRO AS_HelpFile::event, event

  CASE Tag_Names(event,/STRUCTURE) OF
   
   'WIDGET_TREE_SEL' : BEGIN
                         Widget_Control, event.id, GET_VALUE = topic
                         IF topic NE '' THEN BEGIN
                           spacePos = StrSplit(topic, ' ')
                           FOREACH s, spacePos DO IF s NE 0 THEN StrPut, topic, '_', s-1
                           Call_Method, topic, self
                         ENDIF
                       END
   ENDCASE
END

PRO AS_HelpFile__Define

  void = {AS_HelpFile, $
          HelpList : '', $
          wHelpText: 0L, $
          groupLeader : 0L, $
          title : ''}

END