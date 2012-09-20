;*************************************************************************
; Copyright (c) 2008 Australian Synchrotron
;*************************************************************************
;+
; NAME:
; AS_AddMessage
;
; PURPOSE:
; This routine places contents of STRMES into the text widget textWID on widget base wBase.
; If textWID isn't given, then 'MESBOX' is assumed as the uname of the text widget.
;
; CATEGORY:
; Widgets.
;
; CALLING SEQUENCE:
;
; AS_AddMessage, strmes, wBase
;
; INPUTS:
; strmes:  Text to be written to the widget
; wBase: widget of the ID containing the widget
;
; KEYWORD PARAMETERS:
; textWID: Widget ID of the text widget that strmes is to be written into.
;
; MODIFICATION HISTORY:
;   Written by: David J. Cookson, 1-Mar-2003
; 21-May-2008 (STM) Update and renamed to AS convention. Added TEXTWID keyword to allow
;                   more general use. 
;-

FUNCTION AS_AddMessage, strmes, wBase, TEXTWID=textWID, SPLIT = split 

  @as_scatterheader.macro

  IF ~Keyword_Set(textWID) THEN textWID = 'MESBOX'

  IF Keyword_Set(split) THEN strmes = as_splitmessage(strmes, split)

	Widget_Control, Widget_Info(wBASE, FIND_BY_UNAME=textWID), SET_VALUE=strmes[0]
	Return, 1

END
