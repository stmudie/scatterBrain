PRO AS_PlotControl_event, event

  @as_scatterheader.macro

    id = Widget_Info(event.top, FIND_BY_UNAME='Plot Control Base')
    widget_control, id, GET_UVALUE=AS_PlotControl
    AS_PlotControl->event, event
END

PRO AS_SurfaceControl_event, event

  @as_scatterheader.macro
  
    widget_control, event.top, GET_UVALUE=AS_PlotControl
    AS_PlotControl->SurfaceEvent, event
END

PRO AS_PlotControl::UpDatePlot, _REF_EXTRA = extra

  @as_scatterheader.macro

  self.oProfiles->UpdatePlot, _EXTRA = extra

END



PRO AS_PlotControl::SurfaceEvent, event

  @as_scatterheader.macro

  IF Tag_Names(event, /STRUCTURE) EQ 'WIDGET_BASE' THEN BEGIN
    Widget_Control, Widget_Info(event.id, FIND_BY_UNAME = 'Surface Draw'), XSIZE = event.x, YSIZE = event.y
  ENDIF

END

PRO AS_PlotControl::event, event

@as_scatterheader.macro

widgetType = Tag_Names(event,/STRUCTURE)

IF widgetType EQ 'FSC_FIELD_EVENT' THEN BEGIN
  event.object.getproperty, name = widgetName
ENDIF ELSE widgetName = Widget_Info(event.ID, /UNAME)


CASE TAG_NAMES( event, /STRUCTURE ) OF
    'WIDGET_CONTEXT': BEGIN
                        wHideButton = WIDGET_INFO( self.wContextBase, FIND_BY_UNAME = 'Show/Hide' )
                        WIDGET_CONTROL, wHideButton, SET_UVALUE = WIDGET_INFO( event.id, /TREE_ROOT )
                        wApplyButton = Widget_Info( self.wContextBase, FIND_BY_UNAME = 'Apply Parameters')
                        selected = Widget_Info(event.id,/TREE_DRAG_SELECT)
                        IF selected[0] GE 0 THEN IF Widget_Info(selected[0],/PARENT) EQ self.wProfileTree $
                                                THEN Widget_Control, wApplyButton, SENSITIVE=1 $ 
                                                ELSE Widget_Control, wApplyButton, SENSITIVE=0 $
                        ELSE Widget_Control, wApplyButton, SENSITIVE=0
                        WIDGET_DISPLAYCONTEXTMENU, event.id, event.x, event.y, self.wContextBase
                      END
    'WIDGET_DROP'   : BEGIN
                        draggedWidgets = Widget_Info(event.drag_id, /TREE_DRAG_SELECT)
                        dropIsOnFolder = (event.position eq 2 && WIDGET_INFO( event.id, /TREE_FOLDER ) && event.id NE self.wProfileTree)
                        
                        IF event.drag_id NE self.wProfileTree THEN BEGIN
                          
                          numFiles = N_Elements(draggedWidgets)
                          IF numFiles GT 1 THEN self.oFrame.SetProperty, UPDATEIMAGE = 0  
                          
                          FOR i = 0, N_Elements(draggedWidgets) -  1 DO BEGIN
                            
                            IF numFiles GT 1 AND i EQ numFiles - 1 THEN self.oFrame.SetProperty, UPDATEIMAGE = 1
                            
                            IF draggedWidgets[i] NE Widget_Info(event.drag_id, /CHILD) THEN BEGIN
                            
                              Widget_Control, draggedWidgets[i], GET_UVALUE = uvalue
                              
                              IF ISA(uvalue, 'STRUCT') THEN self.notify, {PLOTDATDROP, filename : uvalue.filename} $
                              ELSE BEGIN
                                Widget_Control, draggedWidgets[i], GET_VALUE = f_name1
                                self.notify, {PLOTDROP, filename : f_name1}
                              ENDELSE
                              
                              ;f_name1 = saxs_get_filenames(/single)
                              ;if f_name1 EQ 'NULL' then return
                              ;retval = saxs_image_proc(f_name1, INFO = info)
                              
                                                                                     
                              ;sf=info.frame_obj->GetImage(f_name1)
                              ;setup_pts = info.frame_obj->CakeSetup()
                              ;IF info.frame_obj->cake() LE 1 THEN RETURN 
                              ;info.plotControl_obj->AddPlot, f_name1
                              
;                              WIDGET_CONTROL, Widget_Info(self.wGroupleader, FIND_BY_UNAME='SIMAGE'), SET_VALUE=f_name1
;                              
;                              self.oProfiles->UpdateProfileWidgets
                            ENDIF
                          ENDFOR
                        ENDIF ELSE BEGIN 
                        
                          dropIsOnFolder = 1
                        
                        ENDELSE
                        
                        IF dropIsOnFolder THEN BEGIN
                          allLeaves = Widget_Info(self.wProfileTree, /ALL_CHILDREN)
                          IF Where([allLeaves,self.wProfileTree] EQ event.id) EQ -1 THEN RETURN
                          selProfile = Widget_Info(event.drag_id, /TREE_DRAG_SELECT)
                          IF selProfile[0] EQ -1 THEN RETURN
;                          IF Widget_Info(event.id, /N_CHILDREN) GT 0 THEN BEGIN
;                            Widget_Control, Widget_Info(event.id, /CHILD), /DESTROY
;                            self.numBlanks = self.numBlanks - 1
;                          ENDIF
                          
                          numSelected = N_Elements(selProfile)                          
                          FOR i = 0, numSelected - 1 DO BEGIN
                            IF (Widget_Info(selProfile, /PARENT))[0] NE self.wProfileTree AND Widget_Info((Widget_Info(selProfile, /PARENT))[0],/PARENT) NE self.wProfileTree THEN BEGIN
                              IF numSelected EQ 1 THEN selProfile = allLeaves[N_Elements(allLeaves)-1] ELSE BEGIN
                                
                                names = StrArr(numSelected)
                                FOR j = 0, N_Elements(selProfile) - 1 DO BEGIN
                                  Widget_Control, selProfile[j], GET_VALUE = temp
                                  names[j] = temp
                                ENDFOR
                                FOR j = 0, N_Elements(selProfile) - 1 DO BEGIN
                                  Widget_Control, selProfile[j], GET_VALUE = fname
                                  selProfile[j] = allLeaves[(N_Elements(allLeaves) -  numSelected) + Where(names EQ fname)]
                                ENDFOR
                              ENDELSE
                            ENDIF 
                            Widget_Control, selProfile[i], GET_UVALUE = profile
                            Widget_Control, event.id, GET_UVALUE = blank
                            result = self.oProfiles->SetBlank(profile, blank)
                            IF result EQ -1 THEN RETURN
                            
;                            self.oProfiles->MoveProfile, profile, blank+1
;                            self.oProfiles->ReOrgColours
                            
                            ;self.oProfiles->GetProperty, COLOUR=profile
                            
;                            Widget_Control, event.id, GET_UVALUE=profileNum
;                            blankColourIndex = profileNum
;                            self.oProfiles->GetProperty, COLOUR=blankColourIndex
;                            
;                            self.plotPalette->GetProperty, RED_VALUES = red, GREEN_VALUES = green, BLUE_VALUES = blue
;                            
;                            red = red[]
;                            green[] =
;                            blue[] =
                            
                            
                          ENDFOR
                          widget_tree_move, selProfile, event.id, index=-1
                          IF event.id NE self.wProfileTree THEN BEGIN
                            Widget_Control, event.id, SET_TREE_BITMAP = 0               
                            Widget_Control, event.id, GET_UVALUE=profileNum
                            colourIndex = profileNum
                            self.oProfiles->GetProperty, COLOUR=colourIndex
                            self.plotpalette.GetProperty, N_COLORS = numColours
                            rgb = self.plotPalette->GetRGB(colourIndex < (numColours-1))
                            colourPlane = replicate(1,16)#replicate(1,16)
                            buttonColour = Byte([[[rgb[0]*colourPlane]],[[rgb[1]*colourPlane]],[[rgb[2]*colourPlane]]])
                            folderButton = [[[self.folder]],[[self.folder]],[[self.folder]]]+ buttonColour*[[[self.folderOpacity]],[[self.folderOpacity]],[[self.folderOpacity]]] 
                            Widget_Control, event.id, SET_TREE_BITMAP = folderButton, /SET_TREE_EXPANDED
                            tempName=Widget_Info(event.id, /UNAME)
                            Widget_Control, event.id, SET_UNAME = 'Blank Leaf ' + (StrSplit(tempName,/EXTRACT))[2]
                            self.numBlanks = self.numBlanks + 1
                          ENDIF
                                                    
                        ENDIF
                        
                        allLeaves = Widget_Info(self.wProfileTree, /ALL_CHILDREN)
                        tempLeaves = !Null
                        FOREACH leaf, allLeaves DO BEGIN
                          children = Widget_Info(leaf, /ALL_CHILDREN)
                          IF N_Elements(children) EQ 1 AND children[0] EQ 0 THEN BEGIN
                          
                            nameExtract = StrSplit(Widget_Info(leaf, /UNAME),/EXTRACT)
                            IF nameExtract[0] EQ 'Blank' THEN BEGIN
                              Widget_Control, leaf, SET_UNAME ='Profile ' + StrJoin(nameExtract[1:*], ' ', /SINGLE)
                              Widget_Control, leaf, GET_UVALUE=profileNum
                              colourIndex = profileNum
                              self.oProfiles->GetProperty, COLOUR=colourIndex
                              self.plotPalette.GetProperty, N_COLORS=numColours
                              rgb = self.plotPalette->GetRGB(colourIndex < (numColours-1))
                              colourPlane = replicate(1,16)#replicate(1,16)
                              buttonColour = Byte([[[rgb[0]*colourPlane]],[[rgb[1]*colourPlane]],[[rgb[2]*colourPlane]]])
                              IF Widget_Info(leaf,/VALID) THEN Widget_Control, leaf, SET_TREE_BITMAP = buttonColour  
                              
                            ENDIF
                          
                          ENDIF
                         
                          IF children[0] NE 0 THEN tempLeaves = [tempLeaves, Widget_Info(leaf, /ALL_CHILDREN)]
                        ENDFOREACH
                        
                        allLeaves = [allLeaves,tempLeaves]
                        
                        FOREACH leaf, allLeaves DO BEGIN
                          Widget_Control, leaf, GET_UVALUE =val
                          print, val
                        ENDFOREACH
                        
                      END
    'WIDGET_TREE_SEL' : BEGIN
                          
                           selProfiles = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
                           IF selProfiles[0] GE 0 THEN BEGIN 
                             plots = !Null
                             FOREACH prof, selProfiles DO BEGIN
                               Widget_Control, prof, GET_UVALUE = temp
                               plots = [plots,temp]
                             ENDFOREACH 
                             plotsTemp = plots
                             self.oProfiles->LineWidth, plotsTemp, 3, /ADD
                           ENDIF
                           IF self.lastSelected.Count() GT 0 THEN self.oProfiles->LineWidth, self.lastSelected.toArray(), -3, /ADD
                                   
                           IF event.clicks EQ 1 THEN BEGIN
                            IF N_Elements(plots) GE 1 THEN self.oProfiles->SelectPlot, plots ELSE self.oProfiles->SelectPlot, -1
                            IF selProfiles[0] GE 0 THEN BEGIN
                              Widget_Control, selProfiles[0], GET_UVALUE = temp
                              offset = (mult = temp)
                              self.oProfiles->GetProperty, MULT = mult, OFFSET = offset
                              infoArray = ['Mult = ' + String(mult),'Offset = ' + String(offset)]
                              Widget_Control, self.wInfoBox, SET_VALUE = infoArray
                              self.notify, {PLOTSELECT, filename : temp, clicks : 1 }
                            ENDIF 
                          ENDIF
                          IF event.clicks EQ 2 THEN BEGIN
                            selProfile = (WIDGET_INFO( self.wProfileTree, /TREE_SELECT ))[0]
                            IF selProfile EQ -1 THEN RETURN
                            Widget_Control, selProfile, GET_UVALUE = temp
                            self.notify, {PLOTSELECT, filename : temp, clicks : 2 }
                          ENDIF
                          
                          IF plots NE !Null THEN self.lastSelected = List(plots, /EXTRACT) ELSE self.lastSelected = List()
                          
                        END

     ELSE : BEGIN
             CASE widgetName OF

                  'Apply Parameters' :       BEGIN
                                     selProfile = (Widget_Info( self.wProfileTree, /TREE_SELECT ))
                                     children = !Null
                                     FOREACH p, selProfile DO children = [children,Widget_Info(p, /ALL_CHILDREN)]
                                     IF children[0] NE 0 THEN selProfile = [children,selProfile]
                                     index = IntArr(N_Elements(selProfile))
                                     FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                        IF selProfile[i] GT 0 THEN Widget_Control, selProfile[i], GET_UVALUE = temp
                                        index[i] = temp
                                     ENDFOR
                                     self->Replot, index
                                     
                                   END
                                   
                  
                  'Contour Plot' :  self.multiplot, /CONTOUR

                  'Surface Plot' :  self.multiplot, /SURFACE 
                  
                  'Mult' :         BEGIN
                                    
                                    selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
                                    index = IntArr(N_Elements(selProfile))
                                    FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                       Widget_Control, selProfile[i], GET_UVALUE = temp
                                       index[i] = temp
                                    ENDFOR
                                    
                                    mult = index
                                    self.oProfiles->GetProperty, MULT = mult
                                    
                                    valid = 0
                                    
                                    WHILE valid NE -1 DO BEGIN
                                    
                                      multTemp = TextBox(GROUP_LEADER = event.top, TITLE='Mult', VALUE = StrCompress(String(mult, FORMAT = '(F8.4)'),/REMOVE_ALL), CANCEL=cancelled)
                                      IF cancelled THEN RETURN
                                      valid = StRegex(multTemp,'[^0-9.]')>StRegex(multTemp,'\..*\.')
                                      IF Float(multTemp) EQ 0 THEN valid = 0
                                    ENDWHILE
                                    
                                    mult = Float(multTemp) 
                                    
                                    self.oProfiles->MultOffset, index, MULT = mult
                                    
                                   END
                  'Offset' :       BEGIN
                                    
                                    selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
                                    index = IntArr(N_Elements(selProfile))
                                    FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                       Widget_Control, selProfile[i], GET_UVALUE = temp
                                       index[i] = temp
                                    ENDFOR
                                    
                                    offset = index
                                    self.oProfiles->GetProperty, OFFSET = offset
                                    
                                    valid = 0
                                    
                                    WHILE valid NE -1 DO BEGIN 
                                    
                                      offsetTemp = TextBox(GROUP_LEADER = event.top,TITLE='Offset', VALUE = StrCompress(String(offset, FORMAT = '(F8.4)'),/REMOVE_ALL), CANCEL=cancelled)
                                      IF cancelled THEN RETURN
                                      valid = StRegex(offsetTemp,'[^0-9.-]')>StRegex(offsetTemp,'\..*\.')>StRegex(offsetTemp,'-.*-')                             
                                    
                                    ENDWHILE
                                    
                                    offset = Float(offsetTemp)

                                    self.oProfiles->MultOffset, index, OFFSET = offset
                                    
                                   END
                  'Fit'   :        BEGIN
                                    selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
                                    index = IntArr(N_Elements(selProfile))
                                    FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                       Widget_Control, selProfile[i], GET_UVALUE = temp
                                       index[i] = temp
                                    ENDFOR
                                    
                                    peak  = self.oProfiles->FitPeak(index) 
                                   END
                  'Width' :        BEGIN
                                    selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
                                    index = IntArr(N_Elements(selProfile))
                                    FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                       Widget_Control, selProfile[i], GET_UVALUE = temp
                                       index[i] = temp
                                    ENDFOR

                                    Widget_Control, event.id, GET_UVALUE = width
                                    self.oProfiles->LineWidth, index, width
                                   
                                   END
                  'Opacity' :      BEGIN
                                    selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
                                    index = IntArr(N_Elements(selProfile))
                                    FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                       Widget_Control, selProfile[i], GET_UVALUE = temp
                                       index[i] = temp
                                    ENDFOR

                                    Widget_Control, event.id, GET_UVALUE = opacity
                                    self.oProfiles->LineOpacity, index, opacity
                                   END
                  'Change Colour': BEGIN
                                    selProfile = (Widget_Info( self.wProfileTree, /TREE_DRAG_SELECT ))[0]
                                    IF selProfile EQ -1 THEN BREAK
                                    allLeaves = Widget_Info(self.wProfileTree, /ALL_CHILDREN)
                                    allLeavesTemp = allLeaves[0]
                                    FOR i = 0, N_Elements(allLeaves) - 1 DO BEGIN
                                      childLeaves = Widget_Info(allLeaves[i],/ALL_CHILDREN)
                                      IF i NE 0 THEN allLeavesTemp = [allLeavesTemp,allLeaves[i]]
                                      IF childLeaves[0] NE 0 THEN allLeavesTemp = [allLeavesTemp,childLeaves] 
                                    ENDFOR
                                    allLeaves = allLeavesTemp                                    
                                    ;FOR i = 0, N_Elements(numLeaves) - 1 DO numLeaves = numLeaves + Widget_Info(allleaves[i], /N_CHILDREN)
                                    
                                    numLeaves = N_Elements(allLeaves)
                                    
                                    index = Where(allLeaves EQ selProfile)
                                                                      
                                    Widget_Control, selProfile, GET_UVALUE = profileRef
                                    
                                    self.oProfiles->GetProperty, COLOUR=profileRef                                    

                                    IF N_Elements(profileRef) EQ 1 THEN BEGIN
                                      
                                      customTables = ''
                                      IF Obj_Valid(self.resource) THEN BEGIN
                                        customTables = self.resource.GetResource('COLOURTABLEFILE')
                                      ENDIF
                                      
                                      as_editopalette, /MODAL, INDEX = profileRef, GROUPLEADER = event.top, CUSTOMPALETTE = self.plotPalette, CUSTOMTABLES = customTables, TABLEINDEX = 42;, /HIDECUSTOM
                                                              
                                      FOR i = 0, numLeaves - 1 DO BEGIN
                                        ;wTemp = Widget_Info(self.wProfileTree, FIND_BY_UNAME = 'Profile Leaf ' + String(i))
                                                                               
                                        profileBlank = StrSplit(Widget_Info(allLeaves[i], /UNAME), /EXTRACT)
                                        Widget_Control, allLeaves[i], GET_UVALUE=profileNum
                                        colourIndex = profileNum
                                        self.oProfiles->GetProperty, COLOUR=colourIndex
                                        self.plotPalette->GetProperty, N_COLORS = numColours
                                        rgb = self.plotPalette->GetRGB(colourIndex < (numColours - 1))
                                        colourPlane = replicate(1,16)#replicate(1,16)
                                        buttonColour = Byte([[[rgb[0]*colourPlane]],[[rgb[1]*colourPlane]],[[rgb[2]*colourPlane]]])
                                        
                                        IF profileBlank[0] EQ 'Profile' THEN BEGIN  
                                          IF Widget_Info(allLeaves[i],/VALID) THEN Widget_Control, allLeaves[i], SET_TREE_BITMAP = buttonColour  
                                        ENDIF
                                        IF profileBlank[0] EQ 'Blank' THEN BEGIN
                                          folderButton = [[[self.folder]],[[self.folder]],[[self.folder]]]+ buttonColour*[[[self.folderOpacity]],[[self.folderOpacity]],[[self.folderOpacity]]] 
                                          Widget_Control, event.id, SET_TREE_BITMAP = folderButton
                                          IF Widget_Info(allLeaves[i],/VALID) THEN Widget_Control, allLeaves[i], SET_TREE_BITMAP = folderButton                                    
                                        ENDIF
                                        
                                        
                                        ;dummyArr = replicate(1b,50)#replicate(1b,30)
                                        ;buttonColour = Byte([[[rgb[0]*dummyArr]],[[rgb[1]*dummyArr]],[[rgb[2]*dummyArr]]])
                                        ;Widget_Control, (*self.wPlotColour)[i], SET_VALUE = buttonColour
                                      ENDFOR
                                    ENDIF
                                   END
                  'XLog'         : BEGIN
                                    self.oProfiles->LinLog, 'X'                        
                                   END
                  'YLog'         : BEGIN
                                    self.oProfiles->LinLog, 'Y'                         
                                   END
                  'Power Field'  : BEGIN 
                                    power = -event.object.Get_Value()
                                    self.oProfiles.SetPowerPlot, power
                                   END
                  'Power Slide'  : BEGIN
                                    self.oProfiles.SetPowerPlot, SLIDE = fix(event.value)
                                   END
                  'Delete All'   : BEGIN
                                     childLeaves = Widget_Info(self.wProfileTree, /ALL_CHILDREN)
                                     IF childLeaves[0] EQ 0 THEN BREAK
                                     FOREACH leaf, childLeaves DO Widget_Control, leaf, /DESTROY
                                     self.oProfiles.DeleteProfile, /ALL
                                     self.numPlots = 0
                                   END
                  'Expand All'   : BEGIN
                                     childLeaves = Widget_Info(self.wProfileTree, /ALL_CHILDREN)
                                     IF childLeaves[0] EQ 0 THEN BREAK
                                     FOREACH leaf, childLeaves DO Widget_Control, leaf, /SET_TREE_EXPANDED
                                   END
                  'Collapse All' : BEGIN
                                     childLeaves = Widget_Info(self.wProfileTree, /ALL_CHILDREN)
                                     IF childLeaves[0] EQ 0 THEN BREAK
                                     FOREACH leaf, childLeaves DO Widget_Control, leaf, SET_TREE_EXPANDED = 0
                                   END
                  'Realtime'     : BEGIN
                                     self.oProfiles.SetProperty, REALTIME = event.select
                                     selProfile = WIDGET_INFO( self.wProfileTree, /ALL_CHILDREN )
                                     IF selProfile[0] EQ 0 THEN RETURN
                                     index = IntArr(N_Elements(selProfile))
                                     FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                       Widget_Control, selProfile[i], GET_UVALUE = temp
                                       index[i] = temp
                                     ENDFOR
                                     IF event.select THEN self.oProfiles->LineOpacity, index, 0.2 ELSE self.oProfiles->LineOpacity, index, 1
                                   END
                  'AutoPlot'     : BEGIN
                                     
                                   END
                  'IgnorePlot'   : BEGIN
                                     self.oProfiles.SetProperty, IGNORELIVE = event.select
                                   END
                  'AutoPorod'    : BEGIN
                                     selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
                                     index = IntArr(N_Elements(selProfile))
                                     FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                       Widget_Control, selProfile[i], GET_UVALUE = temp
                                       index[i] = temp
                                     ENDFOR
                                     data = self.oProfiles.GetProfiles(index[0])
                                     tempFileName = GetEnv('TEMP') + 'tempdata.dat'
                                     OpenW, tempProfileFile, tempFileName, /GET_LUN 
                                      PrintF, tempProfileFile, data.toarray(), FORMAT = '(' + StrCompress(3,/REMOVE) + 'A' + StrCompress(1+Max(StrLen(reform(data.toArray()))),/REMOVE) + ')'
                                     FREE_LUN, tempProfileFile
                                     SPAWN, 'Autoporod ' + tempFileName, APreturn
;                                     APreturn = StrSplit(APReturn, /EXTRACT)
;                                     APReturn.remove,0
;                                     APReturnString = ''
;                                     FOREACH element, APReturn DO APReturnString = APReturnString + String(element) 
;                                     XDisplayFile, TEXT = APReturnString, TITLE = 'Autoporod Info'
                                      XDisplayFile, TEXT = APReturn, TITLE = 'Autoporod Info'
                                     
                                   END
                  'SUM'          : BEGIN
                                    selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
                                    filenames = list()
                                    FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                      Widget_Control, selProfile[i], GET_UVALUE = temp
                                      self.oProfiles.GetProperty, FNAME=temp
                                      filenames.add, temp
                                    ENDFOR
                                    self.notify, {SUM, filenames : filenames}
                                    
                                   END
                  'CONTOUR'      : BEGIN
                                     Widget_Control, event.id, GET_UVALUE = add
                                     selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
                                     filenames = list()
                                     indices = list()
                                     FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                       Widget_Control, selProfile[i], GET_UVALUE = temp
                                       indices.add, temp
                                       self.oProfiles.GetProperty, FNAME=temp
                                       filenames.add, temp
                                     ENDFOR
                                     self.notify, {CONTOUR, filenames : filenames, indices : indices, add : add}
                
                                   END
                  'Hide'         : BEGIN
                                     selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
                                     index = IntArr(N_Elements(selProfile))
                                     FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                       Widget_Control, selProfile[i], GET_UVALUE = temp
                                       index[i] = temp
                                     ENDFOR
                                     self.oProfiles->HidePlot, index
                                   END       
                  'Show'         : BEGIN
                                     selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
                                     index = IntArr(N_Elements(selProfile))
                                     FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                       Widget_Control, selProfile[i], GET_UVALUE = temp
                                       index[i] = temp
                                     ENDFOR
                                     self.oProfiles->ShowPlot, index
                                   END
                  'Show/Hide'    : BEGIN
                                     selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
                                     index = IntArr(N_Elements(selProfile))
                                     FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
                                       Widget_Control, selProfile[i], GET_UVALUE = temp
                                       index[i] = temp
                                     ENDFOR
                                     self.oProfiles->ShowHidePlot, index
                                   END
                  'Delete'       : BEGIN
                                    self.deletePlot
                                   END
;                  'Copy Blank'   : BEGIN
;                                    selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
;                                    index = IntArr(N_Elements(selProfile))
;                                     FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
;                                       Widget_Control, selProfile[i], GET_UVALUE = temp
;                                       index[i] = temp
;                                     ENDFOR
;                                     print, selProfile
;                                   END
                  ELSE :                     
             ENDCASE
            END
ENDCASE
IF Obj_Valid(self.oProfiles) THEN self->UpDatePlot, KEEPFIT = widgetName EQ 'Fit'
                  
END

PRO AS_PlotControl::MultiPlot, CONTOUR = contour, SURFACE = surf

  @as_scatterheader.macro

  IF Keyword_Set(surf) THEN plotType = 'Surface' ELSE plotType = 'Contour'
  IF KeyWord_Set(contour) THEN plotType = 'Contour'
  
  selProfile = WIDGET_INFO( self.wProfileTree, /TREE_SELECT )
  index = IntArr(N_Elements(selProfile))
  fnameArr = StrArr(N_Elements(selProfile))
  FOR i =0, N_Elements(selProfile) - 1 DO BEGIN
    Widget_Control, selProfile[i], GET_UVALUE = temp
    index[i] = temp
    fname = index[i]
    self.oProfiles.GetProperty, FNAME = fname
    fnameArr[i] = fname
  ENDFOR

  data = self.oProfiles.GetProfiles(index)
 
  xContour = !null 
  yContour = !null
  zContour = !null
  
  self.oProfiles.GetProperty, XRANGEZOOM = xRange
  
  FOREACH prof, data, index DO BEGIN
    inRange = Where(prof[0,*] GT xRange[0] AND prof[0,*] LT xRange[1])
    xContour = [xContour, Reform(prof[0,inRange])]
    zContour = [zContour, Reform(prof[1,inRange])]
    yContour = [yContour, replicate(index,N_Elements(inRange))]
  ENDFOREACH
 
 
  CASE plotType OF
  'Contour' : BEGIN
                contourPlot = AS__SaxsContourPlot(xContour, yContour, zContour, FILENAMES = fnameArr)
              END
  'Surface' : BEGIN
                wSurfaceBase = Widget_Base(/ROW, TITLE = 'scatterBrain Surface Plot', /TLB_SIZE_EVENTS, EVENT_PRO='AS_SurfaceControl_Event')
                WIDGET_CONTROL, wSurfaceBase, SET_UVALUE = self
                wAxisTypeBase = Widget_Base(wSurfaceBase, /COLUMN)
                wSurfaceDraw = Widget_Window(wSurfaceBase, UNAME = 'Surface Draw')
                WIDGET_CONTROL, wSurfaceBase, /REALIZE
                WIDGET_CONTROL, wSurfaceDraw, GET_VALUE=oSurfaceWindow
                oSurfaceWindow.Select
                Triangulate, xContour, yContour, tri
                zData = TriGrid(xContour, yContour, zContour, tri, XGRID = xVector, YGRID=yVector, NY = N_Elements(selProfile), NX = N_Elements(xContour)/N_Elements(selProfile))
                s = Surface((zData), xVector, yVector, RGB_TABLE = 39, VERT_COLORS=bytscl(zData),/CURRENT, XTITLE = 'q ($\AA^{-1}$)',YTITLE = 'Plot Number', ZTITLE = 'Intensity')
              END
  ENDCASE 

END

PRO AS_PlotControl::Callback, event

  @as_scatterheader.macro

  CASE event.type OF 
  
  'NewPlot' : self->AddPlot, event.label, event.index
  'DeletePlot' : BEGIN
                   self.SelectPlot, event.index
                   self.DeletePlot, leaveProfile = event.leaveProfile
                 END
  'PlotNameChange' : self->ReNamePlot, event.label, event.index
  'Resize'  : BEGIN
                Widget_Control, self.wPCBase, ysize = event.y
                Widget_Control, self.wProfileTree, ysize = 0 > (event.y - 165)
              END
  'QMarker' : self.notify, {QMARKER, q:event.q}
  ELSE :
  ENDCASE

END

PRO AS_PlotControl::Notify, event

  @as_scatterheader.macro

  FOREACH notify, self.notifyObj DO IF Obj_Valid(notify) THEN notify.notify, event

END

PRO AS_PlotControl::RePlot, index

  @as_scatterheader.macro

  FOR i = 0, N_Elements(index) -1 DO BEGIN
    fname = index[i]
    self.oProfiles->GetProperty, FNAME=fname, DETECTORNO = detectorNo
    self.notify, {PLOTREPLOT, filename : fname, detectorNo : detectorNo}
    self.oProfiles->ReplaceWithLast, index[i]
;    allLeaves = Widget_Info(self.wProfileTree, /ALL_CHILDREN)
;    Widget_Control, allLeaves(N_Elements(allLeaves)-1),/DESTROY
;    self.numPlots = self.numPlots - 1

  ENDFOR

END

PRO AS_PlotControl::ReNamePlot, plotLabel, plotIndex

  @as_scatterheader.macro

  topChildren = (children = Widget_Info(self.wProfileTree, /ALL_CHILDREN))
  
  FOREACH child, topChildren DO children = [children,Widget_Info(child, /ALL_CHILDREN)]
  FOREACH child, children[Where(children GT 0)] DO BEGIN
    Widget_Control, child, GET_UVALUE = temp
    IF temp EQ plotIndex THEN BEGIN
      Widget_Control, child, SET_VALUE = plotLabel
      RETURN
    ENDIF 
  ENDFOREACH 

END

PRO AS_PlotControl::SelectPlot, plotIndex

  @as_scatterheader.macro
  
  topChildren = (children = Widget_Info(self.wProfileTree, /ALL_CHILDREN))
  IF topChildren[0] EQ 0 THEN RETURN
  FOREACH child, topChildren DO children = [children,Widget_Info(child, /ALL_CHILDREN)]
  FOREACH child, children[Where(children GT 0)] DO BEGIN
    Widget_Control, child, GET_UVALUE = temp
    IF temp EQ plotIndex THEN BEGIN
      Widget_Control, self.wProfileTree, SET_TREE_SELECT = 0
      Widget_Control, child, SET_TREE_SELECT = 1
      RETURN
    ENDIF
  ENDFOREACH

END

PRO AS_PlotControl::DeletePlot, LEAVEPROFILE = leaveProfile

  @as_scatterheader.macro

   IF (Widget_Info(self.wProfileTree, /TREE_SELECT))[0] EQ -1 THEN RETURN
   nameArray = !Null
   FOREACH leafName, Widget_Info(Widget_Info(self.wProfileTree, /TREE_SELECT),/UNAME) DO nameArray = [nameArray, (StrSplit(leafName, /EXTRACT))[0]]
   blankPositions = Where(nameArray EQ 'Blank')
   IF blankPositions[0] NE -1 THEN BEGIN
   
     childLeaves = !Null
     FOREACH blankPos, blankPositions DO childLeaves = [childLeaves, Widget_Info((Widget_Info( self.wProfileTree, /TREE_SELECT))[blankPos],/ALL_CHILDREN)]
     Widget_Control, (Widget_Info( self.wProfileTree, /TREE_SELECT))[blankPos], SET_UNAME = 'Profile'
     IF childLeaves[0] NE 0 THEN FOR i = 0, N_Elements(childLeaves) - 1 DO Widget_Control, childLeaves[i], SET_TREE_SELECT = 1 
     self.DeletePlot 
   
   ENDIF ELSE BEGIN
   
     selProfile = Widget_Info( self.wProfileTree, /TREE_SELECT )
     allLeaves = Widget_Info(self.wProfileTree, /ALL_CHILDREN)
     allLeavesTemp = allLeaves[0]
     FOR i = 0, N_Elements(allLeaves) - 1 DO BEGIN
       childLeaves = Widget_Info(allLeaves[i],/ALL_CHILDREN)
       IF i NE 0 THEN allLeavesTemp = [allLeavesTemp,allLeaves[i]]
       IF childLeaves[0] NE 0 THEN allLeavesTemp = [allLeavesTemp,childLeaves] 
     ENDFOR
     allLeaves = allLeavesTemp
     profileIndex = (index = IntArr(N_Elements(selProfile)))
     FOR i = 0, N_Elements(selProfile) - 1 DO index[i] = Where(allLeaves EQ selProfile[i])
     
     FOR i = 0, N_Elements(profileIndex) - 1 DO BEGIN
       Widget_Control, selProfile[i], GET_UVALUE = temp
       profileIndex[i] = temp
     ENDFOR
     
     self.plotPalette->GetProperty, BLUE=blue, RED=red, GREEN=green
     self.plotPalette.GetProperty, N_COLORS = paletteSize
     tempArr = IntArr(paletteSize)
     FOREACH colour, profileIndex DO BEGIN
     
       self.plotPalette->GetProperty, N_COLORS = numColours
       self.oProfiles.GetProperty, colour = colour
       tempArr[colour < (numColours - 1)] = 1
      
     ENDFOREACH
     tempArr = Where(tempArr EQ 0, COMPLEMENT = moveColours)
     temp = N_Elements(index)
     red = red[[tempArr,moveColours]]
     blue = blue[[tempArr,moveColours]]
     green = green[[tempArr,moveColours]]
     
     self.plotPalette->SetProperty, BLUE=blue, RED=red, GREEN=green
     
     
     profileIndices = LonArr(N_Elements(allLeaves))
     FOR i = 0, N_Elements(allLeaves) - 1 DO BEGIN 
       Widget_Control, allLeaves[i], GET_UVALUE = temp
       profileIndices[i] = temp
     ENDFOR
     
     FOR i = 0, N_Elements(selProfile) - 1 DO BEGIN
       IF Widget_Info(selProfile[i],/VALID) THEN Widget_Control, selProfile[i], /DESTROY
       IF N_Elements(Where(profileIndex[i] EQ profileIndices)) EQ 1 THEN BEGIN
         IF ~KeyWord_Set(leaveProfile) THEN self.oProfiles->DeleteProfile, profileIndex[i], /NONOTIFY   
       ENDIF 
     ENDFOR
    
     self.numPlots = self.numPlots - N_Elements(index)
     
     
     allLeaves = Widget_Info(self.wProfileTree, /ALL_CHILDREN)
     IF allLeaves[0] NE 0 THEN BEGIN
     
     allLeavesTemp = allLeaves[0]
     FOR i = 0, N_Elements(allLeaves) - 1 DO BEGIN
       childLeaves = Widget_Info(allLeaves[i],/ALL_CHILDREN)
       IF i NE 0 THEN allLeavesTemp = [allLeavesTemp,allLeaves[i]]
       IF childLeaves[0] NE 0 THEN allLeavesTemp = [allLeavesTemp,childLeaves] 
     ENDFOR
     allLeaves = allLeavesTemp
     ENDIF
     self.oProfiles->ReOrgColours
     
     IF allLeaves[0] NE 0 THEN BEGIN
      FOR i = 0, N_Elements(allLeaves) - 1 DO BEGIN
         profileBlank = StrSplit(Widget_Info(allLeaves[i], /UNAME), /EXTRACT)
         Widget_Control, allLeaves[i], GET_UVALUE=profileNum
         colourIndex = profileNum
         self.oProfiles->GetProperty, COLOUR=colourIndex
         self.plotPalette.GetProperty, N_COLORS = numColours
         rgb = self.plotPalette->GetRGB(colourIndex < (numColours - 1))
         colourPlane = replicate(1,16)#replicate(1,16)
         buttonColour = Byte([[[rgb[0]*colourPlane]],[[rgb[1]*colourPlane]],[[rgb[2]*colourPlane]]])
         IF profileBlank[0] EQ 'Profile' THEN BEGIN  
            IF Widget_Info(allLeaves[i],/VALID) THEN Widget_Control, allLeaves[i], SET_TREE_BITMAP = buttonColour  
         ENDIF
         IF profileBlank[0] EQ 'Blank' THEN BEGIN
            folderButton = [[[self.folder]],[[self.folder]],[[self.folder]]]+ buttonColour*[[[self.folderOpacity]],[[self.folderOpacity]],[[self.folderOpacity]]] 
            IF Widget_Info(allLeaves[i],/VALID) THEN Widget_Control, allLeaves[i], SET_TREE_BITMAP = folderButton                                    
         ENDIF
         
         
       ENDFOR
     ENDIF
   ENDELSE
  
   allLeaves = Widget_Info(self.wProfileTree, /ALL_CHILDREN)
   tempLeaves = !Null
   FOREACH leaf, allLeaves DO BEGIN
     IF leaf EQ 0 THEN BREAK
     children = Widget_Info(leaf, /ALL_CHILDREN)
     IF N_Elements(children) EQ 1 AND children[0] EQ 0 THEN BEGIN
     
       nameExtract = StrSplit(Widget_Info(leaf, /UNAME),/EXTRACT)
       IF nameExtract[0] EQ 'Blank' THEN BEGIN
         Widget_Control, leaf, SET_UNAME ='Profile ' + StrJoin(nameExtract[1:*], ' ', /SINGLE)
         Widget_Control, leaf, GET_UVALUE=profileNum
         colourIndex = profileNum
         self.oProfiles->GetProperty, COLOUR=colourIndex
         rgb = self.plotPalette->GetRGB(colourIndex)
         colourPlane = replicate(1,16)#replicate(1,16)
         buttonColour = Byte([[[rgb[0]*colourPlane]],[[rgb[1]*colourPlane]],[[rgb[2]*colourPlane]]])
         IF Widget_Info(leaf,/VALID) THEN Widget_Control, leaf, SET_TREE_BITMAP = buttonColour  
         
       ENDIF
     
     ENDIF
    
     IF children[0] NE 0 THEN tempLeaves = [tempLeaves, Widget_Info(leaf, /ALL_CHILDREN)]
   ENDFOREACH
   
   allLeaves = [allLeaves,tempLeaves]
   
   FOREACH leaf, allLeaves DO BEGIN
     IF leaf EQ 0 THEN CONTINUE
     Widget_Control, leaf, GET_UVALUE =val
     ;print, val
   ENDFOREACH

END

PRO AS_PlotControl::AddPlot, plotLabel, plotIndex, UPDATEPLOT = updatePlot 

  @as_scatterheader.macro

  self.numPlots = self.numPlots + 1
  
;  print, self.numPlots
  ;wPlotsBase = Widget_Base(self.wPCBase,/ROW)
  ;dummyArr = replicate(1b,50)#replicate(1b,30)
    
  self.plotPalette.GetProperty, N_COLORS = numColours
    
  rgb = self.plotPalette->GetRGB((self.numPlots-1) < (numColours - 1))
  colourPlane = replicate(1,16)#replicate(1,16)
  ;buttonColour = Byte([[[rgb[0]*dummyArr]],[[rgb[1]*dummyArr]],[[rgb[2]*dummyArr]]])
  buttonColour = Byte([[[rgb[0]*colourPlane]],[[rgb[1]*colourPlane]],[[rgb[2]*colourPlane]]])
  ;wPlotColour = Widget_Button(wPlotsBase, VALUE = buttonColour, UVALUE = self.numPlots-1, UNAME = 'Change Colour')
  wProfileLeaf = Widget_Tree(self.wProfileTree, VALUE = plotLabel, UNAME = 'Profile Leaf ' + String(self.numplots-1), UVALUE = plotIndex, BITMAP = buttonColour, /FOLDER)
   
;  wPad = Widget_Label(wPlotsBAse, VALUE = '  ')
;  wPlotDisplay = Widget_Button(wPlotsBase, VALUE = 'Hide', UVALUE = self.numPlots-1, UNAME= 'Show/Hide')
;  wPad = Widget_Label(wPlotsBAse, VALUE = '  ')
;  wPlotLabel = Widget_Label(wPlotsBase, VALUE=plotLabel)
  
;  IF Ptr_Valid(self.wPlotLabel) THEN BEGIN
;    temp = *self.wPlotLabel
;    temp = [temp,wPlotLabel]
;    *self.wPlotLabel = temp
;  ENDIF ELSE self.wPlotLabel = Ptr_New(wPlotLabel)
  
  
  IF Obj_Valid(self.oProfiles) THEN BEGIN
    realWidget = Widget_Info(self.wPCBase, FIND_BY_UNAME='Realtime')
    IF Widget_Info(realWidget, /VALID) THEN IF Widget_Info(Widget_Info(self.wPCBase, FIND_BY_UNAME='Realtime'),/BUTTON_SET) THEN self.oProfiles->LineOpacity, plotIndex, 0.2
    self->UpDatePlot
  ENDIF

END

PRO AS_PlotControl::Cleanup
  
  @as_scatterheader.macro

  Obj_Destroy, self.plotPalette

END

FUNCTION AS_PlotControl::Init, GROUPLEADER = groupLeader, FRAMEOBJ = oFrame, PLOTPALETTE=plotPalette, PROFILES_OBJ = oProfiles, RESOURCE=resource, DOCK=dock, NOTIFYOBJ = notifyObj,POLLEPICS=pollEpics

self.folder = [ $
    [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],$
    [  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, 255, 255, 255, 255, 255],$
    [  0,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, 255, 255, 255, 255],$
    [  0,  52,  52,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255, 255, 255],$
    [  0,  52,  52,  52,   0,   0,   0,   0,   0,   0,   0,  52,  52, 255, 255, 255],$
    [  0,  52,   0,  52,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255, 255],$
    [  0,  52,   0,  52,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255, 255],$
    [  0,  52,   0,   0,  52,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255],$
    [  0,  52,   0,   0,  52,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255],$
    [  0,  52,   0,   0,   0,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, 255],$
    [  0,  52,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255, 255],$
    [  0,  52,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255, 255],$
    [255,  52,   0,   0,   0,  52,  52,  52,  52,  52,  52,  52,  52, 255, 255, 255],$
    [255, 255,  52,  52,  52, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],$
    [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],$
    [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255]]

self.folderOpacity = [$
    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]]

IF Keyword_Set(notifyObj) THEN $
    IF TypeName(notifyObj[0]) EQ 'NOTIFY' $
      THEN self.notifyObj = List(notifyObj,/EXTRACT)
  
  self.lastSelected = List()

  IF Obj_Valid(resource) THEN self.resource = resource
  IF KeyWord_Set(groupLeader) THEN self.wGroupleader = groupLeader
  IF KeyWord_Set(oFrame) THEN self.oFrame = oFrame
  IF KeyWord_Set(oProfiles) THEN BEGIN
    self.oProfiles = oProfiles
    self.oProfiles->GetProperty, BASE = profBase
    geom = Widget_Info(profBase, /GEOMETRY)
    IF KeyWord_Set(dock) THEN BEGIN
      self.wPCBase = Widget_Base(profBase, UNAME = 'Plot Control Base', YSIZE = geom.ysize, /COLUMN) 
    ENDIF ELSE BEGIN
      self.wPCBase = Widget_Base(/COLUMN, GROUP_LEADER = profBase, XOFFSET = geom.xoffset + geom.xsize + 2*geom.margin, /FLOATING, /TLB_SIZE_EVENTS, UNAME = 'Plot Control Base')
    ENDELSE      
  ENDIF ELSE BEGIN
    IF self.groupLeader GT 0 THEN BEGIN 
      geom = Widget_Info(groupLeader, /GEOMETRY)
      self.wPCBase = Widget_Base(/COLUMN, GROUP_LEADER = groupLeader, XOFFSET = geom.xoffset + geom.xsize + 2*geom.margin, /FLOATING, UNAME = 'Plot Control Base')
    ENDIF ELSE self.wPCBase = Widget_Base(/COLUMN, UNAME = 'Plot Control Base')
  ENDELSE
 
  wLogBase = Widget_Base(self.wPCBase, /ROW)
  logLabel = Widget_Label(wLogBase, VALUE = 'Lin <> Log  ')
  self.wXLog = Widget_Button(wLogBase,VALUE='X Axis', UNAME = 'XLog')
  self.wYLog = Widget_Button(wLogBase,VALUE='Y Axis', UNAME = 'YLog')
  powerField = FSC_Field(wLogBase, Title = 'Power Law: -', VALUE = 0., XSIZE = 3, DECIMAL=2, OBJECT = powerFieldObj, EVENT_PRO = 'AS_PlotControl_event', NAME = 'Power Field')
  powerSlide = Widget_Slider(wLogBase, /VERTICAL, VALUE = 50, MINIMUM=0, MAXIMUM=100, YSIZE = 1, UNAME = 'Power Slide')
  Widget_Control, powerField, SET_UVALUE = powerFieldObj
  
  allButtonBase = Widget_Base(self.wPCBase, /ROW)
  deleteAllBut = Widget_Button(allButtonBase, VALUE = 'Clear All', UNAME = 'Delete All')
  ExpandAllBut = Widget_Button(allButtonBase, VALUE = 'Expand All', UNAME = 'Expand All')
  CollapseAllBut = Widget_Button(allButtonBase, VALUE = 'Collapse All', UNAME = 'Collapse All')
  plotModeBase = Widget_Base(self.wPCBase, /EXCLUSIVE)
  ;IF KeyWord_Set(pollEpics) THEN BEGIN  ; Put this if statement back in if making overwrite mode the default.
    realTimeButton = Widget_Button(plotModeBase, VALUE='Live Overwrite Mode', UNAME = 'Realtime')
    autoPlotButton = Widget_Button(plotModeBase, VALUE = 'Live Add Plot Mode', UNAME = 'AutoPlot') 
    ignoreLiveButton = Widget_Button(plotModeBase, VALUE = 'Ignore Live Data Mode', UNAME = 'IgnorePlot')
    Widget_Control, ignoreLiveButton, /SET_BUTTON
    ;self.oProfiles.SetProperty, REALTIME = 1
  ;ENDIF ELSE self.oProfiles.SetProperty, REALTIME = 0
  
  
  wPlotLabel = Ptr_New(/ALLOCATE_HEAP)
  
  treeBase = Widget_Base(self.wPCBase, /COLUMN)
  IF KeyWord_Set(dock) THEN self.wProfileTree = Widget_Tree(treeBase, /MULTIPLE, /DRAGGABLE, /DROP_EVENTS, /CONTEXT, XSIZE = 280, YSIZE = geom.ysize - 165) $
                       ELSE self.wProfileTree = Widget_Tree(treeBase, /MULTIPLE, /DRAGGABLE, /DROP_EVENTS, /CONTEXT, XSIZE = 280)
  
  self.wContextBase = WIDGET_BASE(treeBase, /CONTEXT_MENU, UNAME = 'TreeContextBase' )
  wDeleteButton = Widget_Button(self.wContextBase, VALUE = 'Delete', UNAME = 'Delete')
  wReplotButton = Widget_Button(self.wContextBase, VALUE = 'Apply Parameters', UNAME = 'Apply Parameters')
  wMultButton = Widget_Button(self.wContextBase, VALUE = 'Mult', UNAME = 'Mult')
  wOffsetButton = Widget_Button(self.wContextBase, VALUE = 'Offset', UNAME = 'Offset')
  wFitButton = Widget_Button(self.wContextBase, VALUE = 'Fit Peak', UNAME = 'Fit')
;  wContourButton = Widget_Button(self.wContextBase, VALUE = 'Contour Plot', UNAME = 'Contour Plot')
;  wSurfaceButton = Widget_Button(self.wContextBase, VALUE = 'Surface Plot', UNAME = 'Surface Plot')
  wHideButton = Widget_Button(self.wContextBase, VALUE = 'Hide', UNAME = 'Hide')
  wShowButton = Widget_Button(self.wContextBase, VALUE = 'Show', UNAME = 'Show')
  wShowHideButton = Widget_Button(self.wContextBase, VALUE = 'Show <> Hide', UNAME = 'Show/Hide')
  wColourButton = Widget_Button(self.wContextBase, VALUE = 'Change Colour', UNAME = 'Change Colour')
  wWidthButton = Widget_Button(self.wContextBase, VALUE = 'Line Width', UNAME = 'Change Width',/MENU)
  wWidth1Button = Widget_Button(wWidthButton, VALUE = '1', UNAME = 'Width', UVALUE = 1) 
  wWidth2Button = Widget_Button(wWidthButton, VALUE = '2', UNAME = 'Width', UVALUE = 2)
  wWidth3Button = Widget_Button(wWidthButton, VALUE = '3', UNAME = 'Width', UVALUE = 3)
  wWidth4Button = Widget_Button(wWidthButton, VALUE = '4', UNAME = 'Width', UVALUE = 4)
  wWidth5Button = Widget_Button(wWidthButton, VALUE = '5', UNAME = 'Width', UVALUE = 5)
  wWidth6Button = Widget_Button(wWidthButton, VALUE = '6', UNAME = 'Width', UVALUE = 6)
  wWidth7Button = Widget_Button(wWidthButton, VALUE = '7', UNAME = 'Width', UVALUE = 7)
  wWidth8Button = Widget_Button(wWidthButton, VALUE = '8', UNAME = 'Width', UVALUE = 8)
  wWidth9Button = Widget_Button(wWidthButton, VALUE = '9', UNAME = 'Width', UVALUE = 9)
  wWidth10Button = Widget_Button(wWidthButton, VALUE = '10', UNAME = 'Width', UVALUE = 10)
  wOpacityButton = Widget_Button(self.wContextBase, VALUE = 'Line Opacity', UNAME = 'Change Opacity',/MENU)
  wOpacity1Button = Widget_Button(wOpacityButton, VALUE = '10%', UNAME = 'Opacity', UVALUE = 0.1) 
  wOpacity2Button = Widget_Button(wOpacityButton, VALUE = '20%', UNAME = 'Opacity', UVALUE = 0.2)
  wOpacity3Button = Widget_Button(wOpacityButton, VALUE = '30%', UNAME = 'Opacity', UVALUE = 0.3)
  wOpacity4Button = Widget_Button(wOpacityButton, VALUE = '40%', UNAME = 'Opacity', UVALUE = 0.4)
  wOpacity5Button = Widget_Button(wOpacityButton, VALUE = '50%', UNAME = 'Opacity', UVALUE = 0.5)
  wOpacity6Button = Widget_Button(wOpacityButton, VALUE = '60%', UNAME = 'Opacity', UVALUE = 0.6)
  wOpacity7Button = Widget_Button(wOpacityButton, VALUE = '70%', UNAME = 'Opacity', UVALUE = 0.7)
  wOpacity8Button = Widget_Button(wOpacityButton, VALUE = '80%', UNAME = 'Opacity', UVALUE = 0.8)
  wOpacity9Button = Widget_Button(wOpacityButton, VALUE = '90%', UNAME = 'Opacity', UVALUE = 0.9)
  wOpacity10Button = Widget_Button(wOpacityButton, VALUE = '100%', UNAME = 'Opacity', UVALUE = 1.0)
  wProteinButton = Widget_Button(self.wContextBase, VALUE = 'Protein Tools', /MENU)
  wAutoPorodButton = Widget_Button(wProteinButton, VALUE = 'Autoporod', UNAME = 'AutoPorod')
  w2DTools = Widget_Button(self.wContextBase, VALUE = '2D Tools',/MENU)
  wSum = Widget_Button(w2dTools, VALUE = 'Sum', UNAME = 'SUM')
  wContour = Widget_Button(w2dTools, VALUE = 'Contour', UNAME = 'CONTOUR', UVALUE = 0)
  wContourAdd = Widget_Button(w2dTools, VALUE = 'Add to Contour', UNAME = 'CONTOUR', UVALUE = 1)
  ;wCopyBlankButton = Widget_Button(self.wContextBase, VALUE = 'Copy Blank', UNAME = 'Copy Blank')
      
  self.wInfoBox = Widget_Text(treeBase, VALUE = '', YSIZE = 5, EDITABLE=0)
    
  Widget_Control, self.wPCBase, SET_UVALUE = self
 ; Widget_Control, self.wPCBase, /REALIZE


  IF KeyWord_Set(plotPalette) THEN  self.plotPalette = plotPalette ELSE BEGIN
    self.plotPalette = Obj_New('IDLgrPalette')
    self.plotPalette->LoadCT, 34
  ENDELSE

  XMANAGER, 'AS_PlotControl', self.wPCBase, /NO_BLOCK

  RETURN, 1

END

PRO AS_PlotControl__Define 

folder = [ $
    [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],$
    [  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0, 255, 255, 255, 255, 255],$
    [  0,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, 255, 255, 255, 255],$
    [  0,  52,  52,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255, 255, 255],$
    [  0,  52,  52,  52,   0,   0,   0,   0,   0,   0,   0,  52,  52, 255, 255, 255],$
    [  0,  52,   0,  52,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255, 255],$
    [  0,  52,   0,  52,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255, 255],$
    [  0,  52,   0,   0,  52,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255],$
    [  0,  52,   0,   0,  52,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255],$
    [  0,  52,   0,   0,   0,  52,  52,  52,  52,  52,  52,  52,  52,  52,  52, 255],$
    [  0,  52,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255, 255],$
    [  0,  52,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,  52, 255, 255, 255],$
    [255,  52,   0,   0,   0,  52,  52,  52,  52,  52,  52,  52,  52, 255, 255, 255],$
    [255, 255,  52,  52,  52, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],$
    [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255],$
    [255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255]]

folderOpacity = [$
    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 1.0, 0.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],$
    [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0]]

void = {AS_PlotControl, $
        INHERITS IDL_OBJECT, $
        wPlotLabel     : Ptr_New(), $
        notifyObj      : List(), $
        plotPalette    : Obj_New(), $
        oProfiles      : Obj_New(), $
        oFrame         : Obj_New(), $
        resource       : Obj_New(), $
        oSurface       : Obj_New(), $
        lastSelected   : List(),    $
        wGroupleader   : 0L, $
        wXLog          : 0L, $
        wYLog          : 0L, $        
        wPCBase        : 0L, $
        wProfileTree   : 0L, $
        wContextBase   : 0L, $
        wInfoBox       : 0L, $
        numPlots       : 0,  $
        numBlanks      : 0,   $
        folder         : folder, $
        folderOpacity  : folderOpacity $
        }

END
