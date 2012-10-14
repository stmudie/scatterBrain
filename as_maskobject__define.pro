FUNCTION as_maskObject::init, x, y
   
  @as_scatterheader.macro 
   
  self.numPoints = 50 
   
  ;self.oModel = IDLgrModel()
  IF N_Elements(x)*N_Elements(y) NE 0 THEN BEGIN
    self.oPolygon = IDLgrPolygon([x,x[0]],[y,y[0]])
    self.oPolyline = IDLgrPolyline([x,x[0]],[y,y[0]])
    self.data = Ptr_New(Transpose([[x],[y]]))
  ENDIF ELSE BEGIN
    self.oPolygon = IDLgrPolygon()
    self.oPolyline = IDLgrPolyline()
    self.data = Ptr_New(/ALLOCATE_HEAP)
  ENDELSE
  self.tessellator = IDLgrTessellator()
  self.maskLabels =  IDL_Container()
  self.maskFont = Obj_New('IDLgrFont', 'Helvetica', SIZE = 3)
  self.oPolygon.SetProperty, /DEPTH_TEST_DISABLE
  self.oPolyline.SetProperty, /DEPTH_TEST_DISABLE
  self.add, self.oPolygon
  self.add, self.oPolyline
  
  result = self->IDLgrModel::Init(_extra=extra)
  
  self->RegisterProperty, 'name', 4, NAME = 'Name'
  self->RegisterProperty, 'colour', 5, NAME = 'Colour'
  self->RegisterProperty, 'lineOpacity', 3, NAME = 'Line Opacity', VALID_RANGE = [0D,1D,0.1D]
  self->RegisterProperty, 'fillOpacity', 3, NAME = 'Fill Opacity', VALID_RANGE = [0D,1D,0.1D]
  self->RegisterProperty, 'lineThickness', 8, NAME = 'Line Thickness'
  self->RegisterProperty, 'lineStyle', 6, NAME = 'Line Style'
  self->RegisterProperty, 'maskShape', 9, ENUMLIST = ['Polygon','Circle'], NAME = 'Mask Shape'
  self->RegisterProperty, 'maskType', 9, ENUMLIST = ['Not Active','Excluding','Including'], NAME = 'Mask Type'
  self->RegisterProperty, 'centreX', 3, NAME = 'Centre X'
  self->RegisterProperty, 'centreY', 3, NAME = 'Centre Y'
  self->RegisterProperty, 'radiusMin', 3, NAME = 'Radius Min'
  self->RegisterProperty, 'radiusMax', 3, NAME = 'Radius Max'
  self->RegisterProperty, 'angleMin', 3, NAME = 'Angle Min', VALID_RANGE = [-360D,360D]
  self->RegisterProperty, 'angleMax', 3, NAME = 'Angle Max', VALID_RANGE = [-360D,360D]
  self->RegisterProperty, 'beamRelative', 1, NAME = 'Beam Relative', /ADVANCED_ONLY
  self->RegisterProperty, 'lock', 1, NAME = 'Lock Mask'
    
  RETURN, Result

END

PRO as_maskObject::GetProperty, $
  COLOUR = COLOUR, $
  LINEOPACITY = lineOpacity, $
  FILLOPACITY = fillOpacity, $
  LINETHICKNESS = lineThickness, $
  LINESTYLE = lineStyle, $
  MASKSHAPE = maskShape, $
  MASKTYPE = maskType, $
  CENTREX = centreX, $
  CENTREY = centreY, $
  RADIUSMIN = radiusMin, $
  RADIUSMAX = radiusMax, $
  ANGLEMIN = angleMin, $
  ANGLEMAX = angleMax, $
  BEAMRELATIVE = beamRelative, $
  LOCK = lock, $
  DATA = data, $
  RELATIVE = relative, $
  SELECTEDVERTEX = selectedVertex, $
   _Ref_extra = extra
  
  @as_scatterheader.macro
  
  IF Arg_Present(colour) THEN self.oPolygon.GetProperty, COLOR=COLOUR
  IF Arg_Present(lineOpacity) THEN self.oPolyline.GetProperty, ALPHA_CHANNEL = lineOpacity
  IF Arg_Present(fillOpacity) THEN self.oPolygon.GetProperty, ALPHA_CHANNEL = fillOpacity
  IF Arg_Present(lineThickness) THEN self.oPolyline.GetProperty, THICK = lineThickness
  IF Arg_Present(lineStyle) THEN self.oPolyline.GetProperty, LINESTYLE = lineStyle
  IF Arg_Present(maskShape) THEN maskShape = self.maskShape
  IF Arg_Present(maskType) THEN maskType = self.maskType
  IF Arg_Present(centreX) THEN centreX = self.centreX
  IF Arg_Present(centreY) THEN centreY = self.centreY
  IF Arg_Present(radiusMin) THEN radiusMin = self.radiusMin
  IF Arg_Present(radiusMax) THEN radiusMax = self.radiusMax
  IF Arg_Present(angleMin) THEN angleMin = self.angleMin
  IF Arg_Present(angleMax) THEN angleMax = self.angleMax
  IF Arg_Present(beamRelative) THEN beamRelative = self.beamRelative
  IF Arg_Present(lock) THEN lock = self.lock
  IF Arg_Present(data) THEN IF KeyWord_Set(relative) AND self.beamRelative THEN data = *self.data - self.beamPosition#Replicate(1,self.numVertices) ELSE data = *self.data
  IF Arg_Present(selectedVertex) THEN selectedVertex = self.selectedVertex
    
  self->IDLgrModel::GetProperty, _EXTRA = extra
  
END

FUNCTION as_maskObject::GetSaveParams

  @as_scatterheader.macro
  
  IF N_Elements(*self.data) EQ 0 THEN RETURN, -1
  
  self.GetProperty, COLOUR = colour
  
  CASE self.maskShape OF
    0 : RETURN, {name : self.NAME, maskshape : 'Polygon', masktype : self.maskType, data : *self.data, beamrelative : self.beamRelative, colour : colour, lock : self.lock}
    1 : RETURN, {name : self.NAME, maskshape : 'Circle',  masktype : self.maskType, data : [self.centreX, self.centreY, self.radiusMax, self.radiusMin, self.angleMax, self.angleMin], beamrelative : self.beamRelative, colour : colour, lock : self.lock}
    ELSE : RETURN, -1
  ENDCASE
  
END

PRO as_maskObject::SetProperty, $
  COLOUR = COLOUR, $
  LINEOPACITY = lineOpacity, $
  FILLOPACITY = fillOpacity, $
  FILLHIDE = fillHide, $
  LINETHICKNESS = lineThickness, $
  LINESTYLE = lineStyle, $
  MASKSHAPE = maskShape, $
  MASKTYPE = maskType, $
  CENTREX = centreX, $
  CENTREY = centreY, $
  RADIUSMIN = radiusMin, $
  RADIUSMAX = radiusMax, $
  ANGLEMIN = angleMin, $
  ANGLEMAX = angleMax, $
  BEAMRELATIVE = beamRelative, $
  BEAMPOSITION = beamPosition, $
  RELATIVE = relative, $
  LOCK = Lock, $
  DATA = data,$
  _Ref_extra = extra
  
  @as_scatterheader.macro

  IF self.lock THEN RETURN
  IF N_Elements(colour)        THEN BEGIN
    self.oPolygon.SetProperty, COLOR=COLOUR
    self.oPolyline.SetProperty, COLOR=COLOUR
    self.selectVertex, self.selectedVertex
  ENDIF
  IF N_Elements(lineOpacity)   THEN BEGIN
    self.oPolyline.SetProperty, ALPHA_CHANNEL = Fix(lineOpacity*100)/100.
    FOREACH maskLabel, self.maskLabels.Get(/ALL) DO IF Obj_Valid(maskLabel) THEN maskLabel.SetProperty, ALPHA_CHANNEL = Fix(lineOpacity*100)/100. 
  ENDIF
  IF N_Elements(fillOpacity)   THEN self.oPolygon.SetProperty, ALPHA_CHANNEL = Fix(fillOpacity*100)/100.
  IF N_Elements(fillHide)      THEN BEGIN
    self.oPolygon.SetProperty, HIDE = fillHide
    self.fillHide = fillHide
  ENDIF
  IF N_Elements(lineThickness) THEN self.oPolyline.SetProperty, THICK = lineThickness
  IF N_Elements(lineStyle)     THEN self.oPolyline.SetProperty, LINESTYLE = lineStyle
  IF N_Elements(maskShape)     THEN BEGIN
    IF Size(maskShape, /TYPE) EQ 7 THEN BEGIN
      CASE StrUpCase(maskShape) OF
          'POLYGON' : maskShape = 0
          'CIRCLE'  : maskShape = 1 
          ELSE      : maskShape = 0
      ENDCASE
    ENDIF
    IF maskShape GT 1 THEN maskShape = 0
    self.maskShape = maskShape
    IF maskShape EQ 1 THEN self.makeArc
  ENDIF
  IF N_Elements(maskType) THEN BEGIN
    IF maskType EQ 0 THEN self.SetProperty, LINEOPACITY = 0, FILLOPACITY = 0
    IF maskType GT 0 AND self.maskType EQ 0 THEN self.SetProperty, LINEOPACITY = 1, FILLOPACITY = 0.5
    self.maskType = maskType
  ENDIF
  IF N_Elements(centreX)       THEN self.centreX = centreX
  IF N_Elements(centreY)       THEN self.centreY = centreY
  IF N_Elements(radiusMin)     THEN self.radiusMin = radiusMin
  IF N_Elements(radiusMax)     THEN self.radiusMax = radiusMax
  IF N_Elements(angleMin)      THEN self.angleMin = angleMin
  IF N_Elements(angleMax)      THEN self.angleMax = angleMax
  IF N_Elements(centreX) + N_Elements(centreY) + N_Elements(radiusMin) + N_Elements(radiusMax) + N_Elements(angleMin) + N_Elements(angleMax) GT 0 THEN BEGIN
    IF self.maskShape EQ 1 THEN self.makeArc
  ENDIF
  IF N_Elements(beamRelative) THEN BEGIN
    IF beamRelative EQ 1 AND self.beamRelative EQ 0 THEN BEGIN
      self.centreX = self.centreX - self.beamPosition[0]
      self.centreY = self.centreY - self.beamPosition[1] 
    ENDIF
    IF beamRelative EQ 0 AND self.beamRelative EQ 1 THEN BEGIN
      self.centreX = self.centreX + self.beamPosition[0]
      self.centreY = self.centreY + self.beamPosition[1] 
    ENDIF
    self.beamRelative = beamRelative
  ENDIF
  IF N_Elements(beamPosition) EQ 2 THEN BEGIN
    IF self.beamRelative EQ 1 THEN *self.data = *self.data + (beamPosition-self.beamPosition)#Replicate(1,self.numVertices)
    self.beamPosition = beamPosition
    self.UpdatePolygon, *self.data
  ENDIF
  IF N_Elements(data) GT 0 THEN BEGIN
    self.UpdatePolygon, data + KeyWord_Set(relative) * self.beamPosition#Replicate(1,N_Elements(data[0,*]))
  ENDIF
  
  self->IDLgrModel::SetProperty, _EXTRA = extra

  IF N_Elements(lock)          THEN self.lock = lock
  
END

PRO as_maskObject::ShowLabels, show

  @as_scatterheader.macro

  show = show < 1
  count = self.maskLabels.count()
  IF count GT 0 THEN BEGIN
    labels =  (self.maskLabels.Get(/ALL))
    FOREACH l, labels DO l.SetProperty, ALPHA_CHANNEL = show
  ENDIF
      
END

FUNCTION as_maskObject::NumVertices
  
  @as_scatterheader.macro

  RETURN, self.numVertices

END

PRO as_maskObject::SelectVertex, vertex 

  @as_scatterheader.macro

  IF N_Elements(vertex) EQ 0 OR self.numVertices EQ 0 THEN RETURN
  
  IF vertex GE self.numVertices THEN vertex = self.numVertices - 1 
  self.selectedVertex = vertex
  
  self.oPolygon.GetProperty, COLOR = colour
  vertColours = IntArr(3,self.numVertices)
  
  FOR v=0, N_Elements(vertColours[0,*]) - 1 DO vertColours[*,v] = colour
  labels = self.maskLabels.Get(/ALL)
  FOREACH l, labels DO l.SetProperty, COLOR = [0,0,0]
  IF Size(vertex,/TYPE) GE 1 AND Size(vertex,/TYPE) LE 5 THEN BEGIN
    vertex = vertex < (self.numVertices - 1)
    IF vertex + 1 GE self.numVertices THEN vertColours[*,0] = [0,200,255] ELSE vertColours[*,vertex+1] = [0,200,255]
    labels[vertex].SetProperty, COLOR = [0,200,255]
  ENDIF
  self.oPolyline.SetProperty, VERT_COLORS = vertColours, LABEL_NOGAPS = Replicate(0,self.numVertices), /USE_LABEL_COLOR

END

PRO as_maskObject::DeleteVertex

  @as_scatterheader.macro

  IF self.lock THEN RETURN

  IF self.maskShape EQ 0 AND self.numVertices GT 0 AND self.numVertices GT self.selectedVertex THEN BEGIN
    CASE self.selectedVertex OF
        self.numVertices-1 : BEGIN
                               self.UpdatePolygon, [[(*self.data)[*,0:self.selectedVertex-1]]]
                               self.SelectVertex, self.selectedVertex-1
                             END
        0                  : self.UpdatePolygon, [(*self.data)[*,self.selectedVertex+1:*]]
        ELSE               : self.UpdatePolygon, [[(*self.data)[*,0:self.selectedVertex-1]],[(*self.data)[*,self.selectedVertex+1:*]]]
    ENDCASE
  ENDIF

END

PRO as_maskObject::MoveVertex, position, ADD=add, RELATIVE=relative

  @as_scatterheader.macro

  IF self.lock THEN RETURN

  position = position + KeyWord_Set(relative)*self.beamRelative*self.beamPosition

  IF self.maskShape EQ 0 THEN BEGIN
    data = *self.data
    IF data NE !NULL THEN IF N_Elements(data[0,*]) GE self.numPoints THEN add = 0 
    IF KeyWord_Set(add) THEN BEGIN
      IF data EQ !NULL THEN BEGIN 
        data = [[position]]
        self.selectedVertex = -1 
      ENDIF ELSE BEGIN
        IF self.selectedVertex EQ self.numVertices - 1 THEN data = [[data[*,0:self.selectedVertex]],[[position]]] $
                                                       ELSE data = [[data[*,0:self.selectedVertex]],[[position]],[data[*,self.selectedVertex+1:*]]]
      ENDELSE
      self.UpdatePolygon, data
      self.selectVertex, self.selectedVertex + 1 
    ENDIF ELSE BEGIN
      data[*,self.selectedVertex] = position
      self.UpdatePolygon, data
    ENDELSE
  ENDIF

END

PRO as_maskObject::UpdatePolygon, data

  @as_scatterheader.macro

  IF self.lock THEN RETURN
  
  IF N_Elements(data) NE 0 THEN BEGIN
    *self.data = data
    numVertices = N_Elements(data[0,*])
    IF numVertices LT self.numVertices THEN BEGIN
      self.maskLabels.remove, (self.maskLabels.Get(/ALL))[Indgen(self.numVertices-numVertices) + numVertices]
      self.oPolyline.SetProperty, LABEL_OBJECTS = (self.maskLabels.Get(/ALL))
    ENDIF 
    IF numVertices GT self.numVertices THEN BEGIN
      FOR i = self.numVertices, numVertices-1 DO BEGIN
        charMult = 0.25
        text = IDLgrText(StrCompress(String(i),/REMOVE_ALL),FONT = self.maskFont, CHAR_DIMENSIONS = [50*charMult,50*charMult],ALIGNMENT = 0)
        self.maskLabels.add, text
        self.oPolyline.SetProperty, LABEL_OBJECTS = (self.maskLabels.Get(/ALL)), /USE_LABEL_COLOR
      ENDFOR
    ENDIF
    self.numVertices = numVertices
    IF self.numVertices - 1 GE 2 THEN BEGIN
      polyLines = IntArr(3*(self.numVertices))
      FOR i = 0, self.numVertices - 2 DO polyLines[i*3:i*3+2] = [2,i,i+1]
      polyLines[-3:-1] = [2,self.numVertices-1,0] ; Definition for polylines keyword to IDLgrPolyline is : [m1, i0, i1, ...,im1-1,m2,i0, i1, ..., im2-1 ...] where each mj is an integer specifying the number of vertices that define the polyline (the vertex count), and each associated set of i0...im-1 are indices into the array of vertices   
      self.oPolyline.SetProperty, DATA = data, POLYLINES = polyLines, LABEL_POLYLINES = IndGen(self.numVertices), LABEL_OFFSETS = Replicate(0,self.numVertices), /USE_TEXT_ALIGNMENTS, LABEL_NOGAPS = Replicate(0,self.numVertices)
      self.tessellator.reset
      self.tessellator.AddPolygon, [data[*,0:self.numVertices - 1]]
      result = self.tessellator.Tessellate(v, p)
      self.oPolygon.SetProperty, DATA = v, POLYGON = p, HIDE = self.fillHide
    ENDIF ELSE BEGIN
      self.oPolygon.SetProperty, /HIDE
      IF self.numVertices EQ 1 THEN polyLines = [2,0,0] ELSE polyLines = [2,0,1,2,1,0]
      self.oPolyline.SetProperty, DATA = data, POLYLINES = polyLines, LABEL_POLYLINES = IndGen(self.numVertices), LABEL_OFFSETS = Replicate(0,self.numVertices), /USE_TEXT_ALIGNMENTS, LABEL_NOGAPS = Replicate(0,self.numVertices)
    ENDELSE
  ENDIF
  
END

PRO AS_MaskObject::MakeArc

  @as_scatterheader.macro

    IF self.lock THEN RETURN

    centreX = self.centreX
    centreY = self.centreY

    IF self.beamRelative EQ 1 THEN BEGIN
      centreX = centreX + self.beamPosition[0]
      centreY = centreY + self.beamPosition[1]
    ENDIF

    th = self.angleMin*!dtor + (self.angleMax - self.angleMin) * !dtor * Findgen(self.numPoints/2)/(self.numPoints/2-1)
    cirpol = Transpose([[th],[Replicate(self.radiusMax, self.numPoints/2)]])
    IF self.radiusMin GT 0 THEN cirpol = [[cirpol],[Reverse(Transpose([[th],[Replicate(self.radiusMin, self.numPoints/2)]]),2)]]
    cirxy = CV_Coord(FROM_POLAR=cirpol,/TO_RECT)
    IF Abs(self.angleMax - self.angleMin) LT 360 AND self.radiusMin LE 0 THEN cirxy = [[0,0],[cirxy]] 
    self.SetProperty, data = cirxy + [centreX,centreY]#replicate(1,N_Elements(cirpol))

END


PRO as_maskObject__define

  void = {as_maskobject, $
          INHERITS IDLgrModel, $
          ;oModel         : Obj_New(), $
          oPolygon       : Obj_New(), $
          oPolyline      : Obj_New(), $
          tessellator    : Obj_New(), $
          maskLabels     : Obj_New(), $
          maskFont       : Obj_New(), $ 
          numVertices    : 0, $
          selectedVertex : 0, $
          maskShape      : 0, $
          maskType       : 0, $
          centreX        : 0.0, $
          centreY        : 0.0, $
          radiusMin      : 0.0, $
          radiusMax      : 0.0, $
          angleMin       : 0.0, $
          angleMax       : 0.0, $
          beamRelative   : 0, $
          beamPosition   : [0.0,0.0], $
          data           : Ptr_New(), $
          fillHide       : 0, $
          numPoints      : 0, $
          lock           : 0 }

END