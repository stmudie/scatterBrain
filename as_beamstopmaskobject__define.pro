FUNCTION as_beamstopmaskobject::init, xCentre, yCentre, radius, angle, width, length
   
  @as_scatterheader.macro 
   
  ;self.oModel = IDLgrModel()
  self.numVertices = 23
  self.oPolygon = IDLgrPolygon()
  self.oPolyline = IDLgrPolyline()
  self.data = Ptr_New(/ALLOCATE_HEAP)
  self.tessellator = IDLgrTessellator()
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
  self->RegisterProperty, 'maskType', 9, ENUMLIST = ['Not Active','Excluding'], NAME = 'Mask Type'
  self->RegisterProperty, 'centreX', 3, NAME = 'Centre X'
  self->RegisterProperty, 'centreY', 3, NAME = 'Centre Y'
  self->RegisterProperty, 'offsetX', 3, NAME = 'Offset X'
  self->RegisterProperty, 'offsetY', 3, NAME = 'Offset Y'
  self->RegisterProperty, 'radius', 3, NAME = 'Radius'
  self->RegisterProperty, 'angle', 3, NAME = 'Angle', VALID_RANGE = [-360D,360D]
  self->RegisterProperty, 'width', 3, NAME = 'Width'
  self->RegisterProperty, 'length', 3, NAME = 'Length'


  self.SetProperty, NAME = 'Beam Stop Mask'
  self.SetProperty, CENTREX = xCentre
  self.SetProperty, CENTREY = yCentre
  self.SetProperty, RADIUS = radius
  self.SetProperty, ANGLE = angle
  self.SetProperty, LENGTH = length
  self.SetProperty, WIDTH = width
        
  RETURN, Result

END

PRO as_beamstopmaskobject::CreateMaskPolygon

  @as_scatterheader.macro

    IF self.lock THEN RETURN

    angle = !dtor*self.angle
    
    angleMax = angle - asin(self.width/(2*self.radius))
    angleMin = angle + asin(self.width/(2*self.radius))-2*!dpi

    th = angleMin + (angleMax - angleMin) * Findgen(20)/19
    cirpol = Transpose([[th],[Replicate(self.radius, 20)]])
    cirxy = CV_Coord(FROM_POLAR=cirpol,/TO_RECT)
    cirxy = cirxy + [self.centreX+self.offsetX,self.centreY+self.offsetY]#replicate(1,N_Elements(cirpol))
    startPoint = cirxy[*,0]+self.length*[cos(angle),sin(angle)]
    endPoint = cirxy[*,-1]+self.length*[cos(angle),sin(angle)]
    beamstop = [[startpoint], [cirxy], [endpoint], [startpoint]]
    IF Total(Finite(beamstop)) EQ N_Elements(beamstop) THEN self.SetProperty, data = beamstop

END

PRO as_beamstopmaskobject::GetProperty, $
  COLOUR = COLOUR, $
  LINEOPACITY = lineOpacity, $
  FILLOPACITY = fillOpacity, $
  LINETHICKNESS = lineThickness, $
  LINESTYLE = lineStyle, $
  MASKTYPE = maskType, $
  CENTREX = centreX, $
  CENTREY = centreY, $
  OFFSETX = offsetX, $
  OFFSETY = offsetY, $
  RADIUS = radius, $
  ANGLE = angle, $
  WIDTH = width, $
  LENGTH = length, $
  LOCK = lock, $
  DATA = data, $
  _Ref_extra = extra
  
  @as_scatterheader.macro
  
  IF Arg_Present(colour) THEN self.oPolygon.GetProperty, COLOR=COLOUR
  IF Arg_Present(lineOpacity) THEN self.oPolyline.GetProperty, ALPHA_CHANNEL = lineOpacity
  IF Arg_Present(fillOpacity) THEN self.oPolygon.GetProperty, ALPHA_CHANNEL = fillOpacity
  IF Arg_Present(lineThickness) THEN self.oPolyline.GetProperty, THICK = lineThickness
  IF Arg_Present(lineStyle) THEN self.oPolyline.GetProperty, LINESTYLE = lineStyle
  IF Arg_Present(maskType) THEN maskType = self.maskType
  IF Arg_Present(centreX) THEN centreX = self.centreX
  IF Arg_Present(centreY) THEN centreY = self.centreY
  IF Arg_Present(offsetX) THEN offsetX = self.offsetX
  IF Arg_Present(offsetY) THEN offsetY = self.offsetY
  IF Arg_Present(radius) THEN radius = self.radius
  IF Arg_Present(angle) THEN angle = self.angle
  IF Arg_Present(length) THEN length = self.length
  IF Arg_Present(width) THEN width = self.width
  IF Arg_Present(lock) THEN lock = self.lock
  IF Arg_Present(data) THEN data = *self.data
      
  self->IDLgrModel::GetProperty, _EXTRA = extra
  
END

FUNCTION as_beamstopmaskobject::GetSaveParams

  @as_scatterheader.macro
  
  IF N_Elements(*self.data) EQ 0 THEN RETURN, -1
  RETURN, {name : self.NAME, maskshape : 'Beamstop',  masktype : self.maskType, offsetX : self.offsetX, offsetY : self.offsetY, radius : self.radius, angle : self.angle, width : self.width}
 
END

PRO as_beamstopmaskobject::SetProperty, $
  COLOUR = COLOUR, $
  LINEOPACITY = lineOpacity, $
  FILLOPACITY = fillOpacity, $
  FILLHIDE = fillHide, $
  LINETHICKNESS = lineThickness, $
  LINESTYLE = lineStyle, $
  MASKTYPE = maskType, $
  BEAMPOSITION = beamPosition, $
  CENTREX = centreX, $
  CENTREY = centreY, $
  OFFSETX = offsetX, $
  OFFSETY = offsetY, $
  RADIUS = radius, $
  ANGLE = angle, $
  WIDTH = width, $
  LENGTH = length, $
  LOCK = Lock, $
  DATA = data,$
  _Ref_extra = extra
  
  @as_scatterheader.macro
  
  IF N_Elements(lock)          THEN self.lock = lock
  IF self.lock THEN RETURN
  IF N_Elements(beamPosition) EQ 2 THEN BEGIN
    self.centreX = beamPosition[0]
    self.SetProperty, CENTREY = beamPosition[1]
  ENDIF
  IF N_Elements(colour)        THEN BEGIN
    self.oPolygon.SetProperty, COLOR=COLOUR
    self.oPolyline.SetProperty, COLOR=COLOUR
  ENDIF
  IF N_Elements(lineOpacity)   THEN BEGIN
    self.oPolyline.SetProperty, ALPHA_CHANNEL = Fix(lineOpacity*100)/100.
  ENDIF
  IF N_Elements(fillOpacity)   THEN self.oPolygon.SetProperty, ALPHA_CHANNEL = Fix(fillOpacity)/100.
  IF N_Elements(fillHide)      THEN BEGIN
    self.oPolygon.SetProperty, HIDE = fillHide
    self.fillHide = fillHide
  ENDIF
  IF N_Elements(lineThickness) THEN self.oPolyline.SetProperty, THICK = lineThickness
  IF N_Elements(lineStyle)     THEN self.oPolyline.SetProperty, LINESTYLE = lineStyle
  IF N_Elements(maskType) THEN BEGIN
    IF maskType EQ 0 THEN self.SetProperty, LINEOPACITY = 0, FILLOPACITY = 0
    IF maskType GT 0 AND self.maskType EQ 0 THEN self.SetProperty, LINEOPACITY = 1, FILLOPACITY = 0.5
    self.maskType = maskType
  ENDIF
  IF N_Elements(centreX)       THEN self.centreX = centreX
  IF N_Elements(centreY)       THEN self.centreY = centreY
  IF N_Elements(offsetX)       THEN self.offsetX = offsetX
  IF N_Elements(offsetY)       THEN self.offsetY = offsetY
  IF N_Elements(radius)     THEN self.radius = radius
  IF N_Elements(angle)      THEN self.angle = angle
  IF N_Elements(width)      THEN self.width = width
  IF N_Elements(length)      THEN self.length = length
  IF N_Elements(centreX) + N_Elements(centreY) + N_Elements(radius) + N_Elements(angle) + N_Elements(width) + N_Elements(length) +$
     N_Elements(offsetX) + N_Elements(offsetY) GT 0 THEN BEGIN
     
     self.CreateMaskPolygon
  ENDIF
  IF N_Elements(data) GT 0 THEN BEGIN
    self.UpdatePolygon, data
  ENDIF
  
  self->IDLgrModel::SetProperty, _EXTRA = extra
  
END

PRO as_beamstopmaskobject::ShowLabels, show

  @as_scatterheader.macro

  show = show < 1
  count = self.maskLabels.count()
  IF count GT 0 THEN BEGIN
    labels =  (self.maskLabels.Get(/ALL))
    FOREACH l, labels DO l.SetProperty, ALPHA_CHANNEL = show
  ENDIF
      
END

FUNCTION as_beamstopmaskobject::NumVertices

  @as_scatterheader.macro

  RETURN, self.numVertices

END

PRO as_beamstopmaskobject::SelectVertex, vertex 

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

PRO as_beamstopmaskobject::DeleteVertex

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

PRO as_beamstopmaskobject::MoveVertex, position, ADD=add, RELATIVE=relative

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

PRO as_beamstopmaskobject::UpdatePolygon, data

  @as_scatterheader.macro

  IF self.lock THEN RETURN
  
  IF N_Elements(data) NE 0 THEN BEGIN
    *self.data = data
    polyLines = IntArr(3*(23))
    FOR i = 0, 21 DO polyLines[i*3:i*3+2] = [2,i,i+1]
    polyLines[-3:-1] = [2,22,0] ; Definition for polylines keyword to IDLgrPolyline is : [m1, i0, i1, ...,im1-1,m2,i0, i1, ..., im2-1 ...] where each mj is an integer specifying the number of vertices that define the polyline (the vertex count), and each associated set of i0...im-1 are indices into the array of vertices   
    self.oPolyline.SetProperty, DATA = data, POLYLINES = polyLines, /USE_TEXT_ALIGNMENTS
    self.tessellator.reset
    self.tessellator.AddPolygon, [data[*,0:22]]
    result = self.tessellator.Tessellate(v, p)
    self.oPolygon.SetProperty, DATA = v, POLYGON = p, HIDE = self.fillHide
  ENDIF
  
END


PRO as_beamstopmaskobject__define

  void = {as_beamstopmaskobject, $
          INHERITS IDLgrModel, $
          ;oModel         : Obj_New(), $
          oPolygon       : Obj_New(), $
          oPolyline      : Obj_New(), $
          tessellator    : Obj_New(), $
          numVertices    : 0, $
          maskType       : 0, $
          centreX        : 0.0, $
          centreY        : 0.0, $
          offsetX        : 0.0, $
          offsetY        : 0.0, $
          radius         : 0.0, $
          angle          : 0.0, $
          width          : 0.0, $
          length         : 0.0, $
          data           : Ptr_New(), $
          fillHide       : 0, $
          lock           : 0 }

END