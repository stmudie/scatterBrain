FUNCTION AS_ProfileObj::Init, _REF_Extra=extra

  self.profiles = Ptr_New()
  self.xRange = [0,1]
  self.yRange = [0,1]
  self.profileFont = Obj_New('IDLgrFont','Helvetica', SIZE = 10)
  self.xAxisTitle_obj = OBJ_NEW('IDLgrText','X Axis', FONT = self.profileFont)
  self.yAxisTitle_obj = OBJ_NEW('IDLgrText','Y Axis', FONT = self.profileFont)
  self.profileXAxis = Obj_New('IDLgrAxis', 0, RANGE = [0,100], TICKLEN=0.03, NAME = 'X_AXIS', LOCATION=[1000,0,0], TITLE=self.xAxisTitle_obj, COLOR = [0,0,0], /EXACT)
  self.profileYAxis = Obj_New('IDLgrAxis', 1, RANGE = [0,100], TICKLEN=0.03, NAME = 'Y_AXIS', LOCATION=[0,1000,0], TITLE=self.yAxisTitle_obj, COLOR = [0,0,0], /EXACT)
  self.profileXAxis->GetProperty, TICKTEXT = xAxisText_obj
  self.profileYAxis->GetProperty, TICKTEXT = yAxisText_obj
  
  self.xAxisText_obj = xAxisText_obj
  self.yAxisText_obj = yAxisText_obj
  self.xAxisText_obj->SetProperty, RECOMPUTE_DIMENSIONS=2
  self.yAxisText_obj->SetProperty, RECOMPUTE_DIMENSIONS=2
  self.xAxisTitle_obj->SetProperty, RECOMPUTE_DIMENSIONS=2
  self.yAxisTitle_obj->SetProperty, RECOMPUTE_DIMENSIONS=2
    
  modelInit = self->IDLgrModel::Init(_EXTRA=extra)
  self->Add, self.profileXAxis
  self->Add, self.profileYAxis

  RETURN, modelInit

END

FUNCTION AS_ProfileObj::scaleVector, range, Position=position

    ; This is a utility routine to calculate the scaling vector
    ; required to position a vector of specified range at a
    ; specific position given in normalized coordinates. The
    ; scaling vector is given as a two-element array like this:
    ;
    ;   scalingVector = [translationFactor, scalingFactor]
    ;
    ; The scaling vector should be used with the [XYZ]COORD_CONV
    ; keywords of a graphics object or model. For example, if you
    ; wanted to scale an X axis into the data range of -0.5 to 0.5,
    ; you might type something like this:
    ;
    ;   xAxis->GetProperty, Range=xRange
    ;   xScale = Normalize(xRange, Position=[-0.5, 0.5])
    ;   xAxis, XCoord_Conv=xScale

On_Error, 1
IF N_Params() EQ 0 THEN Message, 'Please pass range vector as argument.'

IF (N_Elements(position) EQ 0) THEN position = [0.0, 1.0] ELSE $
    position=Float(position)
    
range = Float(range)

scale = [((position[0]*range[1])-(position[1]*range[0])) / $
    (range[1]-range[0]), (position[1]-position[0])/(range[1]-range[0])]

RETURN, scale
END

PRO AS_ProfileObj::AddProfile, data, _REF_Extra=extra
  self.xRange = [MIN(data[0,*]) < self.xRange[0],MAX(data[0,*]) > self.xRange[1]]
  self.yRange = [MIN(data[1,*]) < self.yRange[0],MAX(data[1,*]) > self.yRange[1]]
    
  IF ~Ptr_Valid(self.profiles) THEN BEGIN
    numProf = 1
    self.profiles = Ptr_New(ObjArr(1))
    profilesTemp = ObjArr(1)
  ENDIF ELSE BEGIN
    numProf = N_Elements(*self.profiles) + 1
    profilesTemp = ObjArr(numProf)
    profilesTemp[0:numProf-2] = *self.profiles
    
  END
    
  profilesTemp[numProf-1] = Obj_New('IDLgrPlot')
  profilesTemp[numProf-1]->SetProperty, DATAX = data[0,*], DATAY = data[1,*], _EXTRA=extra
  
  FOR i = 0, numProf -1 DO BEGIN
    profilesTemp[i]->SetProperty, XCOORD_CONV = NORMALIZE(self.xRange), YCOORD_CONV = NORMALIZE(self.yRange), XRANGE = self.xRange, YRANGE = self.yRange 
  ENDFOR
  
  self->Add, profilesTemp[numProf-1]                 
  self.profileXAxis->SetProperty, XCOORD_CONV = self->scaleVector(self.xRange), RANGE = self.xRange
  self.profileYAxis->SetProperty, YCOORD_CONV = self->scaleVector(self.yRange), RANGE = self.yRange

  *self.profiles = profilesTemp

END

PRO AS_ProfileObj__Define

  void = {AS_ProfileObj , $
               INHERITS IDLgrModel,  $
               profiles       : Ptr_New(), $
               profileXAxis   : Obj_New(), $ 
               profileYAxis   : Obj_New(), $
               xAxisText_obj  : Obj_New(), $
               yAxisText_obj  : Obj_New(), $
               xAxisTitle_obj : Obj_New(), $ 
               yAxisTitle_obj : Obj_New(), $
               profileFont    : Obj_New(), $
               xRange         : [0.0,0.0], $
               yRange         : [0.0,0.0]  $ 
               }
END

