FUNCTION as_plotobject::init, ERRORBARS = errorBars, SHOWERRORBARS = showErrorBars, _REF_EXTRA = extra

  @as_scatterheader.macro

  IF KeyWord_Set(errorBars) THEN self.errorBars = errorBars
  IF KeyWord_Set(showErrorBars) THEN self.showErrorBars = showErrorBars

  self.errorBarObj = IDLgrPolyline(COLOR = [150,150,150])

  RETURN, self.idlgrplot::init(_EXTRA = extra)

END

PRO as_plotobject::SetProperty, ERRORBARS = errorBars, SHOWERRORBARS = showErrorBars, XCoord_Conv = XCoord_Conv, YCoord_Conv = YCoord_Conv, _REF_EXTRA = extra

  @as_scatterheader.macro
  
  IF N_Elements(errorBars) GT 0 THEN IF Ptr_Valid(self.errorBars) THEN *self.errorBars = errorBars ELSE self.errorBars = Ptr_New(errorBars)
  IF N_Elements(showErrorBars) GT 0 THEN self.showErrorPlot, showErrorBars

  IF KeyWord_Set(XCoord_Conv) THEN BEGIN
    self.errorBarObj.SetProperty, XCoord_CONV = XCoord_CONV 
    self.idlgrplot::SetProperty, XCoord_CONV = XCoord_CONV
  ENDIF
  IF KeyWord_Set(YCoord_Conv) THEN BEGIN
    self.errorBarObj.SetProperty, YCoord_CONV = YCoord_CONV 
    self.idlgrplot::SetProperty, YCoord_CONV = YCoord_CONV
  ENDIF
  
  self.idlgrplot::SetProperty, _EXTRA = extra
  
  IF N_Elements(extra) GT 0 THEN BEGIN
  
    IF Where(StrUpCase(extra) EQ 'HIDE',/NULL) NE !NULL THEN BEGIN
      self.idlgrplot::GetProperty, HIDE = hide
      IF hide EQ 1 THEN self.errorBarObj.SetProperty, HIDE = 1 ELSE self.showErrorPlot
    ENDIF
   
  ENDIF
  
  IF N_Elements(extra) GT 0 OR KeyWord_Set(ERRORBARS) THEN BEGIN
    IF (Where(StrUpCase(extra) EQ 'DATAX' or StrUpCase(extra) EQ 'DATAY',/NULL)) NE !NULL OR KeyWord_Set(ERRORBARS) THEN self.PlotErrors
  ENDIF
  
END

PRO as_plotobject::showErrorPlot, show

  IF N_Elements(show) GT 0 THEN self.showErrorBars = show
  self.errorBarObj.SetProperty, HIDE = ~self.showErrorBars
  
END

PRO as_plotobject::GetProperty, ERRORBARS = errorBars, SHOWERRORBARS = showErrorBars, _REF_EXTRA = extra

  @as_scatterheader.macro
  
  IF Arg_Present(errorBars) THEN IF Ptr_Valid(errorBars) THEN errorBars = *self.errorBars
  IF Arg_Present(showErrorBars) THEN showErrorBars = self.showErrorBars

  self.idlgrplot::GetProperty, _EXTRA = extra

END

PRO as_plotobject::PlotErrors, FORCE=force

  @as_scatterheader.macro

  IF KeyWord_Set(force) THEN self.showErrorPlot, 1
  
  nPoints = N_Elements((*self.Data)[0,*])
  x = Reform([(*self.Data)[0,*],(*self.Data)[0,*]],2*nPoints)
  IF Size(*self.errorBars, /N_DIMENSIONS) EQ 1 THEN errorBars = Transpose([[(*self.errorBars)],[(*self.errorBars)]]) ELSE errorBars = *self.errorBars
  y = Reform([(*self.Data)[1,*]+errorBars[1,*], (*self.Data)[1,*]+errorBars[0,*]],2*nPoints)
  
  polylines = Reform(Transpose([[replicate(2,nPoints)],[IndGen(nPoints)*2],[IndGen(nPoints)*2+1]]),3*nPoints)

  self.errorBarObj.SetProperty, DATA = Transpose([[x],[y]]),polylines = polylines
  
  self.errorBarObj.GetProperty, PARENT = parent
  IF ~Obj_Valid(parent) THEN self.PARENT.add, self.errorBarObj

END

PRO as_plotobject::CleanUp

  @as_scatterheader.macro

  Obj_Destroy, self.errorBarObj

  self.idlgrplot::Cleanup

END

PRO as_plotobject__define

  void = {as_plotobject, $
          INHERITS IDLgrPlot, $
          errorBarObj : Obj_New(), $
          errorBars : Ptr_New(), $
          showErrorBars : 0 }
          
END