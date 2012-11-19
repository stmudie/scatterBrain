FUNCTION AS_Profiledata::Init, profile, error, fname, PRENORM = preNorm, CONFIGNAME = configName, QVECTOR = qVector, DSPACE = dSpace, TWOTHETA = twoTheta, PIXEL = pixel, BACK = back, TIME = time, I0COUNT = I0count, IBSCOUNT = IBSCount, I0NORM = I0Norm, IBSNorm = IBSNorm

  @as_scatterheader.macro

  IF KeyWord_Set(preNorm) THEN self.preNorm = 1

  IF Obj_Valid(back) THEN self.back = back
  IF Obj_Valid(logfileobj) THEN self.logfileobj = logfileobj
  IF N_Elements(profile) GE 1 THEN self.profile = Ptr_New(profile) ELSE self.profile = Ptr_New(/ALLOCATE_HEAP)
  IF N_Elements(error) GE 1 THEN self.error = Ptr_New(error) ELSE self.error = Ptr_New(/ALLOCATE_HEAP)
  IF N_Elements(fname) GE 1 THEN self.fname = fname ELSE self.fname = ''
  IF KeyWord_Set(configName) THEN self.configName = configName
  IF KeyWord_Set(qVector)  THEN self.qVector  = Ptr_New(qVector)  ELSE IF N_Elements(profile) GE 1 THEN self.qVector  = Ptr_New(IndGen(N_Elements(profile))) ELSE self.qVector  = Ptr_New(/ALLOCATE_HEAP) 
  IF KeyWord_Set(dSpace)   THEN self.dSpace   = Ptr_New(dSpace)   ELSE IF N_Elements(profile) GE 1 THEN self.dSpace   = Ptr_New(IndGen(N_Elements(profile))) ELSE self.dSpace   = Ptr_New(/ALLOCATE_HEAP)
  IF KeyWord_Set(twoTheta) THEN self.twoTheta = Ptr_New(twoTheta) ELSE IF N_Elements(profile) GE 1 THEN self.twoTheta = Ptr_New(IndGen(N_Elements(profile))) ELSE self.twoTheta = Ptr_New(/ALLOCATE_HEAP)
  IF KeyWord_Set(pixel)    THEN self.pixel    = Ptr_New(pixel)    ELSE IF N_Elements(profile) GE 1 THEN self.pixel    = Ptr_New(IndGen(N_Elements(profile))) ELSE self.pixel    = Ptr_New(/ALLOCATE_HEAP)
  
  IF KeyWord_Set(time) THEN BEGIN
    self.time = time
  ENDIF
  
  IF KeyWord_Set(I0Count) THEN BEGIN
    self.iocnts = I0Count
  ENDIF
  
  IF KeyWord_Set(IBSCount) THEN BEGIN
    self.bscnts = IBSCount
  ENDIF
  
  IF KeyWord_Set(IBSNorm) THEN BEGIN
    self.IbsNorm = IBSNorm
    self.SetNorm
  ENDIF
  
   IF KeyWord_Set(I0Norm) THEN BEGIN
    self.I0Norm = I0Norm
    self.SetNorm
  ENDIF


;  TODO Used to get data from log file this way. But now getting it as parameters on init. This way the profile doesn't have to correspond to file in logfile,
;  E.G. Averaged profiles.  
;  IF Obj_Valid(logFileObj) THEN index = self.logfileobj->GetIndex(self.fname) ELSE index = -1
;  
;  IF index GE 0 THEN BEGIN
;  
;    result = self.logfileobj->GetScale(index, I0 = I0, IBS = IBS, IT = IT, TIME = time, STAMP = timestamp)
;    self.time = time
;    self.itcnts = IT
;    self.bscnts = IBS
;    self.iocnts = I0
;    self.timestamp = timestamp
;  
;  ENDIF

  self.tnrm = 1.0
  self.mult = 1.0

  self.SetNorm, 0

  RETURN, 1
END

PRO AS_Profiledata::RemoveNotify, profiles

  @as_scatterheader.macro

 IF ~Ptr_Valid(self.notifyProfile) THEN RETURN
  
  numNotify = N_Elements(profiles)
  IF numNotify GT 0 THEN BEGIN
    positions = IntArr(numNotify)
    FOR i = 0, numNotify - 1 DO BEGIN
      positions[i] = Where(*self.notifyProfile EQ profiles[i])
    ENDFOR
    positions = positions[Where(positions GT -1)]
    compPositions = IntArr(N_Elements(*self.notifyProfile))
    compPositions[positions] = 1
    compPositions = Where(compPositions EQ 0)
    IF compPositions[0] EQ -1 THEN Ptr_Free, self.notifyProfile ELSE *self.notifyProfile = (*self.notifyProfile)[compPositions]  
  ENDIF

END

;PRO AS_Profiledata::Notify
;
;  FOR i = 0, N_Elements(*self.notifyProfile) - 1 DO (*self.notifyProfile)[i]->Callback
;
;END
;
;PRO AS_Profiledata::Callback
;
;  
;
;END

PRO AS_Profiledata::SetProperty, PROFILE=profile, QVECTOR=qVector, ERROR=error, BACK=back, OFFSET=offset, MULT=mult, NOTIFYPROFILE = notifyProfile, ABSCAL = absCal, COUNTERNORM = counterNorm, IBSNorm = IBSNorm, I0Norm = I0Norm
  
  @as_scatterheader.macro
  
  IF N_Elements(profile) GT 1 THEN *self.profile = profile
  IF N_Elements(qVector) GT 1 THEN *self.qVector = qVector
  IF N_Elements(error) GT 1 THEN *self.error = error
  
  IF KeyWord_Set(counterNorm) THEN BEGIN
    self.tnrm = counterNorm
    self.SetNorm
  ENDIF
  
  IF KeyWord_Set(IBSNorm) THEN BEGIN
    self.IbsNorm = IBSNorm
    self.SetNorm
  ENDIF
  
   IF KeyWord_Set(I0Norm) THEN BEGIN
    self.I0Norm = I0Norm
    self.SetNorm
  ENDIF
  
  IF Obj_Valid(Back) THEN BEGIN
    IF obj_valid(self.back) THEN self.back->RemoveNotify, self
    self.back = back 
    back->SetProperty, NOTIFYPROFILE = self
  ENDIF ELSE IF N_Elements(back) EQ 1 THEN BEGIN
    IF Obj_Valid(self.back) THEN self.back->RemoveNotify, self
    self.back = Obj_New()
  ENDIF
  
  IF N_Elements(offset) THEN BEGIN
    self.os = offset
    ;self->Notify
  ENDIF
  IF N_Elements(mult) GE 1 THEN BEGIN
    self.mult = mult
    ;self->Notify
  ENDIF
  
  numNotify = N_Elements(notifyProfile)
  IF numNotify GT 0 THEN BEGIN
    classes = StrArr(numNotify)
    FOR i = 0, numNotify - 1 DO classes[i] = Where([Obj_Class(notifyProfile[i]),Obj_Class(notifyProfile[i],/SUPERCLASS)] EQ 'AS_PROFILEDATA')
    IF ~Ptr_Valid(self.notifyProfile) THEN self.notifyProfile = Ptr_New((notifyProfile[classes GT -1])[0])
    IF N_Elements(notifyProfile[classes GT -1]) GT 0 THEN BEGIN
      FOREACH noteProfile, notifyProfile[Where(classes GT -1)] DO IF (Where(*self.notifyProfile EQ noteProfile))[0] EQ -1 THEN *self.notifyProfile = [*self.notifyProfile, noteProfile]
    ENDIF
      
  ENDIF
    
END

PRO AS_Profiledata::GetProperty, QVECTOR=qVector, ERROR=error, MULT=mult, OFFSET=offset, FNAME=fname, NOTIFYPROFILE = notifyProfile, I0 = I0, BS = BS, TIME = TIME, BACKGROUND = background, BACKOBJ=backObj, CONFIGNAME = configName
  
  @as_scatterheader.macro
  
  IF Arg_Present(qVector) THEN qVector = (*self.qVector)
  IF Arg_Present(error) THEN error = (*self.error)
  IF Arg_Present(offset) THEN offset = self.os
  IF Arg_Present(mult) THEN mult = self.mult  
  IF Arg_Present(fname) THEN fname = self.fname
  IF Arg_Present(notifyProfile) THEN IF Ptr_Valid(self.notifyProfile) THEN notifyProfile = *self.notifyProfile
  IF Arg_Present(I0) THEN I0 = self.iocnts
  IF Arg_Present(BS) THEN BS = self.bscnts
  IF Arg_Present(TIME) THEN time = self.time
  IF Arg_Present(background) THEN background = self.back.GetData(/BACK)
  IF Arg_Present(backObj) THEN backObj = self.back
  IF Arg_Present(configName) THEN configName = self.configName
END

FUNCTION AS_Profiledata::GetData, PLOT_DSPACE = plotDSpace, PLOT_TWOTHETA = plotTwoTheta, PLOT_PIXEL = plotPixel, XLOG = xLog, YLOG = yLog, BACK = back, NOMULTOFFSET = nomultOffset, NONORM = noNorm

  @as_scatterheader.macro

  qvector = *self.qvector
  profile = *self.profile
  error = *self.error
  IF N_Elements(profile) EQ 0 THEN RETURN, -1
  IF N_Elements(error) NE N_Elements(profile) THEN error = FltArr(N_Elements(profile))
  IF ~KeyWord_Set(noNorm) AND self.preNorm NE 1 THEN BEGIN
    profile = self.sf*profile
    error = self.sf*error
  ENDIF
  IF ~KeyWord_Set(NOMULTOFFSET) THEN profile = self.mult * profile + self.os
  
  IF KeyWord_Set(BACK) AND Obj_Valid(self.back) THEN BEGIN
    background = self.back->GetData(PLOT_DSPACE = plotDSpace, PLOT_TWOTHETA = plotTwoTheta, PLOT_PIXEL = plotPixel)
    back_ind = as_where_array(Reform(background[0,*]),qVector, prof_ind)
    IF N_Elements(back_ind) EQ 1 THEN BEGIN
      result = Dialog_Message('Background and sample have different q-vectors, returning.')
      RETURN, -1
    ENDIF
    
    profile = profile[prof_ind] - background[1,back_ind]
    error = error[prof_ind] + background[2,back_ind]
    qvector = qvector[prof_ind]
  ENDIF
  
  IF KeyWord_Set(YLOG) THEN BEGIN
    posElements = WHERE(profile GT 0, nPos)
    IF (nPos GT 0) THEN BEGIN
      error = [[ALog10(profile + error)],[ALog10(profile - error)]] - ALog10(profile)#Replicate(1,2)
      profile = ALog10(profile)
    ENDIF ELSE BEGIN
      result = Dialog_Message('All points non-positive, log plot will be garbage!')
      RETURN, -1
    ENDELSE  
  ENDIF
    
  IF KeyWord_Set(XLOG) THEN BEGIN
    IF KeyWord_Set(plotDSpace) AND ~(KeyWord_Set(plotTwoTheta) OR KeyWord_Set(plotPixel)) THEN RETURN, Transpose([[ALOG10(*self.dSpace)],[profile],[error]])
    IF KeyWord_Set(plotTwoTheta) AND ~(KeyWord_Set(plotDSpace) OR KeyWord_Set(plotPixel)) THEN RETURN, Transpose([[ALOG10(*self.twoTheta)],[profile],[error]])
    IF KeyWord_Set(plotPixel) AND ~(KeyWord_Set(plotTwoTheta) OR KeyWord_Set(plotDSpace)) THEN RETURN, Transpose([[ALOG10(*self.pixel)],[profile],[error]])
    RETURN, [Transpose(ALog10(qVector)),Transpose(profile),Transpose(error)]
  ENDIF ELSE BEGIN  
    IF KeyWord_Set(plotDSpace) AND ~(KeyWord_Set(plotTwoTheta) OR KeyWord_Set(plotPixel)) THEN RETURN, Transpose([[*self.dSpace],[profile],[error]])
    IF KeyWord_Set(plotTwoTheta) AND ~(KeyWord_Set(plotDSpace) OR KeyWord_Set(plotPixel)) THEN RETURN, Transpose([[*self.twoTheta],[profile],[error]])
    IF KeyWord_Set(plotPixel) AND ~(KeyWord_Set(plotTwoTheta) OR KeyWord_Set(plotDSpace)) THEN RETURN, Transpose([[*self.pixel],[profile],[error]])
    RETURN, [Transpose(qVector),Transpose(profile),Transpose(error)]
  ENDELSE
END

PRO AS_ProfileData::SetNorm, normType

  @as_scatterheader.macro

  ; TODO Check definitions for Io and Ibs normalization. The original normalize to Io and T has a tblank (typical blank transmission) term??
  IF N_Elements(normType) GE 1 THEN self.normType = normType ELSE normType = self.normType
  CASE normType OF
    0: self.sf = 1                                ; no normalization
    1: self.sf = 1./(self.iocnts/self.I0Norm)                    ; normalize to Io only
    2: self.sf = 1./(self.bscnts/self.IBSNorm)                   ; normalize to Ibs only
    3: self.sf = 1/self.I0Norm/self.IBSNorm ;$     ; normalize to Io and Ibs
                 ;* self.I0IBSNorm
    4: self.sf = 1/self.iosf /self.itsf $
                 * self.tnrm ;* self.tblank        ; normalize to Io & T          
  ENDCASE
  ;self.sf = self.sf*self.mult
END

PRO AS_ProfileData::SetCalib, CSCalib, ionrm

  @as_scatterheader.macro

  self.sf = CSCalib/(self.bscnts/self.IBSNorm) ;self.tnrm / self.itsf / self.iocnts * ionrm * self.mult * CSCalib
END

FUNCTION AS_ProfileData::FitPeak, range, PARAMS = params, PLOT = plot, PEAKSIGMA = peakSigma, CHI = chi

  @as_scatterheader.macro

  IF range[0] EQ range[1] THEN range = ''
 
  IF N_Elements(range) NE 2 THEN range = [0,N_Elements(*self.qVector)-1]
  
  minQPos = (Where(*self.qVector GT range[0]))[0] > 0
  maxQPos = (Where(*self.qVector GT range[1]))[0] 
  IF maxQPos EQ -1 THEN maxQPos = (N_Elements(*self.qVector)-1)
  
  
  peakFit = GaussFit((*self.qVector)[minQPos:maxQPos],(self.GetData())[1,minQPos:maxQPos], coeffs, SIGMA = sigma, CHI = fitChi, NTERMS = 5)

  IF Arg_Present(params) THEN params = coeffs
  IF Arg_Present(peakSigma) THEN peakSigma = sigma[1]
  IF Arg_Present(chi) THEN chi = fitChi
  IF Arg_Present(plot) THEN plot = Transpose([[(*self.qVector)[minQPos:maxQPos]],[peakFit]])
  
  RETURN, coeffs[1]

END

PRO AS_ProfileData::Cleanup

  @as_scatterheader.macro

   IF Obj_Valid(self.back) THEN self.back.removeNotify, self

   Ptr_Free, self.profile
   Ptr_Free, self.error
   Ptr_Free, self.qVector
   Ptr_Free, self.dSpace
   Ptr_Free, self.twoTheta
   Ptr_Free, self.pixel
   

END

PRO as_profiledata__define

  void = {as_profiledata, $
          profile       : Ptr_New(), $
          notifyProfile : Ptr_New(), $
          error         : Ptr_New(), $
          fname         : ''       , $
          qVector       : Ptr_New(), $
          dSpace        : Ptr_New(), $
          twoTheta      : Ptr_New(), $ 
          pixel         : Ptr_New(), $
          back          : Obj_New(), $
          logfileobj    : Obj_New(), $
          configName    : ''       , $
          npts          : 0,         $ ; number of points in profile
          mult          : 0.0,       $ ; muliplier to allow tweaking of plotted profiles
          os            : 0.0,       $ ; offset of plotted profiles
          iocnts        : 0L,        $ ; Io total counts
          bscnts        : 0L,        $ ; Beam stop total counts
          itcnts        : 0L,        $ 
          itsf          : 0.0,       $ ; transmission scale factors (calc'd from log_file)
          i0Norm        : 0.0,       $ ; Io scale factor
          ibsNorm       : 0.0,       $ ; Ibs scale factor
          sf            : 0.0,       $ ; Total scale factor
          time          : 0.0,       $ ; Total duration of exposure (secs)
          timestamp     : '',        $
          I0IBSNorm     : 0.0,       $ ; Factor to normalise I0 and Ibs - derived from I0/IBS from clear air shot.
          tnrm          : 0.0,       $ ; Factor to normalise I0 and It - derived from It/I0 from a clear air shot. This value is filled in from the profile container obj.
          normType      : 0,         $
          preNorm       : 0          $
          }
       
END
