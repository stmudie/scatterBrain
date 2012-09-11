FUNCTION AS__SaxsQData::Init, p_arr=p_arr,tth_arr=tth_arr,q_arr=q_arr,d=d,pbin=pbin,tthbin=tthbin,qbin=qbin, $
                              dbin=dbin,bkgnd=bkgnd,qmns=qmns,qmxs=qmxs
  
  @as_scatterheader.macro
  
  IF Keyword_Set(p_arr)  THEN self.p_arr  =     Ptr_New(p_arr)   ELSE self.p_arr    = Ptr_New(/ALLOCATE_HEAP)
  IF Keyword_Set(tth_arr)THEN self.tth_arr=     Ptr_New(tth_arr) ELSE self.tth_arr  = Ptr_New(/ALLOCATE_HEAP)
  IF Keyword_Set(q_arr)  THEN self.q_arr  =     Ptr_New(q_arr)   ELSE self.q_arr    = Ptr_New(/ALLOCATE_HEAP)
  IF Keyword_Set(d_arr)  THEN self.d_arr  =     Ptr_New(d_arr)   ELSE self.d_arr    = Ptr_New(/ALLOCATE_HEAP)
  IF Keyword_Set(pbin)   THEN self.pbin   =     pbin
  IF Keyword_Set(tthbin) THEN self.tthbin =     Ptr_New(tthbin)  ELSE self.tthbin   = Ptr_New(/ALLOCATE_HEAP)
  IF Keyword_Set(qbin)   THEN self.qbin   =     Ptr_New(qbin)    ELSE self.qbin     = Ptr_New(/ALLOCATE_HEAP)
  IF Keyword_Set(dbin)   THEN self.dbin   =     Ptr_New(dbin)    ELSE self.dbin     = Ptr_New(/ALLOCATE_HEAP)
  IF Keyword_Set(bkgnd)  THEN self.bkgnd  =     bkgnd
  IF Keyword_Set(qmns)   THEN self.qmns   =     qmns 
  IF Keyword_Set(qmxs)   THEN self.qmxs   =     qmxs
  
  Return, 1

END

FUNCTION AS__SaxsQData::GetProperty, p_arr=p_arr,tth_arr=tth_arr,q_arr=q_arr,d_arr=d_arr,pbin=pbin,tthbin=tthbin,qbin=qbin, $
                                     dbin=dbin,bkgnd=bkgnd,qmns=qmns,qmxs=qmxs,pos=pos

  @as_scatterheader.macro

  IF Keyword_Set(p_arr)  THEN BEGIN
    IF Size(*self.p_arr, /TYPE) EQ 0 THEN RETURN, 0
    IF Keyword_Set(pos) THEN Return, *self.p_arr(pos) ELSE Return, *self.p_arr
  ENDIF
  IF Keyword_Set(tth_arr)THEN BEGIN
    IF Size(*self.tth_arr, /TYPE) EQ 0 THEN Return, 0
    IF Keyword_Set(pos) THEN Return, *self.tth_arr(pos) ELSE Return, *self.tth_arr
  ENDIF  
  IF Keyword_Set(q_arr)  THEN BEGIN 
    IF Size(*self.q_arr, /TYPE) EQ 0 THEN Return, 0 
    IF Keyword_Set(pos) THEN Return, *self.q_arr(pos) ELSE Return, *self.q_arr
  ENDIF
  IF Keyword_Set(d_arr)  THEN BEGIN 
    IF Size(*self.d_arr, /TYPE) EQ 0 THEN Return, 0
    IF Keyword_Set(pos) THEN Return, *self.d_arr(pos) ELSE Return, *self.d_arr
  ENDIF
  IF Keyword_Set(pbin)   THEN Return, self.pbin
  IF Keyword_Set(tthbin)  THEN BEGIN
    IF Size(*self.tthbin, /TYPE) EQ 0 THEN RETURN, 0
    IF Keyword_Set(pos) THEN Return, *self.tthbin(pos) ELSE Return, *self.tthbin
  ENDIF
  IF Keyword_Set(qbin)  THEN BEGIN
    IF Size(*self.qbin, /TYPE) EQ 0 THEN RETURN, 0
    IF Keyword_Set(pos) THEN Return, *self.qbin(pos) ELSE Return, *self.qbin
  ENDIF
  IF Keyword_Set(dbin)  THEN BEGIN
    IF Size(*self.dbin, /TYPE) EQ 0 THEN RETURN, 0
    IF Keyword_Set(pos) THEN Return, *self.dbin(pos) ELSE Return, *self.dbin
  ENDIF
  IF Keyword_Set(bkgnd)  THEN Return, self.bkgnd
  IF Keyword_Set(qmns)   THEN Return, self.qmns
  IF Keyword_Set(qmxs)   THEN Return, self.qmxs
  
  Return,  { p_arr  : *self.p_arr, $ 
            tth_arr : *self.tth_arr, $
            q_arr   : *self.q_arr, $
            d_arr   : *self.d_arr, $
            pbin    : self.pbin, $
            tthbin  : self.tthbin, $
            qbin    : self.qbin,  $
            dbin    : self.dbin, $
            bkgnd   : self.bkgnd, $
            qmns    : self.qmns, $
            qmxs    : self.qmxs }
  
END

FUNCTION AS__SaxsQData::SetProperty, p_arr=p_arr,tth_arr=tth_arr,q_arr=q_arr,d_arr=d_arr,pbin=pbin,tthbin=tthbin,qbin=qbin, $
                                     dbin=dbin,bkgnd=bkgnd,qmns=qmns,qmxs=qmxs

  @as_scatterheader.macro

  IF Keyword_Set(p_arr)  THEN *self.p_arr = p_arr 
  IF Keyword_Set(tth_arr)THEN *self.tth_arr = tth_arr
  IF Keyword_Set(q_arr)  THEN *self.q_arr = q_arr
  IF Keyword_Set(d_arr)  THEN *self.d_arr = d_arr
  IF Keyword_Set(pbin)   THEN self.pbin = pbin
  IF Keyword_Set(tthbin) THEN *self.tthbin = tthbin
  IF Keyword_Set(qbin)   THEN *self.qbin = qbin
  IF Keyword_Set(dbin)   THEN *self.dbin = dbin
  IF Keyword_Set(bkgnd)  THEN self.bkgnd = bkgnd
  IF Keyword_Set(qmns)   THEN self.qmns = qmns
  IF Keyword_Set(qmxs)   THEN self.qmxs = qmxs
  
  Return,  1
  
END

PRO AS__SaxsQData__Define

void = {AS__SaxsQData, $
        p_arr:      Ptr_New(),      $ ; pixel ordinates for 1D scattering profiles
        tth_arr:    Ptr_New(),      $ ; 2 theta (deg) ordinates for 1D scattering profiles
        q_arr:      Ptr_New(),      $ ; Q (A-1) ordinates for 1D scattering profiles
        d_arr:      Ptr_New(),      $ ; d (A) ordinates for 1D scattering profiles
        pbin:       0,              $ ; pixels per pixel (always = 1)
        tthbin:     Ptr_New(),              $ ; deg per pixel
        qbin:       Ptr_New(),              $ ; A-1 per pixel
        dbin:       Ptr_New(),              $ ; Angstroms per pixel
        bkgnd:      0,              $ ; Correctly binned frame constant offset value
        qmns:   fltarr(6),          $ ; vector of qmins for autoprocessing
        qmxs:   fltarr(6)           $ ; vector of qmaxs for autoprocessing
    }

END    
