FUNCTION AS_SaxsImageTools::Init, qData, profileObj, _REF_Extra=extra

  @as_scatterheader.macro

  RETURN, self->AS_CakeObj::Init(qData, profileObj,_Extra=extra)

END

PRO AS_SaxsImageTools::qCalibChange, qcalib, CONFIGNO = configNo

  @as_scatterheader.macro

  self.frame.len = qcalib.cameralength
  self.frame.wlen = qcalib.wavelength
  self.frame.detAngle = qcalib.detectorAngle
  self.cake.ok = 0
  self.cake.newmask = 1

  self.as_maskobj::StoreParams, self.frame.logObj, CONFIG = configNo

END

PRO AS_SaxsImageTools::fitCircle, points

  @as_scatterheader.macro
  
  IF ~Obj_Valid(self.PARENT) THEN RETURN
       
  cir_3pnt,[points[0],points[2],points[4]], [points[1],points[3],points[5]], r, x0, y0
  thickness = 10
  ang = !dpi*indgen(100)/51.
  rad = replicate(r, 100)
  fitCirclesObj = ObjArr(3)
  FOR i = -1, 1, 2 DO BEGIN
    circPol = transpose([[ang],[rad+thickness*i]]) 
    circXY = cv_coord(FROM_POLAR=circpol,/TO_RECT)
    fitCirclesObj[(i+1)/2] = Obj_New('IDLgrPolyline', circXY[0,*]+x0, circXY[1,*]+y0, COLOR = [255,255,0])
    self.PARENT->Add, fitCirclesObj[(i+1)/2]
    self.frame.framewinObj->draw
  ENDFOR

  IF self->fitAnnulus(x0, y0, round(r), polar_arc, halfwidth=thickness) LT 0 THEN Return
  
  npts = n_elements(polar_arc[0,*])
  angdiff = polar_arc[0,npts-1]-polar_arc[0,0]

  ; Show the fitted radii as a function of angle. If we had a perfect first guess
  ; and a perfectly circular ring, this would be a horizontal straight line

  ;plot, polar_arc[0,*]*180/3.14,polar_arc[1,*],pos=[0.1,0.05,0.45,0.3] $
  ;        ,yrange=[min(polar_arc[1,*]),max(polar_arc[1,*])],psym=1,/noerase

  xyellipse = cv_coord(from_polar=[polar_arc[0,*],polar_arc[1,*]],/to_rect)
  x = reform(xyellipse[0,*]) + x0
  y = reform(xyellipse[1,*]) + y0

  if abs(angdiff) LT (!pi * 1.25) then $
    parms = MPFITELLIPSE(x, y, [r,r,x0,y0,0], WEIGHTS=reform(0.75/polar_arc[2,*]^2), /quiet, /circular) $
  else parms = MPFITELLIPSE(x, y, [r,r,x0,y0,0], WEIGHTS=reform(0.75/polar_arc[2,*]^2), /quiet,/tilt)

  ang = findgen(100)/50.0 * !pi
  xx = parms[2] + parms[0]*cos(ang)
  yy = parms[3] + parms[1]*sin(ang)

  ;plots, (xx-float(mask_polygon.xlow))/mask_polygon.zoom $
  ;     , (yy-float(mask_polygon.ylow))/mask_polygon.zoom,thick=1, /device

  ; Re-calculate the radial values from our new center
  if self->fitAnnulus(parms[2], parms[3],round(r), polar_arc, halfwidth=thickness) LT 0 then return

  ;plot, polar_arc[0,*]*180/3.14,polar_arc[1,*],pos=[0.6,0.05,0.95,0.3] $
   ;                   ,yrange=[min(polar_arc[1,*]),max(polar_arc[1,*])],psym=1,/noerase
  x = parms[2]
  y = parms[3]
  r = (parms[0]+parms[1])/2

  self.frame.xc = x
  self.frame.yc = y

  self.cake.ok = 0
  self.cake.newmask = 1
  temp = self->CakeSetup()
  
  Obj_Destroy, fitCirclesObj

END

FUNCTION AS_SaxsImageTools::fitAnnulus, x0, y0, iradius, polar_arc, halfwidth=halfwidth, nsec=nsec

  @as_scatterheader.macro

    IF N_Elements(halfwidth) EQ 0 then h = round(2.0/frmdata.nxpix) ELSE h=round(halfwidth)
    IF N_Elements(nsec) EQ 0 then nsec=0

    nx = self.frame.nxpix
    ny = self.frame.nypix

    x = fltarr(nx,ny)
    y = fltarr(nx,ny)
    FOR i = 0,nx-1 do y[i,*] = findgen(ny)   ; fill array with ascending row integers
    FOR i = 0,ny-1 do x[*,i] = findgen(nx)   ; fill array with ascending column integers

    x = x - x0
    y = y - y0

    IF self.cake.ispatial EQ 1 then BEGIN
        Print, 'Applying spatial correction'
        x = x + self.cake.xerror
        y = y + self.cake.yerror
    ENDIF

    radial = SqRt(x*x + y*y)
    rcount = Histogram(radial, binsize=1 $                    ; bin equal-radius pixels   -  changed it so masks are ignored when doing beam centre - not sure if this is a good idea?
                , min=0, omax=rmax, reverse_indices=lut)      ; make constant-radius LUT
;    rcount = Histogram(radial * *self.mask.mask, binsize=1 $ ; bin equal-radius pixels
;                , min=0, omax=rmax, reverse_indices=lut)     ; make constant-radius LUT
    IF N_Elements(rcount) LE 1 then BEGIN
        retmes = Dialog_Message('Problem with defined ROI')
        return,-1
    ENDIF

    Print, 'N_Elements(lut) = ', N_Elements(lut)
    ; construct vectors with frame and x,y data taken from valid integrable pixels
    fsub = !null
    xsub = !null
    ysub = !null
    
    FOR i=iradius-h,iradius+h do BEGIN
      IF lut[i] NE lut[i+1] THEN BEGIN
        fsub = [fsub,(*self.frame.rawData)[lut[lut[i]:lut[i+1]-1]]]
        xsub = [xsub,x[lut[lut[i]:lut[i+1]-1]]]
        ysub = [ysub,y[lut[lut[i]:lut[i+1]-1]]]
      ENDIF
    ENDFOR

    rsub = sqrt(xsub^2+ysub^2)
    asub = transpose([[atan(ysub,xsub)],[rsub]])
    minang = min(asub[0,*])
    maxang = max(asub[0,*])

    IF nsec EQ 0 then BEGIN
        anginc = 15.0/float(iradius)
        nangs = round((maxang-minang)/anginc)
    ENDIF ELSE BEGIN
        nangs=nsec
        anginc = !PI*2/float(nangs)
    ENDELSE
    print, 'Azimuth array = ',nangs,' sectors from ',minang*180/3.14,' to ',maxang*180/3.14 ,' deg'

    acount = histogram(asub[0,*],nbins=nangs,min=minang,binsize=anginc,reverse_indices=lut2)


    aprofile = fltarr(nangs, 2*h)
    aprof_err = fltarr(nangs, 2*h)
    polar_arc = fltarr(3,nangs)
    nexpected = ((iradius+h)^2-(iradius-h)^2) * anginc / 2
    print,'min radius = ',iradius-h,' max radius = ',iradius+h,' angle inc = ',anginc,' expected pixels = ', nexpected
    nexpected = round(0.8*nexpected)

    FOR i=0,nangs-1 do BEGIN
        IF acount[i] GT nexpected then BEGIN
            ftemp = fsub[lut2[lut2[i]:lut2[i+1]-1]]
            rtemp = rsub[lut2[lut2[i]:lut2[i+1]-1]]
            xtemp = xsub[lut2[lut2[i]:lut2[i+1]-1]]
            ytemp = ysub[lut2[lut2[i]:lut2[i+1]-1]]

            rcount = histogram(rtemp,binsize=1,min=iradius-h,max = iradius+h, reverse_indices=lut3)

            FOR j=0,2*h-1 do BEGIN
                IF rcount[j] GT 1 then BEGIN
                    aprofile[i,j] = total(ftemp[lut3[lut3[j]:lut3[j+1]-1]]) / rcount[j]
                    aprof_err[i,j] = stddev(ftemp[lut3[lut3[j]:lut3[j+1]-1]]) / sqrt(rcount[j])
                ENDIF
            ENDFOR

            polar_arc[0,i] = (i+0.5) * anginc + minang

            yfit = gaussfit(iradius-h+findgen(2*h),aprofile[i,*],fitp, nterms=5, sigma=sigerr)
            ;estim_err = total(abs(yfit-aprofile[i,*]))/total(aprofile[i,*]-min(aprofile[i,*]))
            estim_err = sigerr[1]/2/h
            ;print,[polar_arc[0,i]*180/!pi, fitp[1], fitp[2], sigerr[1]/2/h, 0.75/sigerr[1]^2] ;sigma[2], chisq]


            IF estim_err GT 0.01 then BEGIN
                polar_arc[1,i] = -1
                polar_arc[2,i] = -1
                circxy = cv_coord(from_polar=[polar_arc[0,i],iradius],/to_rect)
                ;Plots, (circxy[0]+x0-mask_polygon.xlow)/mask_polygon.zoom $
                ;            , (circxy[1]+y0-mask_polygon.ylow)/mask_polygon.zoom, psym=3,/device
            ENDIF ELSE BEGIN
                polar_arc[1,i] = fitp[1]
                polar_arc[2,i] = sigerr[1]
                circxy = cv_coord(from_polar=[polar_arc[0,i],polar_arc[1,i]],/to_rect)
                ;Plots, (circxy[0]+x0-mask_polygon.xlow)/mask_polygon.zoom $
                ;            , (circxy[1]+y0-mask_polygon.ylow)/mask_polygon.zoom, psym=4,/device
            ENDELSE
        ENDIF ELSE BEGIN
            polar_arc[0,i] = (i+0.5) * anginc + minang
            polar_arc[1,i] = -1
            polar_arc[2,i] = -1
            circxy = cv_coord(from_polar=[polar_arc[0,i],iradius],/to_rect)
            ;Plots, (circxy[0]+x0)/mask_polygon.zoom-mask_polygon.xlow $
            ;            , (circxy[1]+y0)/mask_polygon.zoom-mask_polygon.ylow, psym=3,/device
        ENDELSE
    ENDFOR

    validpts = where(polar_arc[1,*] GT 0)
    ang_temp = Reform(polar_arc[0,validpts])
    r_temp = Reform(polar_arc[1,validpts])
    r_temp_err = Reform(polar_arc[2,validpts])

    polar_arc = Transpose([[ang_temp],[r_temp],[r_temp_err]])

    RETURN, 0
END

PRO AS_SaxsImageTools::Event, event, scatterEvent

  @as_scatterheader.macro

  self->as_CakeObj::event, event, scatterEvent
END

PRO AS_SaxsImageTools::GetProperty, _REF_Extra = extra

  @as_scatterheader.macro

  self.as_cakeobj::GetProperty, _EXTRA = extra

END

PRO AS_SaxsImageTools::NewParams, paramObj, CONFIGNO = configNo
  
  @as_scatterheader.macro

  self->AS_CakeObj::NewParams, paramObj, CONFIGNO = configNo

END

PRO AS_SaxsImageTools__define

tools = {AS_SaxsImageTools_Struc, $
        dummy : ''}

void = {AS_SaxsImageTools, $
        INHERITS as_cakeobj, $
        tools : tools $
        }

END