FUNCTION AS_CakeObj::Init, qData, profileObj, _REF_Extra=extra

  @as_scatterheader.macro

         IF ~Obj_Valid(qData) THEN RETURN, 0
         self.cake.qData = qData
         self.cake.profileObj = profileObj
         self.cake.newmask =       1               ; = 0 if cake setup is ok for current mask
         self.cake.polyn =         3               ; order of polynomial coefs for spatial errors
         self.cake.spatfname =     'no_name_yet'   ; name of file holding spatial correction arrays
         self.cake.step =          2.0             ; increment of radius in pixels
         self.cake.xerror  = Ptr_New(fltarr(1))
         self.cake.yerror  = Ptr_New(fltarr(1))
         ;self.cake.mask    = Ptr_New(intarr(1))
         self.cake.sectors = Ptr_New(fltarr(1))
         self.cake.radial  = Ptr_New(fltarr(1))
         self.cake.count   = Ptr_New(fltarr(1))
         self.cake.seclut  = Ptr_New(intarr(1))
         self.cake.scount  = Ptr_New(intarr(1))
         self.cake.lut     = Ptr_New(intarr(1))
         self.cake.binlut  = Ptr_New(intarr(1))
         
        RETURN, self->AS_MaskObj::Init(_Extra=extra)
    
END

PRO AS_CakeObj::SaveCakeLUT, slotNum

  @as_scatterheader.macro

  IF Ptr_Valid(self.cake.cakeMemory[slotNum].lut)    THEN *(self.cake.cakeMemory[slotNum].lut)    = *(self.cake.lut) $
                                                     ELSE self.cake.cakeMemory[slotNum].lut       = Ptr_New(*(self.cake.lut))
  IF Ptr_Valid(self.cake.cakeMemory[slotNum].radial) THEN *(self.cake.cakeMemory[slotNum].radial) = *(self.cake.radial) $
                                                     ELSE self.cake.cakeMemory[slotNum].radial    = Ptr_New(*(self.cake.radial))
  IF Ptr_Valid(self.cake.cakeMemory[slotNum].count)  THEN *(self.cake.cakeMemory[slotNum].count)  = *(self.cake.count) $
                                                     ELSE self.cake.cakeMemory[slotNum].count     = Ptr_New(*(self.cake.count))                                                                                                    
  
  self.cake.cakeMemory[slotNum].nq         = self.cake.nq
  self.cake.cakeMemory[slotNum].camlength  = self.frame.len
  self.cake.cakeMemory[slotNum].wavelength = self.frame.wlen
  self.cake.cakeMemory[slotNum].xc         = self.frame.xc
  self.cake.cakeMemory[slotNum].yc         = self.frame.yc

  
END

PRO AS_CakeObj::LoadCakeLUT, slotNum

  temp = self.cake.cakeMemory[slotNum]
  
  self.frame.len    = temp.camlength
  self.frame.wlen   = temp.wavelength
  self.frame.xc     = temp.xc
  self.frame.yc     = temp.yc
  
  IF ~Ptr_Valid(temp.lut) OR ~Ptr_New(*(self.cake.radial)) OR ~Ptr_New(*(self.cake.count)) THEN BEGIN
    self.cake.ok = 0
    result = self->CakeSetup()
    self.cake.cakeMemory[slotNum].lut    = Ptr_New(*(self.cake.lut))
    self.cake.cakeMemory[slotNum].radial = Ptr_New(*(self.cake.radial))
    self.cake.cakeMemory[slotNum].count  = Ptr_New(*(self.cake.count))
    self.cake.cakeMemory[slotNum].nq     = temp.nq
  ENDIF ELSE BEGIN
    *(self.cake.lut)    = *temp.lut
    *(self.cake.radial) = *temp.radial
    *(self.cake.count)  = *temp.count
    self.cake.nq        = temp.nq
  ENDELSE
  
END

;*******************************************************************************
;   saxs_cake_setup
;
;   Construct lookup table and q range vector for rapid radial integration of
;   SAXS image into a 1-D profile
;
;   Modification History
;
;   01-Dec-02   created by David J. Cookson
;   01-Jan-03   (DJC) - incorporated addional user-defined masks
;   29-May-03   (DJC) - changed order of mask application (user masks first)
;   15-Nov-03   (DJC) - added spatial correction code
;   24-Aug-04   (DJC) - added the ability to integrate over constant Qy and Qz
;
;*******************************************************************************

FUNCTION AS_CakeObj::CakeSetup, force_range=force_range, sectors=sectors

  @as_scatterheader.macro

;   Note that each time saxs_cake_setup is called a new q_data structure is created
;   in addition, the profiles array is reset (and erased) so that it has an
;   identical number of points for each profile as the q_data structure of vectors.

 
;print, 'saxs_cake_setup : self.cake.type = ',self.cake.type $
;            , '  self.cake.newmask = ',self.cake.newmask $
;            , '  self.cake.ispatial = ',self.cake.ispatial $
;            , '  self.cake.ispatype = ',self.cake.ispatype
;help, *(self.cake.xerror), *(self.cake.yerror)
    nx = self.frame.nxpix
    ny = self.frame.nypix
    ; Call to renamed, AS updated function on next line.
   ; if self.mask.auto[1] EQ 1 THEN self->AS_MaskObj::MakeMaskPolygon, /BEAMSTOP

;   Spatial correction arrays need to be present even if they are just zeros

    if (n_elements((*self.cake.xerror)[*,0]) EQ self.frame.nxpix) AND (n_elements((*self.cake.xerror)[0,*]) EQ self.frame.nxpix) then begin
        xerror = *self.cake.xerror
        yerror = *self.cake.yerror
        tmpxc = self.frame.xc > 0 < (nx-1)
        tmpyc = self.frame.yc > 0 < (ny-1)
        xerror = xerror - xerror[round(tmpxc),round(tmpyc)]
        yerror = yerror - yerror[round(tmpxc),round(tmpyc)]
    endif else begin
        print,'saxs_cake_setup : reset spatial correction arrays'
        xerror = fltarr(self.frame.nxpix,self.frame.nypix)
        yerror = fltarr(self.frame.nxpix,self.frame.nypix)
    endelse

    ;*******************************************************************************
    ; Masking regions are only re-calculated when self.cake.newmask = 1
    ;
;    if self.cake.newmask EQ 1 then begin
;        mask = fltarr(nx,ny)                                    ; initial mask array is zeroed
;        mask[*]=0
;        nmsk = 0
;
;        ; Apply user including masks (masks from #2 onwards)
;        ;
;        for i = 2,n_elements(self.mask.type)-1 do begin
;            if (self.mask.type[i] EQ 1) AND (self.mask.npts[i] GT 2) then begin
;                nmsk = nmsk+1
;                npt = self.mask.npts[i]-1
;                polyvec = polyfillv(self.mask.x[i,0:npt], self.mask.y[i,0:npt],nx,ny)
;
;;                if n_elements(polyvec) LT 3 then begin
;;                    maskstr = ['Border','BeamStop','#1','#2','#3','#4','#5','#6','#7','#8']
;;                    retmes = dialog_message('Problem with '+maskstr(i)+' masking polygon')
;;                    return,0
;;                endif
;
;                mask[polyvec]=1
;            endif
;        endfor
;
;        ; If no user including masks were applied - include all pixels in frame
;        if nmsk EQ 0 then mask[*]=1
;
;        ; Create and apply outer frame exclusion area from polygon mask #0 (if #0 is active)
;        if self.mask.type[0] EQ 1 then begin
;            tempmask = replicate(0,nx,ny)
;            npt = self.mask.npts[0]-1
;            tempmask[polyfillv(self.mask.x[0,0:npt], self.mask.y[0,0:npt],nx,ny)] = 1
;            mask = mask * tempmask
;        endif
;
;        ; Now apply user excluding masks from #1 (the beam stop mask) onwards
;        ;
;        for i = 1,n_elements(self.mask.type)-1 do begin      ; apply user excluding masks
;            if (self.mask.type[i] EQ 2) AND (self.mask.npts[i] GT 2) then begin
;                npt = self.mask.npts[i]-1
;                polyvec = polyfillv(self.mask.x[i,0:npt], self.mask.y[i,0:npt],nx,ny)
;
;                if n_elements(polyvec) LT 3 then begin
;                    maskstr = ['Border','BeamStop','#1','#2','#3','#4','#5','#6','#7','#8']
;                    retmes = dialog_message('Problem with '+maskstr(i)+' masking polygon')
;                    return,0
;                endif
;
;                mask[polyvec]=0
;            endif
;        endfor
;
;    endif else mask = *self.cake.mask

    ;*******************************************************************************
    ; Count pixels of equal radii -> self.frame.count vector
    ; Construct constant-radius lookup table (LUT) -> self.frame.lut

    ; kludge problem with histogram reverse lookup table
    ; this ensures every bin of the histogram gets at least
    ; one count - otherwise the lookup table LUT does not
    ; work properly later on in saxs_cake
    ; if abs(self.frame.yc) LT ny then mask[*,self.frame.yc]=1

    ;******************************************************************
    ; Only recalc radial array if a vital parameter has changed or we are
    ; calculating angle sectors.  Do not recalc radial array if only the
    ; masking array has been changed

    if (self.cake.ok EQ 0) OR keyword_set(sectors) then begin
    ;print, 'saxs_cake_setup : new radial table'
        a1 = fltarr(nx,ny)
        a2 = fltarr(nx,ny)
        x = fltarr(nx,ny)
        y = fltarr(nx,ny)
        for i = 0,nx-1 do a2[i,*] = findgen(ny)   ; fill array with ascending row integers
        for i = 0,ny-1 do a1[*,i] = findgen(nx)   ; fill array with ascending column integers
        
        if (self.cake.ispatial EQ 1) and (self.cake.ispatype EQ 0) then begin
    ;print, 'Applying cartesian (x,y) spatial correction'
    ;help, self.cake.xerror, self.cake.yerror
    ;print,'min/max xerror = ',min(self.cake.xerror),max(self.cake.xerror)
    ;print,'min/max yerror = ',min(self.cake.yerror),max(self.cake.yerror)
            a1 = a1 + xerror
            a2 = a2 + yerror
        endif
        a1 = a1 - self.frame.xc
        a2 = a2 - self.frame.yc
        radial = sqrt(a1*a1 + a2*a2)
        radDetAngle = !dpi*self.frame.detAngle/180. 
        ;IF self.frame.detAngle NE 0 THEN radial = (self.frame.len/self.frame.psize)*Tan(ACos((self.frame.len*Cos(radDetAngle)-a2*self.frame.psize*Sin(radDetAngle))/SqRt((a1*self.frame.psize)^2+self.frame.len^2+(a2*self.frame.psize)^2)))
        IF self.frame.detAngle NE 0 THEN radial = (self.frame.len/self.frame.psize)*Tan(ACos((self.frame.len*Cos(radDetAngle)+a1*self.frame.psize*Sin(radDetAngle))/SqRt((a2*self.frame.psize)^2+self.frame.len^2+(a1*self.frame.psize)^2)))
        IF self.frame.detAngle NE 0 AND abs(self.frame.detOffsetH) + abs(self.frame.detOffsetV) NE 0 THEN $
            radial = (self.frame.len/self.frame.psize)*Tan(ACos((self.frame.detOffsetH + self.frame.len*Cos(radDetAngle)+a1*self.frame.psize*Sin(radDetAngle))/SqRt((a2*self.frame.psize)^2+self.frame.len^2+(a1*self.frame.psize)^2+(self.frame.detOffsetH*(self.frame.detOffsetH+2*self.frame.len*cos(radDetAngle)+2*a1*self.frame.psize*sin(radDetAngle))+self.frame.detOffsetV*(self.frame.detOffsetV+2*self.frame.len*sin(radDetAngle)-2*a1*cos(radDetAngle))))))
                    
        if (self.cake.ispatial EQ 1) and (self.cake.ispatype EQ 1) then begin
            print, 'Applying polar (theta,radius) spatial correction'
            radial = radial + yerror
        endif

        if self.cake.type GT 0 then begin
            chi = 0;atan((self.frame.xcr-self.frame.xc)/(self.frame.ycr-self.frame.yc))
            print, 'GSAXS chi = ', chi*180/3.1416,' deg'

            theta = atan(a1,a2) - chi
            a1 = radial * cos(theta)
            a2 = radial * sin(theta)

            if self.cake.type EQ 2 then begin            ; In-plane scattering
                radial = abs(a2)
            endif else begin                            ; Out of plane scattering
                radial = abs(a1)
            endelse
        endif
        maxpr = round(max(radial,MIN=minpr))
        
        self.mask.beamStop.GetProperty, radius = beamStopRadius
        
        overhang = sqrt(((abs(self.frame.xc-self.frame.nxpix/2)-self.frame.nxpix/2)>0)^2 $
                        + ((abs(self.frame.yc-self.frame.nypix/2)-self.frame.nypix/2)>0)^2)
        
        overhang = round(overhang > (beamStopRadius/self.frame.psize/(self.cake.step>1)))

        if self.cake.step LT 1 then begin
        ; Create radial array that will be binned psuedo-logarithmically
            binlut = intarr(maxpr+1)
            binlut[0:overhang] = findgen(overhang+1)
            starti = overhang+1
            for index = overhang+1,maxpr do begin
                endi = starti + round((index/40) ^1.8)
                if (endi GT maxpr) OR (starti GT maxpr) then break
                binlut[starti:endi] = index
                starti = endi+1
            endfor
            if starti LE maxpr then binlut[starti:maxpr] = index
            radial = binlut[round(radial)]
        endif else binlut = 0
    endif else begin
        minpr = self.cake.minpr
        maxpr = self.cake.maxpr
        binlut = *self.cake.binlut
        overhang = self.cake.overhang
        radial = *self.cake.radial                      ; use previous radial array
    endelse

    ;*******************************************************************************
    ; Construct a sector look up table if keyword SECTORS is set
    ;
    if keyword_set(sectors) then begin
;print, 'saxs_cake_setup : Calculating Sector LUT'
        sectors = atan(a2,a1)
        nsecs = self.cake.nsectors
        anginc = 2 * !pi / float(nsecs)
;print, 'saxs_cake_setup : nbins, binsize = ', nsecs, anginc
        scount = histogram(sectors, nbins=nsecs, min=-!pi, binsize=anginc, reverse_indices=seclut)
        nsectors = n_elements(scount)
    endif else begin
        nsectors = self.cake.nsectors
        sectors = *self.cake.sectors
        scount = *self.cake.scount
        seclut = *self.cake.seclut
    endelse

    ;*******************************************************************************
    ; Construct radial look up table - this is always done every time that
    ; saxs_cake_setup is called - just to be safe!

    step = self.cake.step > 1
    rcount = histogram(radial * *self.mask.mask, binsize=step $        ; bin equal-radius pixels
                , min=0, omax=rmax, reverse_indices=rlookup); make constant-radius LUT
    
    ; Delete bins with zero pixels, and the zeroth bin
    rcount[0] = 0
    zeroPixBins = Where(rcount EQ 0, numZeroPixBins, COMPLEMENT = nonZeroPixBins)
    rcountOrig = rcount
    rcount = rcount[nonZeroPixBins]
    rlookupTemp = rlookup[[nonZeroPixBins,N_Elements(rcountOrig)]] - numZeroPixBins
    rlookup = [rlookupTemp,rlookup[rlookup[0]:*]]
    
    self.cake.lutnonzerobins = Ptr_New(nonZeroPixBins)
    
    ;
    ;
    ;*******************************************************************************
    ; Using frame information   - replace q-value vector if new q range is bigger
    ;                           - modify or destroy profile array
    ;
    if self.cake.step LT 1 then begin
        temp = findgen(maxpr)
        binlutsub = binlut[where(binlut LE rmax)]
        pcount = histogram(binlutsub, binsize=1, min=0,omax=rmax, reverse_indices=plut)
        p_temp = fltarr(n_elements(pcount))
        for i = 0,n_elements(pcount)-1 do begin
            if pcount[i] GT 0 then p_temp[i] = total(temp[plut[plut[i]:plut[i+1]-1]])/pcount[i]
        endfor
        p_temp = p_temp[nonZeroPixBins]
    endif else p_temp = ((dindgen(n_elements(rcountOrig)))[nonZeroPixBins] + 0.5) * self.cake.step

    tthrad_temp = atan(p_temp * self.frame.psize / self.frame.len)
    tth_temp = 180.0 / 3.14159 * tthrad_temp
    q_temp = 4 * 3.14159 / self.frame.wlen * sin(tthrad_temp/2)
    d_temp = 2 * 3.14159 / (q_temp > 0.00001)

    ; calculate binning factors (eg pixels, degrees, A-1 and A per pixel)
    p_bin = 1.0
    aa = self.frame.psize*(self.cake.step>1)/self.frame.len
    aasq = aa^2
    tth_bin = (aasq + 1)/(aasq * p_temp^2 + 1)
    q_bin = cos(tthrad_temp/2) * tth_bin
    dmin = self.frame.wlen / aa
    d_bin = (d_temp / dmin)^2 * q_bin * 1000

    nq = n_elements(q_temp)                             ; check size of new q_data array
;   old_maxnq = n_elements(profiles[0,*])               ; check size of existing profile array
;
;   if self.cake.ok EQ 1 then begin                      ; If we only changed mask no need
;        if old_maxnq LT nq then begin                   ; to destroy existing profile data
;            new_profiles = dblarr(22,nq)                ; but we may need to expand profile
;            new_profiles[*,0:old_maxnq-1] = profiles    ; array to accomodate larger number
;            profiles = new_profiles                     ; of profile points that the
;        endif                                           ; expanded q vector is now using.
;    endif else begin                                    ; If current q-data is not ok then
;        profiles = dblarr(22,nq)                        ; this scrubs all the resident data
;    endelse

    tempqmns = self.cake.qData->GetProperty(/qmns)
    tempqmxs = self.cake.qData->GetProperty(/qmxs)

    
           
      ;*** Definition here disagrees with definition in saxs_init haven't included in call to qData object ******
      ;     bkgnd:  fltarr(nq),  $ ; Create array space for background profile
        

    result = self.cake.qData->SetProperty(p_arr=p_temp, tth_arr=tth_temp, q_arr=q_temp, d_arr=d_temp, pbin=p_bin , $
                                          tthbin=tth_bin, qbin = q_bin, dbin = d_bin, qmns = tempqmns, qmxs = tempqmxs)

    ;profdata.qmin = q_data.q[overhang]
    ;profdata.qmax = max(q_data.q)
    self.frame.qimin = q_temp[overhang]
    self.frame.qimax = max(q_temp)

    ;*******************************************************************************
    ; Now remake the cakedata data structure - this will be used in saxs_cake function
    ;
    ispatial = self.cake.ispatial
    ispatype = self.cake.ispatype
    polyn = self.cake.polyn
    kx = self.cake.kx
    ky = self.cake.ky
    type = self.cake.type
    nsectors = self.cake.nsectors
    spatfname=self.cake.spatfname
    step = self.cake.step

    
            self.cake.ok =         1         
            self.cake.type =       type
            self.cake.newmask =    0         
            self.cake.sectmask =   0   
            self.cake.ispatial =   ispatial  
            self.cake.ispatype =   ispatype
            self.cake.polyn =      polyn
            self.cake.kx =         kx        
            self.cake.ky =         ky        
            *(self.cake.xerror) =  xerror    
            *(self.cake.yerror) =  yerror    
            self.cake.spatfname =  spatfname 
            ;*self.cake.mask =       mask      
            self.cake.nsectors =   nsectors  
            *self.cake.sectors =    sectors   
            *self.cake.seclut =     seclut    
            *self.cake.scount =     scount    
            *self.cake.radial =     radial    
            *self.cake.lut =        rlookup   
            *self.cake.count =      rcount    
            self.cake.maxpr =      maxpr     
            self.cake.minpr =      minpr     
            *self.cake.binlut =     binlut   
            self.cake.overhang =   overhang  
            self.cake.nq =         nq        
            self.cake.step =       step        
        

;   If detector tilt is non-zero and tilt correction is enabled, generate the
;   appropriate correction array in self.cake.yerror

    if (self.cake.ispatial EQ 1) and (self.frame.tilt NE 0) then begin
        self.cake.ispatype = 1
        saxs_tilt_correct, 64
    endif

;    ; Correctly bin frame offset value into a correctly-binned background profile
;    if saxs_cake(/bkgnd) NE nq then retmes = dialog_message('cake_setup : problem with background')

;print,'saxs_cake_setup : profdata.qmax = ',  profdata.qmax
    if nq LE 1 then self.cake.ok = 0         ; last chance to invalidate cake setup
    return, nq                              ; return the total no of points per profile

END

;*******************************************************************************
;   saxs_cake
;
;   Using the lookup table "self.cake.lut" do a radial integration of the pixels
;   in a frame
;
;   01-Mar-03   created David J. Cookson
;   10-Jun-03   modified (DJC) to allow different profile q ranges
;               Assuming that the beam center and camera length are the
;               same we can now compare profiles integrated around different
;               sectors of a particular image on the same plot.  This means
;               that every profile needs to have an associated NPTS value.
;
;*******************************************************************************

FUNCTION AS_CakeObj::Cake, isec=isec;, NOPROFILE = noProfile, LIVE = live

  @as_scatterheader.macro

    IF N_Elements(noProfile) EQ 0 THEN noProfile = 0

    nq = self.cake.nq
;    cur = profdata.current
    q_arr = self.cake.qData->GetProperty(/Q_ARR)
    maxnq = n_elements(q_arr)
    profile=dblarr(maxnq)
    error=dblarr(maxnq)
    step = self.cake.step > 1
    self.cake.profileObj->GetProperty,MEDMEAN=medmean
    if n_elements(isec) GT 0 then begin

        if (*self.cake.scount)[isec] GT (step * 2) then begin
             radial = *self.mask.mask * *self.cake.radial
             subsect = radial[(*self.cake.seclut)[(*self.cake.seclut)[isec]:(*self.cake.seclut)[isec+1]-1]]
             frmsect = (*self.frame.rawData)[(*self.cake.seclut)[(*self.cake.seclut)[isec]:(*self.cake.seclut)[isec+1]-1]]
             sscount = histogram(subsect, binsize=step $            ; bin equal-radius pixels
                        , min=0, omax=rmax, reverse_indices=ssrlut) ; make constant-radius LUT
                    
             truncatedNonZero = (*self.cake.lutnonzerobins)[Where(*self.cake.lutnonzerobins LT ssrlut[0]-1,/NULL)]
             IF truncatedNonZero EQ !NULL THEN RETURN, { q_arr : q_arr, profile : profile, error : error }
             ssrlutTemp = ssrlut[[truncatedNonZero,N_Elements(sscount)]] - (ssrlut[0]-N_Elements(truncatedNonZero))
             ssrlut = [ssrlutTemp,ssrlut[ssrlut[0]:*]]
             sscount[0] = 0
             sscount = sscount[truncatedNonZero]
             nq = n_elements(sscount)
        endif else begin
             radial = *self.mask.mask * *self.cake.radial
             subsect = [0,1,2]
             frmsect = [0,0,0]
             sscount = histogram(subsect, binsize=step $        ; bin equal-radius pixels
                        , min=0, omax=rmax, reverse_indices=ssrlut); make constant-radius LUT
             truncatedNonZero = (*self.cake.lutnonzerobins)[Where(*self.cake.lutnonzerobins LT ssrlut[0]-1,/NULL)]
             IF truncatedNonZero EQ !NULL THEN RETURN, { q_arr : q_arr, profile : profile, error : error }
             ssrlutTemp = ssrlut[[truncatedNonZero,N_Elements(sscount)]] - (ssrlut[0]-N_Elements(truncatedNonZero))
             ssrlut = [ssrlutTemp,ssrlut[ssrlut[0]:*]]
             sscount[0] = 0
             sscount = sscount[truncatedNonZero]
             nq = n_elements(sscount)
        endelse

        ; profdata.medmean determines what type of intensity value is picked out of the
        ; radial range of pixel values.
        ; 0 = regular mean value,  1 = median value,  2 = minimum value
        
        case medmean of

            0 : begin
                for i = 0,nq - 1 do begin
                    if sscount[i] GT (step * 2) then begin  ; if enough pixels are included
                        profile[i] = total(frmsect[ssrlut[ssrlut[i]:ssrlut[i+1]-1]]) / sscount[i]
                        error[i] = 2 * stddev(frmsect[ssrlut[ssrlut[i]:ssrlut[i+1]-1]]) $
                                                / sqrt(sscount[i])
                    endif else begin
                        ;profile = profdata.offset                 ; if not enough pixels
                        ;profiles[cur+11,i] = 1
                    endelse
                endfor
            end

            1 : begin
                for i = 0,nq - 1 do begin
                    if sscount[i] GT (step * 2) then begin  ; if enough pixels are included
                        profile[i] = median(frmsect[ssrlut[ssrlut[i]:ssrlut[i+1]-1]])
                        error[i] = 2 * stddev(frmsect[ssrlut[ssrlut[i]:ssrlut[i+1]-1]]) $
                                                / sqrt(sscount[i])
                    endif else begin
                        ;profiles[cur,i] = profdata.offset                 ; if not enough pixels
                        ;profiles[cur+11,i] = 1
                    endelse
                endfor
            end

            2 : begin
                for i = 0,nq - 1 do begin
                    if sscount[i] GT (step * 2) then begin  ; if enough pixels are included
                        temp = frmsect[ssrlut[ssrlut[i]:ssrlut[i+1]-1]]
                        temp = temp[sort(temp)]
                        temp = temp[round(0.01*sscount[i]):*]
                        profile[i] = min(temp)
                        error[i] = 2 * stddev(frmsect[ssrlut[ssrlut[i]:ssrlut[i+1]-1]]) $
                                             / sqrt(sscount[i])
                    endif else begin
                        ;profiles[cur,i] = profdata.offset                 ; if not enough pixels
                        ;profiles[cur+11,i] = 1
                    endelse
                endfor
            end
        endcase

        if nq LT maxnq then begin
            profile[nq:maxnq-1] =0                 ; pad unused profile array space with zeros
            error[nq:maxnq-1] =0
        endif
        RETURN, { q_arr : q_arr, profile : profile, error : error }
    endif


    case medmean of

        0: begin
            for i = 0,nq - 1 do begin
                if (*self.cake.count)[i] GT (step * 2) then begin  ; if enough pixels are included
                                                               ; this relates to histogram kludge
                     profile[i] = total((*self.frame.rawData)[(*self.cake.lut)[(*self.cake.lut)[i]:(*self.cake.lut)[i+1]-1]])/(*self.cake.count)[i]
                    error[i] = 2 * stddev((*self.frame.rawData)[(*self.cake.lut)[(*self.cake.lut)[i]:(*self.cake.lut)[i+1]-1]]) / sqrt((*self.cake.count)[i])
                endif else begin
                    ;profiles[cur,i] = profdata.offset       ; if not enough pixels
                    ;profiles[cur+11,i] = 1
                endelse
            endfor
        end

        1: begin
            for i = 0,nq - 1 do begin
                if (*self.cake.count)[i] GT (step * 2) then begin  ; if enough pixels are included
                                                                        ; this relates to histogram kludge
                    profile[i] = median((*self.frame.rawData)[(*self.cake.lut)[(*self.cake.lut)[i]:(*self.cake.lut)[i+1]-1]])
                    error[i] = 2 * stddev((*self.frame.rawData)[(*self.cake.lut)[(*self.cake.lut)[i]:(*self.cake.lut)[i+1]-1]]) / sqrt((*self.cake.count)[i])
                endif else begin
                    ;profiles[cur,i] = profdata.offset       ; if not enough pixels
                    ;profiles[cur+11,i] = 1
                endelse
            endfor
        end

        2: begin
            for i = 0,nq - 1 do begin
                if (*self.cake.count)[i] GT (step * 2) then begin  ; if enough pixels are included
                                                               ; this relates to histogram kludge
                    temp = (*self.frame.rawData)[(*self.cake.lut)[(*self.cake.lut)[i]:(*self.cake.lut)[i+1]-1]]
                    temp = temp[sort(temp)]
                    temp = temp[round(0.1* (*self.cake.count)[i]):*]
                    profile[i] = min(temp)
                    error[i] = 2 * stddev((*self.frame.rawData)[(*self.cake.lut)[(*self.cake.lut)[i]:(*self.cake.lut)[i+1]-1]]) / sqrt((*self.cake.count)[i])
                endif else begin
                    profile[i] = profdata.offset       ; if not enough pixels
                    error[i] = 1
                endelse
            endfor
        end
    endcase

    if nq LT maxnq then begin
        profile[nq:maxnq-1] =0                 ; pad unused profile array space with zeros
        error[nq:maxnq-1] =0
    endif
    
    ;IF noProfile NE 1 THEN self.AddProfile, q_arr, profile, error, self.frame.fname, PIXEL = self.cake.qData.GetProperty(/P_ARR), LIVE=live, TIME = self.frame.time, I0COUNT = self.frame.i0sf, IBSCOUNT = self.frame.ibssf
    ;return, nq
    RETURN, { q_arr : q_arr, profile : profile, error : error }
    
END

;PRO AS_CakeObj::AddProfile, q_arr, data, error, fname, _REF_EXTRA = extra
;
;  self.cake.profileObj->AddProfile, q_arr, data, error, fname, _EXTRA = extra
;
;END

PRO AS_CakeObj::SetProperty, STEP = step, _REF_Extra = extra

  @as_scatterheader.macro

  IF N_Elements(step) GT 0 THEN BEGIN
    IF step GE 0 THEN self.cake.step = step
    self.cake.ok = 0
  ENDIF
  
  self.AS_MaskObj::SetProperty, _EXTRA = extra

END

PRO AS_CakeObj::GetProperty, STEP = step, _REF_Extra = extra

  @as_scatterheader.macro

  IF Arg_Present(step) THEN step = self.cake.step
  self.AS_MaskObj::GetProperty, _EXTRA = extra

END

FUNCTION AS_CakeObj::Sectors, f_name, sectors, NODISPLAYMASK=noDisplayMask, CURRENTIMAGE = currentImage

  @as_scatterheader.macro

  self.cake.nsectors = sectors

  IF ~KeyWord_Set(noDisplayMask) THEN self.NewMask, MASKOBJECT=maskObject
  
  IF ~KeyWord_Set(currentImage) THEN sf=self->GetImage(f_name)
  profileData = List()
  result = self->CakeSetup(/SECTORS)
  
  FOR i =0, sectors-1 DO BEGIN
    IF ~KeyWord_Set(noDisplayMask) THEN BEGIN
      maskObject.SetProperty, MASKTYPE=2, MASKSHAPE=1, CENTREX=self.frame.xc, CENTREY=self.frame.yc, RADIUSMAX=SqRt(Long(self.frame.nxpix)^2+Long(self.frame.nypix)^2), RADIUSMIN=0, ANGLEMAX=i*(360./sectors)+(360./sectors)-180., ANGLEMIN=i*(360./sectors)-180., NAME = 'Sector', COLOUR = [255,168,0]
      self.PolyChanged, /SET
    ENDIF
    profileData.Add, self->Cake(isec=i)
  ENDFOR
  
  IF ~KeyWord_Set(noDisplayMask) THEN BEGIN
    Obj_Destroy, maskObject
    self.PolyChanged, /SET
  ENDIF

  RETURN, profileData

END

FUNCTION AS_CakeObj::GetAndCake, f_name, FRAME = liveFrame, NOPROFILE = noProfile, SAVESUMMED=saveSummed, SUMMEDNAME = summedName, NOSETUP = noSetup

    @as_scatterheader.macro

    self.UpdateBeamCursor
    live = KeyWord_Set(liveFrame)
    sf=self->GetImage(f_name, FRAME=liveFrame)
    IF sf LT 0 THEN RETURN, -1
    IF ~KeyWord_Set(noSetup) OR self.cake.ok EQ 0 OR self.cake.newmask EQ 1 THEN setup_pts = self->CakeSetup()
    
    IF KeyWord_Set(saveSummed) AND N_Elements(f_name) GT 1 THEN BEGIN
      IF ~KeyWord_Set(summedName) THEN summedName = ''
      IF summedName EQ '' THEN summedName = Dialog_Pickfile(/WRITE, /OVERWRITE_PROMPT, FILTER = '*.tif', PATH = self.frame.path ,GET_PATH=path, DEFAULT_EXTENSION='tif')
      IF summedName NE 'Cancel' AND summedName NE '' THEN BEGIN
        self.frame.fname = StrMid(summedName,StrLen(path)) 
        Write_Tiff, summedName, Reverse(*self.frame.rawData,2), /LONG, /SIGNED, DESCRIPTION='Summed frame created by scatterBrain using the following files: ' + StrJoin(f_name.toarray(),' ',/SINGLE)
        self.frame.logobj.NewLogLine, summedName, self.frame.time, self.frame.i0counts, 0, self.frame.iBScounts, TYPE = 'SUMMED'
      ENDIF ELSE BEGIN
        result = Dialog_Message('Invalid filename, not saving summed image, you will not be able to use it for background subtractions, etc.')
      ENDELSE
    ENDIF

    profileData = self->cake()
    
    IF N_Elements(profileData.profile) LE 1 THEN RETURN, -1
    
    RETURN, profileData

END 

FUNCTION AS_CakeObj::GetLUT

  @as_scatterheader.macro

;  nxpix = long(self.frame.nxpix)
;  nypix = long(self.frame.nypix)
;
;  conv = reform(transpose(reform(findgen(nxpix*nypix),nxpix,nypix)),nxpix*nypix)
;  tempLut = *self.cake.lut
;  numBins = tempLut[0]
;  FOR i=0,nxpix*nypix-1 DO BEGIN
;    PRINT, I
;    tempLut[numBins+i] = Where((conv EQ *self.cake.lut)[numBins+i])
;    RESULT = widget_event(/nowait)
;  ENDFOR


  IF ~Ptr_Valid(self.cake.lut) OR ~Ptr_Valid(self.cake.radial) THEN RETURN, -1
  rcount = histogram(reverse(*self.cake.radial * *self.mask.mask,2), binsize=self.cake.step $        ; bin equal-radius pixels
                , min=0, omax=rmax, reverse_indices=rlookup); make constant-radius LUT
  
  ; Delete bins with zero pixels, and the zeroth bin
  rcount[0] = 0
  zeroPixBins = Where(rcount EQ 0, numZeroPixBins, COMPLEMENT = nonZeroPixBins)
  rcountOrig = rcount
  rcount = rcount[nonZeroPixBins]
  rlookupTemp = rlookup[[nonZeroPixBins,N_Elements(rcountOrig)]] - numZeroPixBins
  rlookup = [rlookupTemp,rlookup[rlookup[0]:*]]
  
  RETURN, rlookup
  
END

PRO AS_CakeObj::Event, event

  @as_scatterheader.macro
  self->AS_MaskObj::event, event
END

PRO AS_CakeObj::NewParams, paramObj 

  @as_scatterheader.macro

  self->AS_MaskObj::NewParams, paramObj

  cake = 1

  paramObj->GetParameters, CAKE=cake
  
  cakeNames = Tag_Names(cake)
  
  FOR i = 0, N_Elements(cakeNames) - 1 DO BEGIN
  
    matchTag = Where(Tag_Names(self.cake) EQ cakeNames[i])
    IF matchTag GE 0 THEN BEGIN 
      
      IF Size(cake.(i),/TYPE) EQ 10 AND Size(self.cake.(matchTag),/TYPE) EQ 10 THEN BEGIN
          Ptr_Free, self.cake.(matchTag)
          self.cake.(matchTag) = cake.(i)
      ENDIF 
      IF Size(cake.(i),/TYPE) EQ 10 AND Size(self.cake.(matchTag),/TYPE) NE 10 THEN BEGIN
         self.cake.(matchTag) = *(cake.(i))
      ENDIF 
      IF Size(cake.(i),/TYPE) NE 10 AND Size(self.cake.(matchTag),/TYPE) EQ 10 THEN BEGIN
         *(self.cake.(matchTag)) = cake.(i)
      ENDIF 
      IF Size(cake.(i),/TYPE) NE 10 AND Size(self.cake.(matchTag),/TYPE) NE 10 THEN self.cake.(matchTag) = cake.(i)
              
    ENDIF
   
  ENDFOR

END

PRO AS_CakeObj::qClick, x, y
     
  @as_scatterheader.macro
     
  x = x - self.frame.xc
  y = y - self.frame.yc
  
  IF self.frame.detAngle NE 0 THEN BEGIN
    radDetAngle = !dpi*self.frame.detAngle/180.
    radius = (self.frame.len/self.frame.psize)*Tan(ACos((self.frame.len*Cos(radDetAngle)+x*self.frame.psize*Sin(radDetAngle))/SqRt((y*self.frame.psize)^2+self.frame.len^2+(x*self.frame.psize)^2)))
  ENDIF ELSE BEGIN
    radius = SqRt(x^2 + y^2)
  ENDELSE

  tthrad = atan(radius * self.frame.psize / self.frame.len)
  tth = 180.0 / 3.14159 * tthrad
  q = 4 * 3.14159 * sin(tthrad/2) / self.frame.wlen
  d = 2 * 3.14159 / (q > 0.00001)

  
  expo = 10.0d^(3 -1 - floor(alog10(abs(q))))
  q = long(q*expo)/expo

  self->OverLay_QCirc, q
  self.cake.profileObj->AddQMarker, q
  

END

PRO AS_CakeObj::PowerLawScale, power

  @as_scatterheader.macro

  IF N_Elements(power) EQ 0 THEN power = -4

  self.cake.power = power

  IF power EQ 0 THEN BEGIN 
    *self.frame.histImage = *self.frame.rawData
    self.frame.histObj->UpdateHisto
  ENDIF 

  a1 = fltarr(self.frame.nxpix,self.frame.nypix)
  a2 = fltarr(self.frame.nxpix,self.frame.nypix)
  
  for i = 0,self.frame.nxpix-1 do a2[i,*] = findgen(self.frame.nypix)   ; fill array with ascending row integers
  for i = 0,self.frame.nypix-1 do a1[*,i] = findgen(self.frame.nxpix)   ; fill array with ascending column integers

  a1 = a1 - self.frame.xc
  a2 = a2 - self.frame.yc

  IF self.frame.detAngle NE 0 THEN BEGIN
    radDetAngle = !dpi*self.frame.detAngle/180.
    radius = (self.frame.len/self.frame.psize)*Tan(ACos((self.frame.len*Cos(radDetAngle)+a1*self.frame.psize*Sin(radDetAngle))/SqRt((a2*self.frame.psize)^2+self.frame.len^2+(a1*self.frame.psize)^2)))
  ENDIF ELSE BEGIN
    radius = SqRt(a1^2 + a2^2)
  ENDELSE

  tthrad = atan(radius * self.frame.psize / self.frame.len)
  tth = 180.0 / 3.14159 * tthrad
  q = 4 * 3.14159 * sin(tthrad/2) / self.frame.wlen
  d = 2 * 3.14159 / (q > 0.00001)
  
  *self.frame.histImage = *self.frame.rawData / q^power 
  self.frame.histObj->UpdateHisto
      
END


PRO AS_CakeObj__Define

void = {AS_CakeMemory, lut: Ptr_New(/ALLOCATE_HEAP), nq: 0, count: Ptr_New(/ALLOCATE_HEAP), radial: Ptr_New(/ALLOCATE_HEAP), xc : 0.0, yc : 0.0, wavelength : 0.0, camlength : 0.0}

cake = { AS_CakeObj_Struc, $   ; CURRENT CAKE INTEGRATION LUT
            ok:            0,              $ ; = 0 when saxs_cake_setup  needs to be run
            type:          0,              $ ; 0=SAXS, 1=GISAXS z-axis, 2=GISAXS x-axis
            newmask:       1,              $ ; = 0 if cake setup is ok for current mask
            sectmask:      0,              $ ; = 0 if cake setup is ok for current sector
            ispatial:      0,              $ ; 0=no spatial,1=apply spatial
            ispatype:      0,              $ ; 0=cartesian (x,y), 1=polar (th,r)
            polyn:         3,              $ ; order of polynomial coefs for spatial errors
            kx:            fltarr(4,4),    $ ; polynomial coefs for x-spatial errors
            ky:            fltarr(4,4),    $ ; polynomial coefs for y-spatial errors
            xerror:        Ptr_New(/ALLOCATE_HEAP),      $ ; array of x-spatial errors (radial error)
            yerror:        Ptr_New(/ALLOCATE_HEAP),      $ ; array of y-spatial errors
            spatfname:     'no_name_yet',  $ ; name of file holding spatial correction arrays
            ;mask:          Ptr_New(/ALLOCATE_HEAP),      $ ; mask array preserved between cake setups
            nsectors:      0,              $ ; number of sector angle bins
            sectors:       Ptr_New(/ALLOCATE_HEAP),      $ ; array of polar angles
            seclut:        Ptr_New(/ALLOCATE_HEAP),      $ ; look up table for pixels at equal angles
            scount:        Ptr_New(/ALLOCATE_HEAP),      $ ; holds new vector of equal-angle pixel counts
            lutnonzerobins:Ptr_New(/ALLOCATE_HEAP),      $
            radial:        Ptr_New(/ALLOCATE_HEAP),      $ ; radial distance array
            lut:           Ptr_New(/ALLOCATE_HEAP),      $ ; look up table (LUT) for radially-sorted pixels
            count:         Ptr_New(/ALLOCATE_HEAP),      $ ; number of pixels contributing to a radial sum
            maxpr:         0,              $ ; maximum pixel radius/distance
            minpr:         0,              $ ; minimum pixel radius/distance
            binlut:        Ptr_New(/ALLOCATE_HEAP),      $ ; look up table for logarithmic q-binning
            overhang:      0,              $ ; minimum radial distance for actual intensities
            nq:            0,              $ ; number of q values in integrated profile
            step:          2.0,            $ ; increment of radius in pixels
            qData:         Obj_New(/ALLOCATE_HEAP),      $ ; object holding qdata info.
            profileObj:    Obj_New(/ALLOCATE_HEAP),      $ ; object holding profiles.
            power:         0.0, $
            cakeMemory:    Replicate(void,10)$
            
      }

void = {AS_CakeObj,  $
        INHERITS AS_MaskObj,   $
        cake : cake $
        }

END
