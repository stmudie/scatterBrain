FUNCTION AS_MaskPolygon::Init

END

PRO AS_MaskPolygon::MakeMaskPolygon, FRAMEBORDER=frameborder, BEAMSTOP=beamstop

;*************************************************************************
; Copyright (c) 2008 Australian Synchrotron
;*************************************************************************
;+
; NAME:
; AS__SaxsMakeMaskPolygon
;
; PURPOSE:
; This procedure constructs masking polygons - these are used by saxs_cake_setup to make make
; the mask array used for all profile integrations
;
; CATEGORY:
; SAXS Detector Masks
;
; CALLING SEQUENCE:
;
; AS__SaxsMakeMaskPolygon, frmdata, mask_polygon
;
; INPUTS:
; fmrdata:  Structure containing information about the frame.
; mask_polygon: Structure containing information about the masks.
;
; KEYWORD PARAMETERS:
; FRAMEBORDER: Make frame mask. 
; BEAMSTOP: Make beamstop mask 
;
; OUTPUTS:
; Passed structures updated with new mask parameters.
;
; SIDE EFFECTS:
; Passed structures updated with new mask parameters.
;
; MODIFICATION HISTORY:
;   Written by: David Cookson December 2002
;   01-Dec-02  created David J. Cookson
;   01-Jan-03  DJC - added the masking for image frames from Wire Detector
;   27-Nov-03  DJC - bug fix for SMART frames with 512x512 pixels
;   15-May-08  STM - Updated to AS convention. Removed common blocks - structures passed as variables.
;-

    xc = self.xc
    yc = self.yc
    xbsr = self.xbsr
    ybsr = self.ybsr
    nx = self.nxpix
    ny = self.nypix

    IF KEYWORD_SET(frameborder) THEN BEGIN
        ; Polygon 0 - Outer frame border including - this is applied first
        ; Polygon 1 - Excluding mask for beam stop and contents - this is applied last

        CASE self.type OF

            'SMART': BEGIN
                self.npts[0] = 10
                self.x[0,0:9] = [30,500,950,991,993,1010,500,44,39,30] / 1024. * nx
                self.y[0,0:9] = [984,986,995,950,500,34,34,27,500,984] / 1024. * ny
            END

            'MAR165' : BEGIN
                self.npts[0] = N_ELEMENTS(self.x[0,*])
                radius = MIN([nx,ny])/2 - 1
                cx = nx/2 -1
                cy = ny/2 -1
                th = FINDGEN(self.npts[0])/(self.npts[0]-1) * !PI * 2
                cirpol = TRANSPOSE([[th],[REPLICATE(radius, self.npts[0])]])
                cirxy = CV_COORD(FROM_POLAR=cirpol,/to_rect)
                self.x[0,*] = cirxy[0,*] + cx
                self.y[0,*] = cirxy[1,*] + cy
            END

            'MAR345' : BEGIN
                self.npts[0] = N_ELEMENTS(self.x[0,*])
                radius = MIN([nx,ny])/2 - 1
                cx = nx/2 -1
                cy = ny/2 -1
                th = FINDGEN(self.npts[0])/(self.npts[0]-1) * !PI * 2
                cirpol = TRANSPOSE([[th],[REPLICATE(radius, self.npts[0])]])
                cirxy = CV_COORD(from_polar=cirpol,/TO_RECT)
                self.x[0,*] = cirxy[0,*] + cx
                self.y[0,*] = cirxy[1,*] + cy
            END

            'ADC' : BEGIN
                self.npts[0] = 5
                self.x[0,0:4] = [0.04,0.96,0.96,0.04,0.04] * nx
                self.y[0,0:4] = [0.98,0.98,0.12,0.12,0.98] * ny
            END

            ELSE : BEGIN
                self.npts[0] = 5
                self.x[0,0:4] = [0.02,0.98,0.98,0.02,0.02] * nx
                self.y[0,0:4] = [0.98,0.98,0.02,0.02,0.98] * ny
            ENDELSE
        ENDCASE
    ENDIF

    IF KEYWORD_SET(beamstop) THEN BEGIN
        ; Now make beam stop mask
        ; bdr1 is the border width around the center of the beamstop arm shadow
        ; bdr2 is the border width around the center of the beamstop  shadow
        bdr1 = 10. / 1024. * nx
        bdr2 = 20. / 1024. * nx
        PI = 3.14159
        p = CV_COORD(FROM_RECT=[xbsr-xc,ybsr-yc], /to_polar)
        spoon_th = [p[0], p[0]+bdr1/p[1], p[0]+bdr1/bdr2,p[0]+PI/4, p[0]+PI/2, p[0]+PI*0.75, p[0]+PI]
        spoon_r = [p[1], p[1], bdr2, bdr2, bdr2, bdr2, bdr2]
        mask_th = [spoon_th, REVERSE(2*p[0] - spoon_th)]
        mask_r = [spoon_r, REVERSE(spoon_r)]
        mask_pol = [TRANSPOSE(mask_th),TRANSPOSE(mask_r)]
        maskxy = CV_COORD(FROM_POLAR = mask_pol, /TO_RECT)
        self.npts[1] = 14
        self.x[1,0:self.npts[1]-1] = maskxy[0,*] + xc
        self.y[1,0:self.npts[1]-1] = maskxy[1,*] + yc
    ENDIF
END


PRO AS_MaskPolygon__Define

void = {AS_MaskPolygon,              $ ; CURRENT MASK POLYGON DEFINITIONS
        INHERITS AS_FrameObj,        $ ; This objects inherits the FrameObject.
        nmouse:     0,               $ ; Current vertex point from mouse
        current:    2,               $ ; Current mask being defined
        zoom:       3,               $ ; Current zoom used in mask define window
        xlow:       0,               $ ; Lowest x value shown in draw frame
        ylow:       0,               $ ; Lowest y value shown in draw frame
        type:       intarr(10),      $ ; Type code: 0=unused, 1=inclusive, 2=exclusive
        shape:      intarr(10),      $ ; Shape code: 0=general, 1=circle, 2=sector, 3=qz, 4=qy
        auto:       intarr(10),      $ ;=1 to automatically update when beam center changes
        npts:       intarr(10),      $ ; Current selected mask
        x:          fltarr(10,25),   $ ; (i,j) i vectors of up to j x-coordinates
        y:          fltarr(10,25),   $ ; (i,j) i vectors of up to j y-coordinates
        cirx:       fltarr(10),      $ ; x-center of circle mask
        ciry:       fltarr(10),      $ ; y-center of circle mask
        cirr:       fltarr(10),      $ ; radius of circle mask
        sangle:     fltarr(10),      $ ; start angle if polygon is a beam-centered sector
        fangle:     fltarr(10),      $ ; finish angle if polygon is a beam-centered sector
        qz:         fltarr(10),      $ ; Qz for constant qz integration
        qzw:        fltarr(10),      $ ; Qz integration width
        qy:         fltarr(10),      $ ; Qy for constant qy integration
        qyw:        fltarr(10)       $ ; Qy integration width
       }
       
END