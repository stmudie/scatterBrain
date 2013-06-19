FUNCTION AS_FrameObj::Init, RAWDATA = rawData, HISTMAX=histMax, HISTMIN=histMin, LOGOBJ=logObj, NOTIFY=notify, _REF_Extra=extra

  @as_scatterheader.macro

  IF KeyWord_Set(notify) THEN $
    IF TypeName(notify[0]) EQ 'NOTIFY' $
      THEN self.notify = List(notify, /EXTRACT)
  
  
  self.frame.path  =     'No Name Yet'        
  self.frame.errThresh = 5              
  self.frame.batchtsexp = [0,1,2,5,10,20,30,40,60,80,120,180,240]
  self.frame.offset  =  0
  self.frame.ionrm   = 300000.0       
  self.frame.ibsnrm  = 300000.0       
  self.frame.type =      'TIF'              ; type of frame data stored
  self.frame.format =    'TIF'              ; format of frame data stored
  self.frame.fext =      '001'              ; format file extension
  self.frame.fext2 =     '001'              ; format file extension #2
  self.frame.conv =      [16.,10000.,4.]    ; conversion parameters for detectors like BAS IPs
  self.frame.idark =     0                  ;  =1 if dark frame should be applied to frame
  self.frame.drkfname =  'no_name_yet'      ; name of dark frame image
  self.frame.darksf =    1.0                ; dark frame scale factor
  self.frame.iflood =    0                  ;  =1 if flood frame should be applied to frame
  self.frame.fldfname =  'no_name_yet'      ; name of flood frame image
  self.frame.fldfade =   1.0                ; fade factor for flood frame image
  self.frame.nameformat ='%s.%4.4d'         ; format of frame file name
  self.frame.medsmooth = 0                  ;  =1 if frame is to be smoothed after read in
  self.frame.medval = 3                     ;  = box size for median smoothing
  self.frame.boxsubtract = 0                ;  =1 to subtract boxcar average of frame from frame
  self.frame.boxcarval  = 40                ;  = box size for boxcar average
  self.frame.boxcaros =  3000               ;  = positive offset value applied after boxcar subtraction
  self.frame.timesource =0                  ;  =0 for log file   =1 for frame header
  self.frame.zoom =      3                  ; zoom factor for displayed image
  self.frame.scrx =      371                ; draw screen xsize on main window
  self.frame.scry =      371                ; draw screen ysize on main window
  self.frame.qimax =    -1.0                ; radial max q index for image intensity range
  self.frame.qimin =    -1.0                ; radial min q index for image intensity range
  self.frame.intmax =    -1.0               ; maximum intensity on frame
  self.frame.intmin =    -1.0               ; minimum intensity on frame
  self.frame.intmean =   -1.0               ; mean intensity on frame
  self.frame.intsdev =   -1.0               ; stdeviation of intensity on frame
  self.frame.adufactor = 7.0                ; number of A/D units (raw detector counts) per x-ray photon
  self.frame.toverhead = 0.0               ; shutter open time  = logged exposure time - toverhead
  self.frame.nxchip =    2048               ; physical no. of pixel rows on chip
  self.frame.nychip =    2048               ; physical no. of pixel columns on chip
  self.frame.pschip =    0.046              ; minimum pixel size in mm
  self.frame.psize =     0.092              ; image pixel size in mm
  self.frame.pbin =      4                  ; symmetric pixel bin
  self.frame.nxpix =     1024               ; image number of pixel rows
  self.frame.nypix =     1024               ; image number of pixel columns
  self.frame.detangle =  0.0                ; waxs detector angle
  self.frame.detOffsetH = 0.0               ; waxs detector offset in horizontal (beam direction)
  self.frame.detOffsetV = 0.0               ; waxs detector offset in vertical
  self.frame.roinxp =    1024               ; sub region of interest (ROI) no of x-pixels
  self.frame.roinyp =    1024               ; sub region of interest (ROI) no of y-pixels
  self.frame.roixmx =    1023               ; sub region of interest max x value
  self.frame.roixmn =    0                  ; sub region of interest min x value
  self.frame.roiymx =    1023               ; sub region of interest max x value
  self.frame.roiymn =    0                  ; sub region of interest min x value
  self.frame.wlen =      1.5                ; wavelength of x-rays
  self.frame.len =       1864.0             ; length of camera (sample to det in mm)
  self.frame.callen =    1864.0             ; nominal length of camera when calibrant peak fitted
  self.frame.calwlen =   1.5                ; nominal wavelength of camera when calibrant peak fitted
  self.frame.calfit =    0.1076             ; q value for fitted line
  self.frame.calline =   0.1076             ; q value for calibration line
  self.frame.tilt =      0.00               ; detector tilt from normal to beam
  self.frame.tazim =     0.00               ; azimuthal direction of detector tilt
  self.frame.xc =        202.5              ; pixel x-coord of beam center
  self.frame.yc =        428.2              ; pixel y-coord of beam center
  self.frame.xcr =       202.5              ; pixel x-coord of reflected beam center
  self.frame.ycr =       572.0              ; pixel y-coord of reflected beam center
  self.frame.xbsr =      0.0                ; pixel x-coord of beam stop root
  self.frame.ybsr =      572.0              ; pixel y-coord of beam stop root
  self.frame.bsradius =  2.5                ; effective radius of beam stop (mm)
  self.frame.nfid =      0                  ; number of fiducial mask spots
  self.frame.xfn =       33                 ; number of fiducials horizontal - full mask
  self.frame.yfn =       33                 ; number of fiducials vertical - full mask
  self.frame.xfn1 =      33                 ; number of fiducials horizontal - temporary block
  self.frame.yfn1 =      33                 ; number of fiducials vertical - temporary block
  self.frame.smmm =      300.0              ; sample to mask distance in mm
  self.frame.mdmm =      3.0                ; mask to detector distance in mm
  self.frame.xfpx =      20.0               ; true pixel x-spacing of fiducial spots
  self.frame.yfpx =      20.0               ; true pixel y-spacing of fiducial spots
  self.frame.rfpx =      20.0               ; true pixel radius of fiducial ring
  self.frame.xfmm =      2.540              ; true mm x-spacing of fiducial spots
  self.frame.yfmm =      2.540              ; true mm y-spacing of fiducial spots
  self.frame.rfq =       0.10               ; true A-1 radius of fiducial ring
  self.frame.plotted =   0                  ;  =1 when saxs_plot has just been executed
  
  self.frame.header = Ptr_New(/ALLOCATE_HEAP)
  self.frame.zing = 3.0                 
  self.frame.zingerThreshold = 1000
  self.frame.zingerPixels = List()
  self.frame.nrmtype = 0               
  
  self.frame.updateImage = 1
  
  
  IF KeyWord_Set(logObj) THEN self.frame.logObj = logObj
  
  IF KeyWord_Set(histMax) THEN self.frame.histMax = histMax
  IF KeyWord_Set(histMin) THEN self.frame.histMin = histMin
  
  self.frame.autoScale = 1
  self.frame.histPalette = [[IndGen(256)],[IndGen(256)],[IndGen(256)]]
  self.frame.histType = 'LINEAR'
  self.frame.histGamma = 1.5
  self.frame.histExponent = 4
  self.frame.histMean = 0.5
  self.frame.histSigma = 1
  self.frame.histBeta = 3

  self.frame.circObjects = Ptr_New(ObjArr(2,1))
  self.frame.frameFont_Obj = Obj_New('IDLgrFont', 'Helvetica', SIZE = 3)


  IF KeyWord_Set(rawData) THEN BEGIN
    self.frame.rawData = Ptr_New(rawData)
    self.frame.histImage = Ptr_New(rawData)    
    IF KeyWord_Set(extra) THEN BEGIN
      IF N_Elements(Where(extra EQ 'DATA')) THEN RETURN, self->IDLgrImage::Init(_EXTRA=extra) ELSE RETURN, self->IDLgrImage::Init(DATA=BytScl(rawData), _EXTRA=extra)
    ENDIF ELSE RETURN, self->IDLgrImage::Init(DATA = BytScl(rawData))
  ENDIF

  self.frame.rawData = Ptr_New(BytArr(self.frame.nxpix,self.frame.nypix))
  self.frame.histImage = Ptr_New(*self.frame.rawData)
  
  result = self->IDLgrImage::Init(_EXTRA=extra)
  
  palette = Obj_New('IDLgrPalette')
  palette->LoadCT, 0
  self.SetProperty, palette=palette
  
  self.HistDisplay, /NOSHOW
  
  RETURN, result

END

PRO AS_FrameObj::Notify, event

  @as_scatterheader.macro

    FOREACH notify, self.notify DO IF Obj_Valid(notify) THEN notify.notify, event
   
END

PRO AS_FrameObj::DrawImage, PRESERVEVIEWPLANE = preserve

  @as_scatterheader.macro
    
    IF ~KeyWord_Set(preserve) THEN $ 
    IF N_Elements(*self.frame.rawData) GT 0 THEN self.frame.frameViewObj->SetProperty, VIEWPLANE_RECT = [0,0,n_elements((*self.frame.rawData)[*,0]),n_elements((*self.frame.rawData)[0,*])]
    self.frame.frameWinObj->Draw
        
END

PRO AS_FrameObj::GetFilenames, outputext=outputext, single=single, strprompt=strprompt, template=template, control_str=control_str

@as_scatterheader.macro

IF n_elements(control_str) GT 0 THEN must_exist = 0 ELSE must_exist=1

    CASE self.frame.fext OF
        '001'   : filt = '*.*'
        ''      : filt = '*'
        ELSE    : filt = '*.'+self.frame.fext
    ENDCASE

  IF n_elements(template) NE 0 THEN BEGIN
        CASE self.frame.fext OF
            '001'   : filt = AS_FileNameRoot(File_Basename(template),self.frame.nameformat,/NUM)+'.*'
            ''      : filt = AS_FileNameRoot(File_Basename(template),self.frame.nameformat,/NUM)+'*'
            ELSE    : filt = AS_FileNameRoot(File_Basename(template),self.frame.nameformat,/NUM)+'???.'+self.frame.fext
        ENDCASE
  ENDIF

    IF n_elements(strprompt) EQ 0 THEN strprompt = 'Select image '
    IF n_elements(control_str) EQ 0 THEN control_str = '$'

    IF keyword_set(single) THEN BEGIN
        f_name1 = dialog_pickfile(/READ,PATH=self.frame.path,must_exist=must_exist, /multiple_files $
            , filter = filt, TITLE = strprompt)
        IF f_name1[0] EQ '' THEN return;, 'NULL'
        self.frame.path = File_DirName(f_name1[0],/MARK_DIRECTORY)
        self.frame.fext2 = AS_FileNameRoot(f_name1[0],/EXT)
        self.frame.fnamelist = File_Basename(f_name1[0])
        return
    ENDIF

    f_name1 = dialog_pickfile(/READ,PATH=self.frame.path,must_exist=must_exist, /multiple_files $
                , filter = filt $
                , TITLE = strprompt +': first file in sequence - or select a group of image files')
    IF f_name1[0] EQ '' THEN return;, 'NULL'
    ;IF f_name1[0] EQ self.frame.path + control_str THEN return, control_str
    self.frame.fext2 = AS_FileNameRoot(f_name1[0],/ext)
    self.frame.path = File_DirName(f_name1[0],/MARK_DIRECTORY)

    IF n_elements(f_name1) EQ 1 THEN BEGIN
        f_name1 = f_name1[0]
        CASE self.frame.fext OF
            '001'   : filt = AS_FileNameRoot(File_Basename(f_name1),self.frame.nameformat,/NUM)+'.*'
            ''      : filt = AS_FileNameRoot(File_Basename(f_name1),self.frame.nameformat,/NUM)+'*'
            ELSE    : filt = AS_FileNameRoot(File_Basename(f_name1),self.frame.nameformat,/NUM)+'???.'+self.frame.fext
        ENDCASE
        f_name2 = dialog_pickfile(/READ, PATH=self.frame.path, /must_exist $
              , TITLE = 'Select last image file in sequence' $
              , filter = filt)
        IF f_name2 EQ '' THEN return;, 'NULL'

        fnamelist = AS_MakeFNameSeq(f_name1, f_name2, fext=self.frame.fext2, outputext=outputext, NAMEFORMAT=self.frame.nameformat)
    ENDIF ELSE BEGIN
        fnamelist = AS_MakeFNameSeq(f_name1, 'NULL', outputext=outputext, fext=self.frame.fext, NAMEFORMAT=self.frame.nameformatm /nosequence)
    ENDELSE
    self.frame.fnamelist = fnamelist
    
END

PRO AS_FrameObj::UpdateBeamCursor, HIDE=hide

  @as_scatterheader.macro

  IF Obj_Valid(self.frame.beamCursor) THEN self.frame.beamCursor->SetProperty, DATA = Transpose([[0,self.frame.nxpix,self.frame.xc,self.frame.xc,self.frame.xc],[self.frame.yc,self.frame.yc,self.frame.yc,0,self.frame.nypix]]) ELSE BEGIN
      self.frame.beamCursor = Obj_New('IDLgrPolyline', [0,self.frame.nxpix,self.frame.xc,self.frame.xc,self.frame.xc],[self.frame.yc,self.frame.yc,self.frame.yc,0,self.frame.nypix], LINESTYLE = 2, COLOR = 255)  
      self.PARENT->Add, self.frame.beamCursor
  ENDELSE
  
  IF N_Elements(hide) GT 0 THEN self.frame.beamCursor.SetProperty, HIDE=hide
  
  self.DrawImage, /PRESERVE

END

PRO AS_FrameObj::SetZingerPixMask, threshold, CLEAR = clear

  @as_scatterheader.macro

  IF KeyWord_Set(clear) THEN self.frame.zingerPixels = List() ELSE BEGIN

    type = Size(threshold,/TYPE)
    IF (type GT 1 AND type LT 6) OR (type GT 11) THEN self.frame.zingerThreshold = threshold
    self.frame.zingerPixels = List(Where(*self.frame.rawData GT self.frame.zingerThreshold))
  
  ENDELSE

END

FUNCTION AS_FrameObj::GetRawImage, f_name, frame = frame, quiet=quiet, info_only=info_only

  @as_scatterheader.macro

 ;*******************************************************
    ;  Add as many of these frame input modules as needed
    ;*******************************************************

    oldnxpix = self.frame.nxpix
    oldnypix = self.frame.nypix

    IF self.frame.format EQ 'SMART' THEN BEGIN
       ; read in a Bruker SMART file
       if AS_ReadSiemens(f_name, header, frame,self.frame.nxpix,self.frame.nypix) LT 0 then begin
             if NOT(keyword_set(quiet)) then $
                mesret = dialog_message('Problem opening CCD image file '+ f_name)
             return, -1
       endif

       frame = reverse(frame,2)

       if NOT(keyword_set(quiet)) then begin
           self.frame.timestamp = strmid(header[1,26],0,8)+'_'+strmid(header[1,26],11)

           drkstr1 = string(fix(strmid(header[1,27],0,12)),format='(I0.3)')+'s'
           drkstr2 = AS_FileNameRoot(strmid(header[1,86],0,8))
           if drkstr1 NE drkstr2 then ret = dialog_message(f_name + string(10B) + string(10B) $
                    + 'Dark image ('+drkstr2+') and exposure time ('+drkstr1+') do not match!')
        endif
        *self.frame.header = header
    ENDIF

    ;*******************************************************

    IF self.frame.format EQ 'ADC' THEN BEGIN
       ; read in an ADC/AVIEX file
       if AS_ReadADC(f_name, header, frame,self.frame.nxpix,self.frame.nypix) LT 0 then begin
             if NOT(keyword_set(quiet)) then $
                mesret = dialog_message('Problem opening CCD image file '+ f_name)
             return, -1
       endif

       frame = reverse(frame,2)

       if NOT(keyword_set(quiet)) then begin
           self.frame.timestamp = strmid(header[16],21,8)+'_'+strmid(header[16],30,5)
       endif
       *self.frame.header = header
    ENDIF
   
   ;*******************************************************

    IF self.frame.format EQ 'ADSC' THEN BEGIN
       ; read in an ADSC file
       if AS_ReadADSC(f_name, header, frame,self.frame.nxpix,self.frame.nypix) LT 0 then begin
             if NOT(keyword_set(quiet)) then $
                mesret = dialog_message('Problem opening CCD image file '+ f_name)
             return, -1
       endif

       frame = reverse(frame,2)

       self.frame.psize = float(strsplit(string(strmid(header[7],11)),';',/extract))
       *self.frame.header = header
    ENDIF
    ;*******************************************************

    IF self.frame.format EQ 'MPA' THEN BEGIN
       ; read in a Scott Barton wire MPA ascii format detector file
       if AS_ReadWire2(f_name, header, frame, self.frame.nxpix,self.frame.nypix) LT 0 then begin
             if NOT(keyword_set(quiet)) then $
                mesret = dialog_message('Problem opening CCD image file '+ f_name)
             return, -1
       endif
       *self.frame.header = header
    ENDIF
    ;*******************************************************

;    IF self.frame.format EQ 'BAS' THEN BEGIN
;       ; read in a BAS imaging plate file
;       if AS_ReadBas(frame, header, f_name, NUMXPIX=self.frame.nxpix,NUMYPIX=self.frame.nypix) LT 0 then begin
;             if NOT(keyword_set(quiet)) then $
;                mesret = dialog_message('Problem opening CCD image file '+ f_name)
;             return, -1
;       endif
;       *self.frame.header = header
;    ENDIF
;    ;*******************************************************
;
;    IF self.frame.format EQ 'SPE' THEN BEGIN
;        ; read in a Princeton-format (.SPE) file
;        if AS_ReadPrinceton(f_name, header, frame) LT 0 then begin
;             if NOT(keyword_set(quiet)) then $
;                mesret = dialog_message('Problem opening CCD image file '+ f_name)
;             return, -1
;        endif
;       self.frame.nxpix = header.xdim
;       self.frame.nypix = header.ydim
;       frame = reverse(frame,2)
;       *self.frame.header = header
;    ENDIF
;    ;*******************************************************
;
;    IF self.frame.format EQ 'BRUKER' THEN BEGIN
;        ; read in a Princeton-format file from Bruker detector
;        if AS_ReadPrinceton(f_name, header, frame) LT 0 then begin
;             if NOT(keyword_set(quiet)) then $
;                mesret = dialog_message('Problem opening CCD image file '+ f_name)
;             return, -1
;        endif
;       self.frame.nxpix = header.xdim
;       self.frame.nypix = header.ydim
;       frame = reverse(frame,2)
;       *self.frame.header = header
;    ENDIF
;
    ;*******************************************************
    IF self.frame.format EQ 'TIF' THEN BEGIN
        ; read in a TIFF 16bit format (.TIF) file
        if AS_ReadTiff(f_name, header, frame) LT 0 then begin
             if NOT(keyword_set(quiet)) then $
                mesret = dialog_message('Problem opening CCD image file '+ f_name)
             return, -1
        endif

       if keyword_set(header_only) then return, 0  ;  If header_only keyword set return immediately
       self.frame.nxpix = header.xdim
       self.frame.nypix = header.ydim
       *self.frame.header = header
    ENDIF
    
    ;*******************************************************
    IF self.frame.format EQ 'PIL200K' THEN BEGIN
        ; read in a TIFF 32bit format (.TIF) file
        if AS_ReadTiff(f_name, header, frame, /rotate) LT 0 then begin
             if NOT(keyword_set(quiet)) then $
                mesret = dialog_message('Problem opening CCD image file '+ f_name)
             return, -1
        endif

       if keyword_set(header_only) then return, 0  ;  If header_only keyword set return immediately
       self.frame.nxpix = header.xdim
       self.frame.nypix = header.ydim
       *self.frame.header = header
    ENDIF

;    ;*******************************************************
;    IF self.frame.format EQ 'FITS' THEN BEGIN
;        ; read in a FITS format file (.FTS) file
;        if AS_ReadFits(f_name, header, frame) LT 0 then begin
;             if NOT(keyword_set(quiet)) then $
;                mesret = dialog_message('Problem opening CCD image file '+ f_name)
;             return, -1
;        endif
;       self.frame.nxpix = header.xdim
;       self.frame.nypix = header.ydim
;       *self.frame.header = header
;    ENDIF
;
;    ;*******************************************************
;    IF self.frame.format EQ 'MAR345' THEN BEGIN
;        ; read in a MAR-345 format file
;        if AS_ReadMar345(f_name, header, frame) LT 0 then begin
;             if NOT(keyword_set(quiet)) then $
;                mesret = dialog_message('Problem opening CCD image file '+ f_name)
;             return, -1
;        endif
;       if keyword_set(header_only) then return, 0  ;  If header_only keyword set return immediately
;       self.frame.nxpix = header.nx
;       self.frame.nypix = header.ny
;       self.frame.psize = header.x_pixel_size
;       *self.frame.header = header
;    ENDIF
;
;    ;*******************************************************
;    ;
;    ;   Check that the number of frame pixels agree with the previous setup frame size,
    ;   if not then adjust all relevant parameters

   if oldnxpix NE self.frame.nxpix then begin
        rescale = float(self.frame.nxpix) / float(oldnxpix)
        self.frame.psize = self.frame.psize / rescale
        self.frame.xc = self.frame.xc * rescale
        self.frame.yc = self.frame.yc * rescale
        self.frame.xcr = self.frame.xcr * rescale
        self.frame.ycr = self.frame.ycr * rescale
        self.frame.xbsr = self.frame.xbsr * rescale
        self.frame.ybsr = self.frame.ybsr * rescale
        self.frame.xfpx = self.frame.xfpx * rescale
        self.frame.yfpx = self.frame.yfpx * rescale
        self.frame.rfpx = self.frame.rfpx * rescale
        self.frame.roinxp = self.frame.roinxp * rescale
        self.frame.roinyp = self.frame.roinyp * rescale
        self.frame.roixmx = self.frame.roixmx * rescale
        self.frame.roixmn = self.frame.roixmn * rescale
        self.frame.roiymx = self.frame.roiymx * rescale
        self.frame.roiymn = self.frame.roiymn * rescale
        ; Call to renamed, AS updated function on next two lines.
        ;AS__SaxsMakeMaskPolygon, self, mask_polygon, /FRAMEBORDER
        ;AS__SaxsMakeMaskPolygon, self, mask_polygon, /BEAMSTOP
        ; --
        ;cakedata.newmask = 1
        ;cakedata.ok = 0
   endif

    if (n_elements(frame[*,0]) NE self.frame.nxpix) $
        OR (n_elements(frame[0,*]) NE self.frame.nypix) then begin
        strmes = 'Read in frame = (' + string(n_elements(frame[0,*]),format='(I4)') $
                 + ', ' + string(n_elements(frame[0,*]),format='(I4)') $
                 + ')  but (nx,ny) = (' + string(self.frame.nxpix,format='(I4)') $
                 + ', ' + string(self.frame.nypix,format='(I4)') + ')'
         mesret = dialog_message(strmes)
         return, -1
    endif else return, 0

end

FUNCTION AS_FrameObj::GetImage, seqfnames, quiet=quiet, FRAME=frame

@as_scatterheader.macro

;*******************************************************************************
;   saxs_get_image
;
;   Using either a single filename or a sequence of filenames given in the
;   variable "seqfnames" read in frames, de-zinger and average them.
;
;   When reading in each frame extract a log entry is found in the log file data
;   record (which should already be in memory).
;
;   For each frame we get a log entry that gives us:
;
;       t       - time duration of SAXS frame exposure
;       Io      - incident counts during time t
;       Io(bg)  - incident counter background count rate (c/s)
;       Ibs     - beam stop counts during time t
;       Ibs(bg) - beam stop counter background count rate (c/s)
;
;       If more than one frame is being added together for averaging purposes these
;       numbers obtained for each separate frame need to be averaged and then
;       combined to give:
;
;       iosf    - incident intensity scaling sub-factor (for the final averaged frame)
;       ibssf   - beamstop intensity scaling sub-factor (for the final averaged frame)
;
;   Prior to but not necessarily just before each SAXS frame log entry there should
;   be a transmission measurement entry with:
;
;       t       - time duration of transmission measurement
;       Io      - incident counts during time t
;       Io(bg)  - incident counter background count rate (c/s)
;       It      - transmitted counts during time t
;       It(bg)  - transmission counter background count rate (c/s)
;
;       These numbers are used to produce itsf - the transmitted intensity scaling sub-factor
;
;       Note that for a given sequence of frame to be averaged, only one set of transmission
;       values is used - those obtained from the last transmission log entry before the last
;       frame in the sequence.  NOTE: it is not a good idea to average SAXS frames broken up by a
;       transmission measurement - only the last tranmission data entry will be used for the final
;       scaling factor.
;
;   Finally after all frames are accumlated and averaged, this function returns a total
;   scaling factor for scaling the final integrated profile.
;
;   Modification History
;   01-Dec-02   created David J. Cookson
;   01-Jan-03   (DJC) - fixed lots of bugs
;   25-May-03   (DJC) - itsf is now stored as a 'raw' ratio
;   09-Nov-03   (DJC) - check for suspicious normalization data in frame average sequences
;
;*******************************************************************************

    if self.frame.nrmtype GT 0 then begin
        if self.frame.log EQ 'no_name_yet' then begin
            strtemp = 'No log data in memory - continue with no normalization?'
            if dialog_message(strtemp,/question,/default_no) EQ 'No' then begin
                strtemp = 'If you want normalization - you need to load a log file'
                retmes = dialog_message(strtemp,/information)
                errfatal = 1
                return, -1
            endif else self.frame.nrmtype = 0
        endif
    endif

    nframes = n_elements(seqfnames)
    ;cur = profdata.current

    ; **********************************************************************************
    ; Make up string to describe the sequence of frames in the form  'test.001-005'
    ; Put this file sequence name string into the profdata structure as profdata.imgfname

;    if nframes EQ 1 then $
;        profdata.imgfname[cur] = seqfnames[0] $
;    else $
;        profdata.imgfname[cur] = AS_FileNameRoot(seqfnames[0],self.frame.nameformat,/NUM) $
;            + string(AS_FNameNum(seqfnames[0],self.frame.nameformat), format='(I3.3)') $
;            + string(AS_FNameNum(seqfnames[nframes-1],self.frame.nameformat), format='("-",I3.3)')

    timesum = 0     ; initialize sums for averaging io,it and bs counts for all
    iosum = 0       ; frames used in the average.  We may only be averaging one
    iobgsum = 0     ; frame!
    itsum = 0
    itbgsum = 0
    bssum = 0
    bsbgsum = 0
    sumframe = 0    ; this will become an frame and then later a sum of frames if nframes > 1
    nskipped = 0    ; this is the number of frames in a sequence that were skipped over

    FOR iframe = 0, nframes - 1 DO BEGIN

        ; read in next image in sequence
        status = 0
        IF iframe GT 0 THEN frame = !Null
        IF N_Elements(frame) EQ 0 THEN status = self->GetRawImage(self.frame.path + seqfnames[iframe], frame=frame, /quiet)
        IF status LT 0 THEN BEGIN
            errfatal = 1
            return, -1
        ENDIF
        strtemp = 'Frame Seq. Read # ' + string(iframe,format='(I3.3)') $
                    +' '+ self.frame.path + seqfnames[iframe]
;        mesresult = AS_AddMessage(strtemp,wBase)
        
        IF iframe EQ 0 THEN BEGIN
            sumframe = Long(frame)
            lastframe = Temporary(frame)

            self.frame.fname = seqfnames[0] 

;            index = self.frame.logObj->SearchLogFile(seqfnames[0],/TRANSMISSION)
;
;            ; if entry is found in logfile calculate transmission factor
;            IF index GE 0 THEN BEGIN
;                itsf = self.frame.logObj->GetScale(index)
;            ENDIF ELSE BEGIN
;                IF (self.frame.nrmtype GT 0) THEN BEGIN
;                    strtemp = 'Transmission data for ' + seqfnames[iframe] $
;                                + ' not found - continue with transmission = 1?'
;                    IF dialog_message(strtemp,/question,/default_no) EQ 'No' THEN BEGIN
;                        errfatal = 1
;                        return, -1
;                    ENDIF
;                ENDIF
                itsf = 1
;            ENDELSE

            index = self.frame.logObj->GetIndex(seqfnames[0])           ; find log file entry

            ; Calculate the Io or Ibs structure factor from the log entry for SAXS exposure
            ; if entry is found in logfile add to running count totals
            if index GE 0 then begin
;                if self.frame.timesource EQ 0 then begin
;                    s = log_data.timestamp[*,index]      ; extract time stamp from log
;                    profdata.timestamp[cur] = s[0]+s[1]+s[2]+'_'+s[3]
;                endif
;                profdata.opticstr[cur] = log_data.opticstr[index] $
;                                        + ' ' + strjoin(log_data.sdata[*,index],' ')
                
                void = self.frame.logObj->GetScale(index,I0=iosum,IBS = bssum,TIME=time1)
                
                ;d = min(abs(self.frame.batchtsexp-time1),itime)
                ;time = self.frame.batchtsexp[itime]
                
                timesum = time1 - self.frame.toverhead
                lastio = iosum  ; Note that we are assuming the first Io value read for
                                ; this sequence of frames is good. If the next Io value in
                                ; the log file is within 10% of this value, it will become
                                ; the new Io value to compare against.
            endif else begin
                if (self.frame.nrmtype GT 0) THEN BEGIN ;AND NOT(profdata.ignore_err) then begin
                    strtemp = 'Normalizing data for ' + seqfnames[iframe] + ' not found in log file - continue?'
                                ;+ ' not found in ' + filenames.log + ' - continue?'
                                
                    if dialog_message(strtemp,/question,/default_no) EQ 'No' then begin
;                        profdata.errfatal = 1
                        return, -1
                    endif else begin
                        errnorm = 1
                        timesum = 1.0
                        iosum = 1
                        bssum = 1
                    endelse
                endif else begin
                    errnorm = 1
                    timesum = 1.0
                    iosum = 1
                    bssum = 1
                endelse
            endelse
            
        endif else begin            ; end of processing for first file in read-in sequence

        ; This section handles subsequent frames read in sequence.  Note that this
        ; time we are only doing Io scale factor calculations not transmissions. All
        ; frames read in on the same sequence are assumed to be the same sample with
        ; the same absorption.

            self.frame.fname = 'Summed'

            index = -1
            index = self.frame.logObj->GetIndex(seqfnames[iframe])

            ; if entry is found in logfile add to running count totals
            if index GE 0 then begin

                ; Check that the normalization data has not changed by a suspicious amount
                ; since the last frame.  If the Io value has changed by more than 10% since
                ; the last frame log entry, give the user the opportunity to skip the frame
                ; entirely.
                void = self.frame.logObj->GetScale(index,I0=i0counts, IBS = ibscounts,TIME=time1)
;                io_ratio_check = float(i0counts)/float(lastio)
;                if (io_ratio_check GT self.frame.errthresh) OR (io_ratio_check LT 1/self.frame.errthresh) then begin
;
;                    ; If the user oks the skipping of a frame in a sequence then exit this
;                    ; iteration and continue with the next loop
;                    strtemp = 'Io for ' + seqfnames[iframe] $
;                            + '  is ' + string(round(io_ratio_check*100), format='(I3)') $
;                            + ' % of previous frame - skip this frame?'
;
;                    retmes = dialog_message(strtemp,/question,/cancel)
;                    if retmes EQ 'Cancel' then return,-1
;                    if dialog_message(strtemp,/question) EQ 'Yes' then begin
;                        nskipped = nskipped + 1         ; Increment counter of frames skipped
;                        continue                        ; Break from current loop and go
;                                                        ; directly to the next frame
;                    endif
;                endif

;                lastio = i0counts         ; Save current Io value to check
                                                        ; the next frame's Io value.
;                d = min(abs(self.frame.batchtsexp - time1),itime)
;                time = self.frame.batchtsexp[itime]
                iosum = iosum + i0counts
                bssum = bssum + ibscounts
                timesum = timesum + time1 - self.frame.toverhead
            endif else begin
                if self.frame.nrmtype GT 0 then begin ; abort read-in sequence if nrmtype > 0
                    strtemp = 'Normalizing data for ' + seqfnames[iframe] + ' not found in logfile - aborting operation'
                            ;+ ' not found in ' + filenames.log + ' - aborting operation'
                    mesres = dialog_message(strtemp)
                    ;profdata.errfatal = 1
                    return, -1
                endif else begin            ; if nrmtype = 0 use bogus count values
                    iosum = 1
                    bssum = 1
                endelse
            endelse

            
            IF self.frame.zingerPixels.count() GT 0 THEN BEGIN
              zingerPixels = self.frame.zingerPixels[0].toArray(/TRANSPOSE)
              sumframe[zingerPixels] = -2
            ENDIF
        
            ; Now correlate each subsequent frame with the previous (lastframe)
            ; to remove zingers

            temparr = ((frame-self.frame.offset) $
                        /((lastframe-self.frame.offset)>self.frame.offset) - self.frame.zing) > 0
            zlookup = where(temparr GT 0 AND lastFrame GT 10, zcnt)
            print, 'no of zinger pixels found = ',zcnt
            if min(zlookup) GE 0 then frame[zlookup] = lastframe[zlookup]

            ; Only do this next section for the first correlation ie after the second frame has
            ; been read in.  After the first zinger removal is performed 'lastfile' will
            ; always be zinger free - so only the latest frame will need zinger removal

            if iframe EQ 1 then begin
                temparr = ((lastframe-self.frame.offset) $
                            /((frame-self.frame.offset)>self.frame.offset)-self.frame.zing) > 0
                zlookup = where(temparr GT 0 AND lastframe GT 10, zcnt)
                print, 'no of zinger pixels found = ',zcnt
                if min(zlookup) GE 0 then lastframe[zlookup] = frame[zlookup]
                sumframe = frame + lastframe
            endif else begin
                sumframe = sumframe + frame     ; Add de-zingered frame to the sum
                lastframe = frame
            endelse
        endelse         ; end of code for read in of subsequent files in read in sequence
    endfor              ; end of for loop reading in frames from the sequence.

    ; put multi-image average into frame array
    frame = sumframe

    ; Now calculate Io scaling factor using count sums - note that 'ionrm and ibsnrm' hold a
    ; counts per second value set by the user.  This ensures that the Io or Ibs scaling factor
    ; does not unrealistically inflate/deflate the counts shown in the final SAXS profiles.

    if (timesum NE 0)  then begin               ; check that counts are valid
        iosf=iosum;/self.frame.ionrm;/timesum       ; against iosum/ionrm or ibsum/ibsnrm
        ibssf = bssum;/self.frame.ibsnrm;/timesum
    endif

    self.frame.i0counts = iosum
    self.frame.iBScounts = bssum
    self.frame.time = timesum

    self.frame.itsf = itsf
    self.frame.i0sf = iosf
    self.frame.ibssf = ibssf

       ;**********************************************************************************************
    ; Now apply all requested corrections (in dparam.pro window) to the raw image frame

    frame = long(frame)
    if self.frame.idark EQ 1 THEN begin
        if self.frame.darksf LT 0 THEN frame = frame - dark + self.frame.offset $
        else begin
          frame = frame - (float(dark)-self.frame.offset) * self.frame.darksf / 1;profdata.sf[cur]   Fix this STM.
          print, 'Dark subtraction not correct as sf not set for frame, when it is for dark. Code needs to be fixed'
        endelse
    endif
    if self.frame.iflood EQ 1 THEN frame = round(((float(frame - self.frame.offset)>0)*flood)/1000) + self.frame.offset
    if self.frame.medsmooth THEN frame = median(frame,self.frame.medval)
    if self.frame.boxsubtract THEN frame = ((frame - smooth(frame, self.frame.boxcarval, /nan, missing=1)+self.frame.boxcaros)>0)+self.frame.offset
    
    self->SetProperty,RAWDATA=frame
    self.GetProperty, UPDATEIMAGE = update 
    IF KeyWord_Set(update) THEN self->DrawImage, /PRESERVE

    return, 1;profdata.sf[cur]
end

FUNCTION AS_FrameObj::CheckSaturation

  @as_scatterheader.macro

  IF self.frame.saturation EQ 0 THEN RETURN, 0
  IF self.frame.time GE 1 OR self.frame.time EQ 0 THEN BEGIN
    void = Where(*self.frame.rawData GT self.frame.saturation-self.frame.saturation*0.05, nSat) 
    IF nSat GT 10 THEN RETURN, 1 ELSE RETURN, 0
  ENDIF ELSE BEGIN
    void = Where(*self.frame.rawData/self.frame.time GT self.frame.saturation-self.frame.saturation*0.05, nSat) 
    IF nSat GT 10 THEN RETURN, 1 ELSE RETURN, 0
  ENDELSE
END

PRO AS_FrameObj::Delete_QCirc, qRadius
   
    @as_scatterheader.macro
   
    temp = *self.frame.circObjects
    IF N_Elements(temp) LE 2 THEN RETURN
    IF qRadius EQ -1 THEN del = IndGen(N_Elements(temp)/2-1) ELSE del = 0
    
    FOR i = N_Elements(del) - 1, 0, -1 DO BEGIN
    
      temp = *self.frame.circObjects
      radii = 0
    
      IF N_Elements(temp) GT 2 THEN BEGIN 
       
        radii = StrArr(N_Elements(temp)/2-1)

        FOR j = 0, (N_Elements(temp)/2 -1) - 1 DO BEGIN
          temp[j*2+1]->GetProperty, STRINGS = temp_str
          radii[j] = temp_str[0]
        ENDFOR
        radii = Float(radii)
      ENDIF
   
      IF qRadius NE -1 THEN del = Where(radii EQ qRadius) 
        
      IF del[i] GE 0 THEN BEGIN
        Obj_Destroy, temp[2*del[i]+1]
        Obj_Destroy, temp[2*del[i]]
        IF N_Elements(temp) GT 4 THEN BEGIN
          unMatchedRadii = Where(radii NE radii[del[i]])
          subscripts = FltArr(2*N_Elements(unMatchedRadii))

          FOR j = 0, N_Elements(unMatchedRadii)-1 DO subscripts[[2*j,2*j+1]] = [2*unMatchedRadii[j], 2*unMatchedRadii[j]+1]
          *self.frame.circObjects = [temp[subscripts],ObjArr(2)] 
      
        ENDIF ELSE *self.frame.circObjects = ObjArr(2,1)
      
      ENDIF

    ENDFOR
    self.frame.frameWinObj->Draw

END

PRO AS_FrameObj::OverLay_QCirc, qRadius
   
    @as_scatterheader.macro
   
    IF Obj_Valid(self.PARENT) THEN BEGIN
      self.PARENT->GetProperty, PARENT = view
      IF ~Obj_Valid(view) THEN RETURN
    ENDIF ELSE RETURN
   
    temp = *self.frame.circObjects
    radii = 0
    
    IF N_Elements(temp) GT 2 THEN BEGIN 
      
      radii = StrArr(N_Elements(temp)/2-1)

      FOR i = 0, (N_Elements(temp)/2 -1) - 1 DO BEGIN
        temp[i*2+1]->GetProperty, STRINGS = temp_str
        radii[i] = temp_str[0]
      ENDFOR
      radii = Float(radii)
    ENDIF
    
    IF Where(Abs(radii - qRadius) LT 0.001*qRadius) EQ -1 THEN BEGIN 

      self.frame.frameFont_Obj->SetProperty, SIZE = 10

      radius = (self.frame.len * Tan(2*ASin(qRadius * self.frame.wlen / 4 / !dpi))/self.frame.psize)
      x = radius*IndGen(31)/30.
      y = SqRt(radius^2 - x^2 > 0)
    
      IF self.frame.detAngle EQ 0 THEN BEGIN
        circle = Obj_New('IDLgrPolyline',Shift([-x+self.frame.xc,Reverse(-x+self.frame.xc), x+self.frame.xc,Reverse(x+self.frame.xc)],-40), Shift([-y+self.frame.yc,Reverse(y+self.frame.yc),y+self.frame.yc,Reverse(-y+self.frame.yc)],-40), COLOR = [0,230,255],LINESTYLE = [2, 'AAAA'X])
      ENDIF
      
      charMult = 0.25
      
      IF self.frame.detAngle NE 0 THEN BEGIN
        radDetAngle = !dpi*self.frame.detAngle/180.
        ;qAtXc = 4*!dpi*sin(radDetAngle/2.)/self.frame.wlen
        qAngle = 2*ASin(qRadius * self.frame.wlen / 4 / !dpi)
        angDif = radDetAngle - qAngle
        pixelOffset = self.frame.len*tan(angDif)/self.frame.psize
        
        IF pixelOffset + self.frame.xc GT self.frame.nxpix THEN RETURN
        
        x = pixelOffset + (self.frame.nxpix-self.frame.xc-pixelOffset)*indgen(101)/100.
        
        yNumer = (x*Tan(radDetAngle) + self.frame.len/self.frame.psize)^2
        yDenom = (Cos(qAngle)/Cos(radDetAngle))^2
        y = SqRt((yNumer/yDenom) - x^2 - (self.frame.len/self.frame.psize)^2)
        
        circle = Obj_New('IDLgrPolyline', [reverse(x+self.frame.xc),x+self.frame.xc], [reverse(self.frame.yc+y),self.frame.yc-y], COLOR = [0,255,0],LINESTYLE = [2, 'AAAA'X], /USE_LABEL_ORIENTATION)
      
        charMult = 0.5
      
      ENDIF
      
      label = Obj_New('IDLgrText', Replicate(String(qRadius, format = '(G0)'),7), FONT = self.frame.frameFont_obj, CHAR_DIMENSIONS = [50*charMult,50*charMult])

      circle->SetProperty, LABEL_OBJECTS = LABEL
    
      *self.frame.circObjects = [circle,label,temp]

      self.PARENT->Add,circle

      self.frame.frameWinObj->Draw

    ENDIF

END

PRO AS_FrameObj::OverLay_Line, data, frameModelObj
      
   @as_scatterheader.macro   
   
   IF ~Obj_Valid(*self.frame.lineObject) THEN BEGIN
     self.frame.lineObject = Obj_New('IDLgrPolyline', data, color = [255,0,0])
     frameModelObj->Add, line
   ENDIF ELSE IF N_Elements(data) GT 0 THEN self.frame.lineObject->SetProperty, data ELSE self.frame.lineObject->GetProperty, data=data 
      
   self.frame.frameWinObj->Draw   
END

FUNCTION AS_FrameObj::ScaleImage

  @as_scatterheader.macro

  ; Scales the image data appropriately, depending on scale type. Lifted from David Fannings XStretch.

  IF self.frame.autoScale EQ 1 AND KeyWord_Set(*self.frame.rawData) THEN BEGIN
        moments = moment(*self.frame.rawData * *self.mask.mask)
        histMinTemp = 1.5*moments[0] - SqRt(moments[1])
        histMaxTemp = 1.5*moments[0] + SqRt(moments[1])
               
        
        IF histMaxTemp GT self.frame.histXzoom[1] OR histMinTemp LT self.frame.histXZoom[0] THEN BEGIN
          self.frame.histObj.zoomexternal, /OUT
          self.frame.histObj.setthresholdexternal, min = histMinTemp, max = histMaxTemp
          self.frame.histObj.zoomexternal
        ENDIF
        
        self.frame.histMin = histMinTemp
        self.frame.histMax = histMaxTemp
        
  ENDIF

  IF Obj_Valid(self.frame.histObj) THEN BEGIN
    self.frame.histObj.SetThresholdExternal, MIN = self.frame.histMin, MAX = self.frame.histMax
    ;self.frame.histObj.ZoomExternal
  ENDIF 

 
  ; Turn floating underflow warnings off.
  thisExcept = !Except
  !Except = 0
  
  IF N_Elements(*self.frame.rawData) EQ 0 THEN RETURN, 0
  
  CASE self.frame.histType OF
  
    'LINEAR': BEGIN
      scaledImage = BytScl(*self.frame.rawData, Max=self.frame.histMax, Min=self.frame.histMin, /NAN)
      IF self.frame.histNegative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
    END
    
    'LINEAR 2%': BEGIN
      scaledImage = BytScl(*self.frame.rawData, Max=self.frame.histMax, Min=self.frame.histMin, /NAN)
      IF self.frame.histNegative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
    END
    
    '^0.2' : BEGIN
      scaledImage = BytScl((*self.frame.rawData)^.2, Max=self.frame.histMax^.2, Min=self.frame.histMin^.2, /NAN)
      IF self.frame.histNegative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
    END
    
    'EQUALIZATION': BEGIN
      scaledImage = BytScl(Hist_Equal(*self.frame.rawData), Max=self.frame.histMax, Min=self.frame.histMin, /NAN)
      IF self.frame.histNegative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
    END
    
    'GAMMA': BEGIN
      scaledImage = GmaScl(*self.frame.rawData, Max=self.frame.histMax, Min=self.frame.histMin, $
        Gamma=self.frame.histGamma, Negative=self.frame.histNegative)
      RETURN, scaledImage
    END
    
    'GAUSSIAN': BEGIN
      scaledImage = GaussScl(*self.frame.rawData, Max=self.frame.histMax, Min=self.frame.histMin, $
        Sigma=self.frame.histSigma, Negative=self.frame.histNegative)
      RETURN, scaledImage
    END
    
    'SQUARE ROOT': BEGIN
      scaledImage = BytScl(SQRT(*self.frame.rawData), Max=self.frame.histMax, Min=self.frame.histMin, /NAN)
      IF self.frame.histNegative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
      RETURN, scaledImage
    END
    
    'LOG': BEGIN
      scaledImage =  LogScl(*self.frame.rawData, Max=self.frame.histMax, Min=self.frame.histMin, $
        Mean=self.frame.histMean, Exponent=self.frame.histExponent, Negative=self.frame.histNegative)
      RETURN, scaledImage
    END
    
    'ASINH' :BEGIN
    scaledImage = ASinhScl(*self.frame.rawData, Max=self.frame.histMax, Min=self.frame.histMin, $
      BETA=self.frame.histBeta, Negative=self.frame.histNegative)
    RETURN, scaledImage
  END
  
ENDCASE

END

PRO AS_FrameObj::HistDisplay, group_leader, frameWinObj, frameViewObj, HIST_BUTTON=histBut, XPOS = xpos, YPOS = ypos, XSIZE = xsize, YSIZE = ysize, NOSHOW = noShow

@as_scatterheader.macro

IF Obj_Valid(self.frame.histObj) AND Widget_Info(self.frame.histWin, /VALID) THEN BEGIN
  Widget_Control, self.frame.histWin, /MAP
  RETURN
ENDIF

noShow = KeyWord_Set(NOSHOW)

IF Obj_Valid(frameWinObj) GT 0 THEN self.frame.frameWinObj = frameWinObj
IF Obj_Valid(frameViewObj)  GT 0 THEN self.frame.frameViewObj = frameViewObj 
IF KeyWord_Set(histBut) THEN self.frame.histBut = histBut
IF KeyWord_Set(self.frame.histBut) THEN Widget_Control, self.frame.histBut, SET_VALUE = 1
IF KeyWord_Set(group_leader) THEN self.frame.group_leader = group_leader
IF N_Elements(xpos) EQ 0 THEN xpos = 0
screenSize = Get_Screen_Size()
IF N_Elements(ypos) EQ 0 THEN ypos = screenSize[1]-250

IF self.frame.histMax LE 0 THEN histMax = 0 ELSE histMax = self.frame.histMAx
IF self.frame.histMin LE 0 THEN histMin = 0 ELSE histMin = self.frame.histMin
IF histMin EQ histMax THEN histMax = histMin + 100


self.frame.histObj = Obj_New('AS_XStretch', self.frame.histImage, NOTIFY_OBJ = { OBJECT : self, METHOD : 'Hist' }, GROUP_LEADER = self.frame.group_leader, $
                           NO_WINDOW = 1, NO_MAP_GUI = noShow, XPOS = xpos, XSIZE = xsize, YPOS = ypos, YSIZE = ysize, minThresh = histMin, $
                           PALETTE = self.frame.histPalette, maxThresh = histMax, TYPE = self.frame.histType, BETA = self.frame.histBeta, $
                           GAMMA = self.frame.histGamma, EXPONENT = self.frame.histExponent, SIGMA = self.frame.histSigma, MEAN = self.frame.histMean, $
                           NEGATIVE = self.frame.histNegative, XZOOM=self.frame.histXZoom)

END

PRO AS_FrameObj::HistHide

  @as_scatterheader.macro

  Widget_Control, self.frame.histWin, MAP = 0
  ;;Widget_Control, self.frame.histWin, /DESTROY
  ;Obj_Destroy, self.frame.histObj
  ;self.frame.histWin = 0

END

PRO AS_FrameObj::Hist, xstruct

  @as_scatterheader.macro

  IF xstruct.hist_win GT 0 THEN BEGIN
    
    self.frame.histPalette = [[xstruct.r], [xstruct.g], [xstruct.b]]
    self.frame.histType = xstruct.type
    self.frame.histMin = xstruct.minThresh
    self.frame.histMax = xstruct.maxThresh
    self.frame.histBeta = xstruct.beta
    self.frame.histGamma = xstruct.gamma
    self.frame.histExponent = xstruct.exponent
    self.frame.histSigma = xstruct.sigma
    self.frame.histMean = xstruct.mean
    self.frame.histNegative = xstruct.negative
    self.frame.histWin = xstruct.hist_win 
    self.frame.histDraw = xstruct.hist_draw
    self.frame.histXZoom = xstruct.xzoom
    
    self->SetProperty, DATA = xstruct.Image
    self.palette.SetProperty, RED_VALUES=xstruct.r, GREEN_VALUES=xstruct.g, BLUE_VALUES=xstruct.b
    self.frame.frameWinObj->Draw
  
  ENDIF ELSE BEGIN
    Widget_Control, self.frame.histBut, SET_VALUE = 0
    self.frame.histWin = 0
  ENDELSE
END

PRO AS_FrameObj::SetProperty, RAWDATA=rawData, HISTMIN = histMin, HISTMAX = histMax, AUTOSCALE = autoScale, FRAMEVIEWOBJ = frameViewObj, FRAMEWINOBJ = frameWinObj, $
                              PATH = path, NORMTYPE = normType, LOGOBJ=logObj, UPDATEIMAGE = updateImage, SATURATION = saturation, ZINGERTHRESH = zingerThresh, _REF_Extra=extra

  @as_scatterheader.macro

  IF KeyWord_Set(frameWinObj) THEN BEGIN
    IF Obj_Class(frameWinObj) EQ 'IDLGRWINDOW' THEN BEGIN
      self.frame.frameWinObj=frameWinObj
    ENDIF 
  ENDIF  
  IF KeyWord_Set(frameViewObj) THEN self.frame.frameViewObj=frameViewObj
  IF KeyWord_Set(path) THEN self.frame.path = path
  IF KeyWord_Set(normType) THEN self.frame.nrmtype = normType
  IF KeyWord_Set(logObj) THEN self.frame.logObj = logObj
  IF N_Elements(updateImage) GT 0 THEN self.frame.updateImage = KeyWord_Set(updateImage)
  
  IF KeyWord_Set(saturation) THEN self.frame.saturation = saturation
  IF KeyWord_Set(zingerThresh) THEN self.frame.zing = zingerThresh
    
  IF KeyWord_Set(rawData) OR N_Elements(autoScale) OR KeyWord_Set(histMin) OR KeyWord_Set(histMax) THEN BEGIN 
  
    IF KeyWord_Set(rawData) THEN BEGIN
      *self.frame.rawData = rawData
       IF self.frame.updateImage THEN BEGIN 
         *self.frame.histImage = rawData
         self.frame.histObj.NewImage,*self.frame.rawData , min = self.frame.histmin, max = self.frame.histmax
       ENDIF
    ENDIF
    IF N_Elements(autoScale) NE 0 THEN self.frame.autoScale = KeyWord_Set(autoScale)
    IF N_Elements(histMin) NE 0 THEN self.frame.histMin = histMin
    IF N_Elements(histMax) NE 0 THEN self.frame.histMax = histMax

    
  
;    histPlot = 0
;
;    IF KeyWord_Set(self.frame.histWin) THEN BEGIN
;      winGeom = widget_info(self.frame.histwin, /GEOMETRY )
;      drawGeom = widget_info(self.frame.histDraw, /GEOMETRY )
;      self->HistHide
;      histPlot = 1
;    ENDIF              

     
     IF KeyWord_Set(extra) THEN BEGIN
        IF (Where(extra EQ 'DATA'))[0] NE -1 THEN self->IDLgrImage::SetProperty, _EXTRA=extra 
     ENDIF ELSE BEGIN
       IF self.frame.updateImage THEN self->IDLgrImage::SetProperty, DATA = self->ScaleImage(), _EXTRA = extra $
                           ELSE self->IDLgrImage::SetProperty, _EXTRA = extra
     ENDELSE
     
;     IF histPlot EQ 1 THEN self->HistDisplay, XPOS = winGeom.xoffset, YPOS = winGeom.yoffset, XSIZE = drawGeom.xsize, YSIZE = drawGeom.ysize
  
  ENDIF ELSE IF KeyWord_Set(extra) THEN self->IDLgrImage::SetProperty, _Extra=extra

END

PRO AS_FrameObj::GetProperty, RAWDATA=rawData, HEIGHT = height, IMAGEPATH = path, TIME = time, I0SF = i0sf, IBSSF = ibssf, GROUP_LEADER = groupLeader, _REF_Extra=extra

  @as_scatterheader.macro

  IF Arg_Present(groupLeader) THEN groupLeader = self.frame.group_leader
  IF KeyWord_Set(rawData) THEN rawData = *self.frame.rawData
  IF Arg_Present(height) THEN IF Obj_Valid(self.frame.frameWinObj) THEN BEGIN
    self.frame.frameWinObj.GetProperty, DIMENSIONS=dims
    height = dims[1]
  ENDIF ELSE height = -1 
  IF Arg_Present(path) THEN path = self.frame.path
  IF Arg_Present(time) THEN time = self.frame.time
  IF Arg_Present(i0sf) THEN i0sf = self.frame.i0sf
  IF Arg_Present(ibssf) THEN ibssf = self.frame.ibssf  
  self->IDLgrImage::GetProperty, _Extra=extra
  

END

PRO AS_FrameObj::CleanUp  

  @as_scatterheader.macro

  IF Ptr_Valid(self.frame.rawData) THEN Ptr_Free, self.frame.rawData
  IF Ptr_Valid(self.frame.histImage) THEN Ptr_Free, self.frame.histImage
  IF Ptr_Valid(self.frame.circObjects) THEN BEGIN
    IF N_Elements(*self.frame.circObjects) GT 2 THEN Obj_Destroy, *self.frame.circObjects
    Ptr_Free, self.frame.circObjects
  ENDIF
  IF Obj_Valid(self.frame.frameFont_obj) THEN Obj_Destroy, self.frame.frameFont_obj
    
  Self->IDLgrImage::Cleanup

END

PRO AS_FrameObj::StoreParams, paramObj, CONFIG=config;, FRAME=frame

  @as_scatterheader.macro

  frame = {  wlen    : self.frame.wlen,$
             len     : self.frame.len,$
             xc      : self.frame.xc, $
             yc      : self.frame.yc, $
             detangle: self.frame.detangle}
  
  paramObj.SetParameters, FRAME=frame, CONFIGNO = config

END

PRO AS_FrameObj::NewParams, paramObj, CONFIGNO = configNo

  @as_scatterheader.macro

  frame = 1
  IF N_Elements(configNo) EQ 0 THEN configNo = 0


  paramObj->GetParameters, FRAME=frame
  
  frame = (frame)[configNo]
  
  self->IDLgrIMAGE::SetProperty, DIMENSIONS = [frame.NXPix, frame.NYPix]
  
  frameNames = Tag_Names(frame)
  
  FOR i = 0, N_Elements(frameNames) - 1 DO BEGIN
  
    matchTag = Where(Tag_Names(self.frame) EQ frameNames[i])
    IF matchTag GE 0 THEN BEGIN 
      
      IF Size(frame.(i),/TYPE) EQ 10 AND Size(self.frame.(matchTag),/TYPE) EQ 10 THEN BEGIN
          Ptr_Free, self.frame.(matchTag)
          self.frame.(matchTag) = frame.(i)
      ENDIF 
      IF Size(frame.(i),/TYPE) EQ 10 AND Size(self.frame.(matchTag),/TYPE) NE 10 THEN BEGIN
         self.frame.(matchTag) = *(frame.(i))
      ENDIF 
      IF Size(frame.(i),/TYPE) NE 10 AND Size(self.frame.(matchTag),/TYPE) EQ 10 THEN BEGIN
         *(self.frame.(matchTag)) = frame.(i)
      ENDIF 
      IF Size(frame.(i),/TYPE) NE 10 AND Size(self.frame.(matchTag),/TYPE) NE 10 THEN self.frame.(matchTag) = frame.(i)
              
    ENDIF
   
  ENDFOR
  
  CASE frame.detector OF
    'Pilatus 1M'   : self.frame.format = 'TIF'
    'Pilatus 200K' : self.frame.format = 'PIL200K'
    ELSE : self.frame.format = 'TIF'
  ENDCASE
   
      
END

PRO AS_FrameObj::ReSize, DIM=dim, BUFFER=buffer

  @as_scatterheader.macro

  IF Keyword_Set(dim) THEN BEGIN
    self.frame.frameWinObj->GetProperty, DIMENSIONS = origDim
    aspect = origDim[0]/origDim[1]
    ratio = dim[0]/dim[1]
    IF ratio LE aspect THEN BEGIN
      x = dim[0]
      y = dim[0]/aspect
    ENDIF ELSE BEGIN
      x = dim[1]*aspect
      y = dim[1]
    ENDELSE
  ENDIF ELSE BEGIN
    x = self.frame.nxpix
    y = self.frame.nypix
  ENDELSE

  IF ~Keyword_Set(buffer) THEN buffer = [0,0] 
  
  self.frame.frameWinObj->GetProperty, SCREEN_DIMENSIONS = sDim
  sDim = sDim-buffer
  IF sDim[0] GE x AND sDim[1] GE y THEN self.frame.frameWinObj->SetProperty, DIMENSIONS = [x,y] ELSE BEGIN
    x = x/(x/sDim[0] > y/sDim[1])
    y = y/(x/sDim[0] > y/sDim[1])
    self.frame.frameWinObj->SetProperty, DIMENSIONS = [x,y]
  ENDELSE
  self.frame.frameWinObj->Draw
END

PRO AS_FrameObj__Define

frame = { AS_FrameObj_Struc, $      
               fname   : '',               $
               rawData : Ptr_New(),        $
               saturation : 0.,             $
               histImage : Ptr_New(),      $
               group_leader : 0L,          $
               histMax : 0L,               $
               histMin : 0L,               $
               histType: '',               $
               histBeta: 0.0,              $
               histGamma: 0.0,             $
               histMean : 0.0,             $
               histNegative : 0,           $
               histExponent : 0.0,         $
               histSigma: 0.0,             $
               histPalette : IntArr(256,3),$
               histBut : 0L,               $
               histWin : 0L,               $
               histDraw: 0L,               $
               histXZoom: [0.0,0.0],       $
               frameViewObj : Obj_New(),   $
               frameWinObj: Obj_New(),     $
               autoScale : 0,              $
               circObjects : Ptr_New(),    $
               lineObject  : Obj_New(),    $
               frameFont_obj: Obj_New(),   $
               histObj: Obj_New(),         $
               fnamelist : 'No Names Yet', $
               path      : 'No Name Yet',  $
               timestamp : '',             $
               header   : Ptr_New(),       $
               zing : 3.0,                 $ ; zinger rejection factor (was in profdata).
               nrmtype :  2,               $
               logPath :  '',              $
               logObj : Obj_New(),         $
               errThresh : 5,              $
               batchtsexp : [0,1,2,5,10,20,$
                          30,40,60,80,     $
                          120,180,240],    $ ; allowed integer second choices for detector
               ionrm     : 300000.0,       $
               ibsnrm    : 300000.0,       $
               i0counts  : 0L,             $
               iBScounts : 0L,             $
               time      : 0.0,            $
               itsf      : 0.0,            $
               i0sf      : 0.0,            $
               ibssf     : 0.0,            $
               offset    : 64 ,            $ ; offset on all raw SAXS profiles (dark current) (was in profdata)
               type:      'SMART',         $ ; type of frame data stored
               format:    'SMART',         $ ; format of frame data stored
               fext:      '001',           $ ; format file extension
               fext2:     '001',           $ ; format file extension #2
               os:        fltarr(4),       $ ; offset values for quadrants
               nos:       fltarr(4),       $ ; new offset values for quadrants
               conv:      [16.,10000.,4.], $ ; conversion parameters for detectors like BAS IPs
               idark:     0,               $ ; =1 if dark frame should be applied to frame
               drkfname:   'no_name_yet',  $ ; name of dark frame image
               darksf:    1.0,             $ ; dark frame scale factor
               iflood:    0,               $ ; =1 if flood frame should be applied to frame
               fldfname:   'no_name_yet',  $ ; name of flood frame image
               fldfade:   1.0,             $ ; fade factor for flood frame image
               nameformat:'%s.%3.3d',      $ ; format of frame file name
               medsmooth: 0,               $ ; =1 if frame is to be smoothed after read in
               medval: 3,                  $ ; = box size for median smoothing
               boxsubtract: 0,             $ ; =1 to subtract boxcar average of frame from frame
               boxcarval : 40,             $ ; = box size for boxcar average
               boxcaros:  3000,            $ ; = positive offset value applied after boxcar subtraction
               timesource:0,               $ ; =0 for log file, =1 for frame header
               zoom:      3,               $ ; zoom factor for displayed image
               scrx:      371,             $ ; draw screen xsize on main window
               scry:      371,             $ ; draw screen ysize on main window
               qimax:    -1.0,             $ ; radial max q index for image intensity range
               qimin:    -1.0,             $ ; radial min q index for image intensity range
               intmax:    -1.0,            $ ; maximum intensity on frame
               intmin:    -1.0,            $ ; minimum intensity on frame
               intmean:   -1.0,            $ ; mean intensity on frame
               intsdev:   -1.0,            $ ; stdeviation of intensity on frame
               adufactor: 7.0,             $ ; number of A/D units (raw detector counts) per x-ray photon
               toverhead: 0.0,            $ ; shutter open time = logged exposure time - toverhead
               nxchip:    2048,            $ ; physical no. of pixel rows on chip
               nychip:    2048,            $ ; physical no. of pixel columns on chip
               pschip:    0.046,           $ ; minimum pixel size in mm
               psize:     0.092,           $ ; image pixel size in mm
               pbin:      4,               $ ; symmetric pixel bin
               nxpix:     1024,            $ ; image number of pixel rows
               nypix:     1024,            $ ; image number of pixel columns
               detangle:  0.0,             $ ; waxs detector angle
               detOffsetH: 0.0,            $ ; waxs detector horizontal offset
               detOffsetV: 0.0,            $ ; waxs detector vertical offset
               roinxp:    1024,            $ ; sub region of interest (ROI) no of x-pixels
               roinyp:    1024,            $ ; sub region of interest (ROI) no of y-pixels
               roixmx:    1023,            $ ; sub region of interest max x value
               roixmn:    0,               $ ; sub region of interest min x value
               roiymx:    1023,            $ ; sub region of interest max x value
               roiymn:    0,               $ ; sub region of interest min x value
               wlen:      1.5,             $ ; wavelength of x-rays
               len:       1864.0,          $ ; length of camera (sample to det in mm)
               callen:    1864.0,          $ ; nominal length of camera when calibrant peak fitted
               calwlen:   1.5,             $ ; nominal wavelength of camera when calibrant peak fitted
               calfit:    0.1076,          $ ; q value for fitted line
               calline:   0.1076,          $ ; q value for calibration line
               tilt:      0.00,            $ ; detector tilt from normal to beam
               tazim:     0.00,            $ ; azimuthal direction of detector tilt
               xc:        202.5,           $ ; pixel x-coord of beam center
               yc:        428.2,           $ ; pixel y-coord of beam center
               xcr:       202.5,           $ ; pixel x-coord of reflected beam center
               ycr:       572.0,           $ ; pixel y-coord of reflected beam center
               xbsr:      0.0,             $ ; pixel x-coord of beam stop root
               ybsr:      572.0,           $ ; pixel y-coord of beam stop root
               bsradius:  2.5,             $ ; effective radius of beam stop (mm)
               nfid:      0,               $ ; number of fiducial mask spots
               xfid:      fltarr(2000),    $ ; x-coords of fiducial mask spots
               yfid:      fltarr(2000),    $ ; y-coords of fiducial mask spots
               xfn:       33,              $ ; number of fiducials horizontal - full mask
               yfn:       33,              $ ; number of fiducials vertical - full mask
               xfn1:      33,              $ ; number of fiducials horizontal - temporary block
               yfn1:      33,              $ ; number of fiducials vertical - temporary block
               smmm:      300.0,           $ ; sample to mask distance in mm
               mdmm:      3.0,             $ ; mask to detector distance in mm
               xfpx:      20.0,            $ ; true pixel x-spacing of fiducial spots
               yfpx:      20.0,            $ ; true pixel y-spacing of fiducial spots
               rfpx:      20.0,            $ ; true pixel radius of fiducial ring
               xfmm:      2.540,           $ ; true mm x-spacing of fiducial spots
               yfmm:      2.540,           $ ; true mm y-spacing of fiducial spots
               rfq:       0.10,            $ ; true A-1 radius of fiducial ring
               plotted:   0,               $ ; =1 when saxs_plot has just been executed
               beamcursor: Obj_New(),      $
               updateImage : 1,            $
               zingerPixels : List(),      $
               zingerThreshold : 0l         $
               }

void = { AS_FrameObj , $
         INHERITS IDLgrImage,        $
         frame : frame, $
         notify: List() $
       }

END