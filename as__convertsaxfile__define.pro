FUNCTION AS__ConvertSAXFile::INIT
  
  @as_scatterheader.macro

  RETURN, 1

END

PRO AS__ConvertSAXFile::Convert, saxFileName, XMLFileName, XMLOBJECT = XMLObject

  @as_scatterheader.macro

  IF Arg_Present(saxFileName) NE 1 THEN saxFileName = Dialog_Pickfile(FILTER = '*.sax', /MUST_EXIST, /READ)
  IF saxFileName EQ '' THEN RETURN
  
  convStruct = self.readSAXFile(saxFileName)

  frameStruct = {len : convStruct.cameraLength, wlen : convStruct.wavelength, xc : convStruct.xcentre, yc : convStruct.ycentre, detangle : 0}
  detectorStruct = {DetectorDef : convStruct.frametype, XSize : convStruct.xsize, YSize : convStruct.ysize, PixelSize : convStruct.pixelSize, BasePV : '', CamPV :'', ImagePV :'', LogFilePV: '', FilePV :'', Control :'', SoftwareTrigger : '', AutoLoad : ''}
  maskStruct = Replicate({type : 0, auto : 0, lock: 0, colour: [0,0,0], shape : 'Polygon', name : '', params : Ptr_New()},10)
  FOR i = 0, 9 DO BEGIN
    maskStruct[i].auto = convStruct.masks.auto[i]
    maskStruct[i].lock = convStruct.masks.lock[i]
    maskStruct[i].colour = convStruct.masks.colour[i]
    maskStruct[i].name = 'Mask ' + StrCompress(i, /REMOVE_ALL)
    maskStruct[i].params = Ptr_New([convStruct.masks.x[i,0:convStruct.masks.npts[i]-1],convStruct.masks.y[i,0:convStruct.masks.npts[i]-1]])
    CASE convStruct.masks.type[i] OF
      0 : maskStruct[i].type = 0
      1 : maskStruct[i].type = 2 
      2 : maskStruct[i].type = 1
    ENDCASE
      
  ENDFOR
      
  xmlFileObj = as_scatterXMLFile()
  xmlFileObj.SetParameters, FRAME = frameStruct, ADMAP = detectorStruct, MASK = maskStruct, CONFIGNAME = 'SAXconvertedXML'
  
  IF Arg_Present(XMLObject) THEN BEGIN
    xmlObject = xmlFileObj
    RETURN
  ENDIF 
  
  IF Arg_Present(XMLFileName) NE 1 THEN XMLFileName = Dialog_Pickfile(FILTER = '*.xml', DEFAULT_EXTENSION='xml', /WRITE, /OVERWRITE_PROMPT)
  IF XMLFileName NE '' THEN xmlFileObj.saveFile, XMLFileName, /EMPTY
  
END

FUNCTION AS__ConvertSAXFile::readSAXFile, saxFileName

  @as_scatterheader.macro

  mask_polygon = {mask_polygon,            $
                type    : IntArr(10),    $
                npts    : IntArr(10),    $
                auto    : IntArr(10),    $
                lock    : IntArr(10),    $
                colour  : IntArr(10,3),  $
                shape   : IntArr(10),    $
                cirx    : FltArr(10),    $
                ciry    : FltArr(10),    $
                cirr    : FltArr(10),    $
                x       : FltArr(10,50), $
                y       : FltArr(10,50), $
                sangle  : FltArr(10),    $
                fangle  : FltArr(10),    $
                qz      : FltArr(10),    $
                qzw     : FltArr(10),    $
                qy      : FltArr(10),    $
                qyw     : FltArr(10)     $
                }
  
  OpenR, LUN, saxFileName, /GET_LUN 
  
      ; now loop through until end of *,sax file
    WHILE NOT(eof(LUN)) do begin
       tempstr = ''
       readf, LUN, tempstr
       CASE strmid(tempstr,0,6) of
         'FRTYPE' : type =      strmid(tempstr,7) 
         'FRFRMT' : format =    strmid(tempstr,7)
         'FRNFMT' : nameformat= strmid(tempstr,7)
         'FRFEXT' : begin
                        fext =  strmid(tempstr,7)
                        ;fext2 = frmdata.fext
                    end
         'FRPSIZ' : psize =     float(strmid(tempstr,7))
         'FRNXPX' : nxpix =     fix(strmid(tempstr,7))
         'FRNYPX' : nypix =     fix(strmid(tempstr,7))
         'FRLENG' : len =       float(strmid(tempstr,7))
         'FRWLEN' : wlen =      float(strmid(tempstr,7))
         'FRXCEN' : xc =        float(strmid(tempstr,7))
         'FRYCEN' : yc =        float(strmid(tempstr,7))
         'PDNTYP' : nrmtype =  float(strmid(tempstr,7))
         'PDTRNM' : tnrm =     float(strmid(tempstr,7))
         'PDIONM' : ionrm =    double(strmid(tempstr,7))
         'PDBSNM' : ibsnrm =   double(strmid(tempstr,7))
         'PDTBLK' : tblank =   float(strmid(tempstr,7))
         'PDCSRW' : cscalraw = float(strmid(tempstr,7))
         'PDCSCL' : cscalib =  float(strmid(tempstr,7))
         'PDCSOS' : cscalos =  float(strmid(tempstr,7))
         'PDCSER' : cserror =  float(strmid(tempstr,7))
         'PDCSST' : csstand =  float(strmid(tempstr,7))
         'PDCST1' : csstdthk = float(strmid(tempstr,7))
         'PDCST2' : cssmpthk = float(strmid(tempstr,7))
         'PDCSSI' : csistand = fix(strmid(tempstr,7))
         'PDUSCL' : usecalib = fix(strmid(tempstr,7))
         'MSKPTS' : n_mask_pts = fix(strmid(tempstr,7))
         'NMASKS' : begin
                        if n_elements(n_mask_pts) EQ 0 then n_mask_pts = 12
                        maxnmask = n_elements(mask_polygon.x[*,0]) - 1
                        maxnpt = n_elements(mask_polygon.y[0,*]) - 1
                        nmasks = fix(strmid(tempstr,7))

                        temp1 = intarr(nmasks)
                        readf, LUN, temp1
                        mask_polygon.type[0:nmasks-1] = temp1
                        if nmasks LE maxnmask then mask_polygon.type[nmasks:maxnmask] = 0

                        readf, LUN, temp1
                        mask_polygon.npts[0:nmasks-1] = temp1
                        if nmasks LE maxnmask then mask_polygon.npts[nmasks:maxnmask] = 0

                        temp1 = fltarr(nmasks,n_mask_pts)
                        readf, LUN, temp1
                        mask_polygon.x[0:nmasks-1,0:n_mask_pts-1] = temp1
                        if n_mask_pts LE maxnpt then mask_polygon.x[0:nmasks-1,n_mask_pts:maxnpt] = 0

                        temp1 = fltarr(nmasks,n_mask_pts)
                        readf, LUN, temp1
                        mask_polygon.y[0:nmasks-1,0:n_mask_pts-1] = temp1
                        if n_mask_pts LE maxnpt then mask_polygon.y[0:nmasks-1,n_mask_pts:maxnpt] = 0

                        for i = 2,nmasks-1 do begin
                            if (mask_polygon.npts[i] GT 4) then begin
                                if (abs(mask_polygon.x[i,0] $
                                    - mask_polygon.x[i,mask_polygon.npts[i]-1] $
                                    + mask_polygon.y[i,0] $
                                    - mask_polygon.y[i,mask_polygon.npts[i]-1]) LT 1) then begin

                                    temp = cv_coord(/to_polar, /degrees, from_rect = $
                                        [mask_polygon.x[i,1]-xc,mask_polygon.y[i,1]-yc])
                                    mask_polygon.sangle[i] = temp[0]
                                    temp = cv_coord(/to_polar, /degrees, from_rect = $
                                            [mask_polygon.x[i,mask_polygon.npts-1] $
                                            -xc,mask_polygon.y[i,mask_polygon.npts-1]-yc])
                                    mask_polygon.fangle[i] = temp[0]
                                endif
                            endif
                        endfor
                    end
         ELSE :
      ENDCASE  
  ENDWHILE

  FREE_LUN, LUN
 
  RETURN, {frameType : type, frameFormat : format, nameFormat : nameFormat, pixelSize : psize, xsize : nxpix, ysize : nypix, cameralength : len, wavelength : wlen, $
           xcentre : xc, ycentre : yc, nrmType : nrmtype, masks : mask_polygon}

END

PRO AS__ConvertSAXFile__Define

  void = {AS__ConvertSAXFile, $
           filename : '' $
         }

END