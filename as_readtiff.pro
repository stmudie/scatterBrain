; Written by David J. Cookson
; Renamed from saxs_read_tiff by Stephen Mudie 05-JUN-2008
;
function AS_ReadTiff, file, header, data, rotate=rotate, header_only=header_only

  @as_scatterheader.macro

    CATCH, error
    
    IF error NE 0 THEN BEGIN
      CATCH, /CANCEL
      result = Dialog_Message('Error opening file.')
      RETURN, -1
    ENDIF

    data = READ_TIFF(file, orientation = orient)
    data = reverse(data,2)
    IF KeyWord_Set(rotate) THEN data = rotate(data,1)
    header = {                                  $ ; header structures
            xdim:     n_elements(data[*,0]),    $ ; number of columns
            ydim:     n_elements(data[0,*])     $ ; number of rows
    }

    RETURN, 0

end
