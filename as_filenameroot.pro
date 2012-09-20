;*************************************************************************
; Copyright (c) 2008 Australian Synchrotron
;*************************************************************************
;+
; NAME:
; AS_FileNameRoot
;
; PURPOSE:
; This function returns the root name (extension removed) of f_name (f_name = string or strarray),
; including any numerical suffix, as defined in the input parameter nameformat. 
;
; CATEGORY:
; Files
;
; CALLING SEQUENCE:
;  
; Result = AS_FileNameRoot(f_name, nameformat)
;
; INPUTS:
; f_name:  The file name with the extension and numerical suffix you want removed.
;
; OPTIONAL INPUTS:
; nameformat:  This is only utilised if the /NUM keyword is supplied. It contains a
;              string defining the format of the file name. The following are valid
;            '%s.%3.3d'   
;            '%s.%4.4d'      
;            '%s%3.3d.%s'    
;            '%s%4.4d.%s'    
;            '%s%3.3d'       
;            '%s%4.4d'      
; 
; KEYWORD PARAMETERS:
; LEN: Set equal to a variable/array which will contain the character length of returned string.
;
; NUM:  set this to strip off file number according to format defined by nameformat
;                     
; EXT:  set this to return the extension
;
; OUTPUTS:
; This function returns the full path and file name, with the extension and file number removed.
;
; OPTIONAL OUTPUTS:
; Length of the returned string (in characters) is returned in variable/array passed to LEN. 
; Extension returned if EXT keyword set. 
;
; MODIFICATION HISTORY:
;   Written by: David J. Cookson 01-Feb-03
;   19-Jan-06 (DJC) modified the nameformat codes
;   02-Jan-08 (DJC) generalized so that f_name can be an array of strings
;   16-May-08 (STM) Converted and updated to AS conventions. Original name saxs_fname_root
;-


FUNCTION AS_FileNameRoot, f_name, nameformat, NUM=num, LEN=len, EXT=ext

  @as_scatterheader.macro

    outarr = f_name ; ensure that outarray has identical size and form to f_name

    ; find position of last "."
    IF N_ELEMENTS(f_name) GT 1 THEN $
        f_delimpos = TRANSPOSE(REFORM(STRPOS(f_name,'.',/REVERSE_SEARCH))) $
    ELSE f_delimpos = STRPOS(f_name,'.',/REVERSE_SEARCH)

    IF KEYWORD_SET(num) THEN BEGIN
        CASE nameformat OF
            '%s.%3.3d'      : pos = [0,0]
            '%s.%4.4d'      : pos = [0,0]
            '%s%3.3d.%s'    : pos = [3,0]
            '%s%4.4d.%s'    : pos = [4,0]
            '%s%3.3d'       : pos = [3,0]
            '%s%4.4d'       : pos = [4,0]
            ELSE            : pos = [3,0]
        ENDCASE
    ENDIF ELSE pos = [0,0]

    indices = WHERE(f_delimpos LT 0, n, complement=cindices, ncomplement=nc)
    IF n GT 0 THEN f_delimpos[indices] = STRLEN(f_name[indices])

    IF KEYWORD_SET(ext) THEN outarr = STRMID(f_name,f_delimpos+1) $
    ELSE outarr = STRMID(f_name,0, f_delimpos)

    IF KEYWORD_SET(num) THEN outarr = STRMID(f_name,0,f_delimpos-pos[0])

    len = STRLEN(outarr)

    RETURN, outarr

END
