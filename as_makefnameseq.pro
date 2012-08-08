
;*******************************************************************************
;   saxs_make_fname_seq
;
;   Makes a string vector containing filenames in sequence
;
;*******************************************************************************
;+
; NAME:
;	AS_MakeFNameSeq
;
; PURPOSE:
;	Constructs a string vector of sequential filenames
;
; CATEGORY:
;	Filename manipulation
;
; CALLING SEQUENCE:
;   Result = AS_MakeFNameSeq(F_name1 [,F_name2])
;
; INPUTS:
;	F_name1 : first filename in sequence
;
; OPTIONAL INPUTS:
;	F_name2 : last filename in sequence - if this is not given, a
;               single filename string is returned
;
; KEYWORD PARAMETERS:
;	INC     :  number increment to be placed on subsequent filenames
;
; OUTPUTS:
;	Returns a vector of filenames (no directory path attached) in the
;   name-number format dictated by the NAMEFORMAT keyword.
;
; COMMON BLOCKS:
;	IMAGE_DATA - required for name format
;
; MACROS USED:
;   AS_FNameStripDir, AS_FileNameRoot, AS_FNameNum
;
; MODIFICATION HISTORY:
;    1-Jan-03  created by David J. Cookson
;   20-Oct-03 (DJC) to allow different file numbering format
;   14-Mar-04 (DJC) now allows increments of file number greater than 1
;    3-Jun-08 (STM) Rename (from saxs_make_fname_seq) and update to AS convention.
;-

FUNCTION AS_MakeFNameSeq, f_name1, f_name2, inc=inc, fext=fext, nameformat=nameformat, nosequence=nosequence, outputext=outputext

    IF ~Keyword_Set(fext) THEN fext = ''
    IF ~Keyword_Set(nameformat) THEN nameformat = ''

    IF keyword_set(nosequence) OR (n_elements(f_name1) GT 1) THEN BEGIN
        num = n_elements(f_name1)
        temp_arr = strarr(num)
        FOR i = 0,num-1 DO temp_arr[i] = AS_FNameStripDir(f_name1[i])
        temp_arr = temp_arr[sort(temp_arr)]
    ENDIF ELSE BEGIN
        IF n_elements(inc) EQ 0 THEN inc=1
        IF f_name2 EQ 'NULL' THEN return, AS_FNameStripDir(f_name1)

        f_root1 = AS_FileNameRoot(AS_FNameStripDir(f_name1),nameformat,/NUM)
        f_root2 = AS_FileNameRoot(AS_FNameStripDir(f_name2),nameformat,/NUM)
        f_num1 = AS_FNameNum(f_name1,nameformat)
        f_num2 = AS_FNameNum(f_name2,nameformat)

        ; check validity of file name sequence
        IF (f_num1 LT 0) OR (f_num2 LT 0) OR (f_root1 NE f_root2) OR (f_num1 GT f_num2) THEN BEGIN
            retmes = dialog_message('Invalid numerical sequence in filenames')
            RETURN, -1
        ENDIF
        num = (f_num2 - f_num1)/inc + 1     ; calc no of frames in seq
        temp_arr = strarr(num)
        FOR i = 0, num - 1 DO BEGIN
            CASE nameformat OF
              '%s.%3.3d'    : temp_arr[i] = f_root1+'.'+string(f_num1+i*inc,format='(I3.3)')
              '%s%3.3d.%s'  : temp_arr[i] = f_root1 $
                                +string(f_num1 + i*inc,format='(I3.3)') + '.' + fext
              '%s%4.4d.%s'  : temp_arr[i] = f_root1 $
                                +string(f_num1 + i*inc,format='(I4.4)') + '.' + fext
              '%s%3.3d'     : temp_arr[i] = f_root1 $
                                +string(f_num1 + i*inc,format='(I3.3)')
              '%s%4.4d'     : temp_arr[i] = f_root1 $
                                +string(f_num1 + i*inc,format='(I4.4)')
               ELSE         : temp_arr[i] = f_root1 $
                                +string(f_num1 + i*inc,format='(I3.3)') + '.' + fext
            ENDCASE
        ENDFOR
    ENDELSE

    IF keyword_set(outputext) THEN BEGIN
        FOR i=0,n_elements(temp_arr)-1 DO BEGIN
            temp_arr[i] = strjoin(strsplit(temp_arr[i],'.',/extract),'_',/single)
            temp_arr[i] = temp_arr[i] + '.' + outputext
        ENDFOR
    ENDIF

    RETURN, temp_arr                                ; return array of filenames
END
