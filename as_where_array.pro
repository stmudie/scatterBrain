; This code is base on code found here: https://groups.google.com/forum/?hl=de&fromgroups=#!topic/comp.lang.idl-pvwave/9uVDvnvCw4k
; I had to change the a_ind and b_ind sections, as for the sort function "If Array contains any identical elements, the order in 
; which the identical elements are sorted is arbitrary and may vary between operating systems." The original code from the website
; worked for linux but not windows.


FUNCTION as_where_array, a, b, b_ind
  COMPILE_OPT idl2
  ON_ERROR, 2

  flag=[replicate(0b,n_elements(a)),replicate(1b,n_elements(b))]
  s=[a,b]
  srt=sort(s)
  s=s[srt] 
  flag=flag[srt]
  wh=where(s eq shift(s,-1) and flag ne shift(flag, -1),count,/NULL)
  if count ne 0 then begin
    f_ind = ([wh,wh+1])[sort([wh,wh+1])]
    a_ind = srt[f_ind[Where(~flag[f_ind],COMPLEMENT=comp)]]
    b_ind = srt[f_ind[comp]] - n_elements(a)
  endif else begin
    a_ind = -1 
    b_ind = -1 
  endelse

   RETURN, a_ind
  
END