FUNCTION as_stringtonumber, string, FLOAT=float, NEGATIVE=negative

  @as_scatterheader.macro

  RegEx = '^0-9'
   
  IF KeyWord_Set(float) THEN RegEx = RegEx + '.'
  IF KeyWord_Set(negative) THEN RegEx = RegEx + '-'
  
  number = StrJoin(StrSplit(string, '['+RegEx+']', /REGEX, /EXTRACT))
  
  IF KeyWord_Set(negative) THEN BEGIN
  
    tempNum = StrSplit(number, '-', /REGEX, /EXTRACT)
    IF StrPos(number,'-') EQ 0 THEN number = StrJoin(['-',tempNum]) ELSE number = StrJoin(tempNum)
  
  ENDIF
    
  IF KeyWord_Set(float) THEN BEGIN
  
    tempNum = StrSplit(number, '.', LENGTH=len, /EXTRACT)
    decimalPos = StrPos(number,'.')
    IF decimalPos EQ 0 THEN number = StrJoin(['.',tempNum])
    IF decimalPos EQ -1 THEN number = StrJoin(tempNum)
    IF decimalPos GT 0 THEN BEGIN
      FOR i = 0, N_Elements(len)-2 DO BEGIN
        len[i+1:*] = len[i+1:*] + len[i]
      ENDFOR
      decimalPos = Where(len EQ decimalPos)
      tempNum = [tempNum,'.']
      number = StrJoin([tempNum[0:decimalPos],tempNum[-1],tempNum[decimalPos+1:-2]])
    ENDIF
    
  ENDIF
  
  IF KeyWord_Set(FLOAT) THEN RETURN, Float(number) ELSE RETURN, Long(number)

END