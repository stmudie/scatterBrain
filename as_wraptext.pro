FUNCTION as_wraptext, text, width

  CASE StrUpCase(!Version.OS_Family) OF
    'WINDOWS': newline = String([13B,10B])
    'UNIX': newline = String([10B])
     ELSE : newline = String([10B])
  ENDCASE
  newText = ''
  spaces = [StrSplit(text,' '),StrLen(text)]
  atEnd = 0
  loop = 1
  endChar = -1
  While 0 NE 1 DO BEGIN
      startChar = spaces[(Where(spaces GE endChar))[0]]
      endChar = spaces[(Where((!d.X_CH_SIZE*(spaces-startChar)) LT width))[-1]]
      endChar = endChar > spaces[(Where(spaces GT endChar))[0]]
      newText = newText + StrMid(text,startChar, endChar-startChar)
      newText = newText + newline
      IF endChar GE StrLen(text) THEN BEGIN
        BREAK
      ENDIF
  ENDWHILE

  RETURN, newText

END