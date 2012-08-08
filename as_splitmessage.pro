FUNCTION as_splitmessage, messageInput, xsize

  messageLines = StrSplit(messageInput, String(Byte([13B])),/EXTRACT)

  newMessage = ''
  
  FOREACH message, messageLines DO BEGIN
  
    message = message[0]
    spaces = [StrSplit(message,' '),StrLen(message)]
    atEnd = 0
    loop = 1
  
     endChar = -1
     While 0 NE 1 DO BEGIN
      startChar = spaces[(Where(spaces GE endChar))[0]]
      endChar = spaces[(Where((!d.X_CH_SIZE*(spaces-startChar)) LT xsize))[-1]]
      endChar = endChar > spaces[(Where(spaces GT endChar))[0]]
      newMessage = newMessage + StrMid(message,startChar, endChar-startChar)
      newMessage = newMessage + String([13B])
      IF endChar GE StrLen(message) THEN BEGIN
        BREAK
      ENDIF
    ENDWHILE

  ENDFOREACH

  newMessage = StrMid(newMessage[0],0,StrLen(newMessage[0])-1)

  RETURN, newMessage

END