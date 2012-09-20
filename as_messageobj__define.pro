FUNCTION as_messageObj::init, COLOR = colour, LOCATIONS = locations, CHAR_DIMENSIONS = charDimensions, VERTICAL_ALIGNMENT = vertAlignment

  @as_scatterheader.macro

  IF KeyWord_Set(colour) THEN self.colour = colour ELSE self.colour = [0,0,0]
  IF KeyWord_Set(locations) THEN self.locations = locations ELSE self.locations = [0,0,0]
  IF KeyWord_Set(charDimensions) THEN self.charDimensions = charDimensions ELSE self.charDimensions = [20,20]
  IF KeyWord_Set(vertAlignment) THEN self.vertAlignment = vertAlignment ELSE self.vertAlignment = 0

  self.textIDs = Ptr_New(/ALLOCATE_HEAP)
  RETURN, self->IDLgrModel::Init()

END

PRO as_messageObj::SetProperty, $
  LOCATIONS = locations, $
  COLOR = colour,$
  CHAR_DIMENSIONS = charDimensions, $
  VERTICAL_ALIGNMENT= vertAlignment, $
  _Ref_extra = extra

  @as_scatterheader.macro

  IF N_Elements(locations) GT 0 THEN self.locations = locations
  IF N_Elements(colour) GT 0 THEN self.colour = colour
  IF N_Elements(charDimensions) GT 0 THEN self.charDimensions = charDimensions
  IF N_Elements(vertAlignment) GT 0 THEN self.vertAlignment = vertAlignment
  
  IF self.count() GT 0 THEN FOREACH m, self.get(/ALL) DO m.SetProperty, LOCATIONS = self.locations, COLOR = self.colour, CHAR_DIMENSIONS = self.charDimensions, VERTICAL_ALIGNMENT = self.vertAlignment

  self->IDLgrModel::SetProperty, _EXTRA = extra

END

PRO as_messageObj::AddMessage, ID, message, NO_CHANGE = noChange

  @as_scatterheader.macro

  messageNum = Where(*self.textIDs EQ ID)
  IF messageNum GE 0 THEN BEGIN
    IF ~KeyWord_Set(noChange) THEN (self.get(POSITION = messageNum)).SetProperty, STRINGS=message
  ENDIF ELSE BEGIN
    text = IDLgrText(message, LOCATIONS = self.locations-[0,25*N_Elements(*self.textIDs),0], COLOR = self.colour, CHAR_DIMENSIONS = charDimensions, VERTICAL_ALIGNMENT = vertAlignment)
    self.add, text
    *self.textIDs = [*self.textIDs,ID]
  ENDELSE
  
END

PRO as_messageObj::DeleteMessage, ID

  @as_scatterheader.macro
  
  messageNum = Where(*self.textIDs EQ ID, COMPLEMENT=otherMessages)
  IF messageNum GE 0 THEN BEGIN
    self.remove, POSITION = messageNum
    IF otherMessages NE -1 THEN *self.textIDs = (*self.textIDs)[otherMessages] ELSE *self.textIDs = !NULL
  ENDIF
  
END

PRO as_messageObj__define

  void = {as_messageObj, $
          INHERITS IDLgrModel, $
          locations      : FltArr(3), $
          colour         : IntArr(3), $
          charDimensions : FltArr(2), $
          vertAlignment  : 0.0, $
          textIDs        : Ptr_New()}
          

END