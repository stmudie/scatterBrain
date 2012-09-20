FUNCTION NOTIFY::init, method, object, data

  COMPILE_OPT idl2
  DEFSYSV, '!debug', EXISTS = debug
  IF ~debug THEN ON_ERROR, 2

  IF Obj_Valid(object) THEN self.object = object[0]
  IF TypeName(method) EQ 'STRING' THEN self.method = method[0]
  IF TypeName(data) EQ 'HASH' THEN self.data = data

  RETURN, 1

END

FUNCTION NOTIFY::validMethod, QUIET = quiet

  COMPILE_OPT idl2
  DEFSYSV, '!debug', EXISTS = debug
  IF ~debug THEN ON_ERROR, 2

  status = !NULL
  IF Obj_Valid(self.object) THEN BEGIN
    IF Obj_HasMethod(self.object, self.method) THEN status = 1 ELSE status = -2 
  ENDIF ELSE status = -1

  IF KeyWord_Set(quiet) THEN RETURN, status
  CASE status OF 
    -1 : result = Dialog_Message('Invalid Object.', /ERROR)
    -2 : result = Dialog_Message('Valid object, but invalid method.', /ERROR)
   ELSE:
  ENDCASE

  RETURN, status

END

PRO NOTIFY::notify, event

  COMPILE_OPT idl2
  DEFSYSV, '!debug', EXISTS = debug
  IF ~debug THEN ON_ERROR, 2

  IF self.validMethod() NE 1 THEN RETURN
  IF TypeName(self.data) NE 'HASH' THEN $
    Call_Method, self.method, self.object, event ELSE $
      Call_Method, self.method, self.object, event, DATA = self.data

END

PRO NOTIFY::SetProperty, $
  OBJECT = object, $
  METHOD = method, $
  DATA = data

  COMPILE_OPT idl2
  DEFSYSV, '!debug', EXISTS = debug
  IF ~debug THEN ON_ERROR, 2

  IF Obj_Valid(object) THEN self.object = object[0]
  IF TypeName(method) EQ 'STRING' THEN self.method = method[0]
  IF TypeName(data) EQ 'HASH' THEN self.data = data[0]

END

PRO NOTIFY::GetProperty, $
  OBJECT = object, $
  METHOD = method, $
  DATA = data

  COMPILE_OPT idl2
  DEFSYSV, '!debug', EXISTS = debug
  IF ~debug THEN ON_ERROR, 2

  IF Arg_Present(object) THEN object = self.object
  IF Arg_Present(method) THEN method = self.method
  IF Arg_Present(data)   THEN data = self.data

END

PRO NOTIFY__Define

  void = {notify, $
          INHERITS IDL_Object, $
          object: Obj_New(), $
          method : '', $
          data: Hash()}
  
END