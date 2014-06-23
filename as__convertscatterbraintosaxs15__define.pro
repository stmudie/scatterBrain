FUNCTION as__convertscatterbraintosaxs15::init, xmlFile

  IF xmlFile EQ !NULL THEN xmlFile = ''
  IF File_Test(xmlFile) EQ 0 THEN xmlFile = Dialog_Pickfile()

  if xmlFile EQ 'Cancel' THEN RETURN, 0

  self.expobj = as_scatterxmlfile()
  self.expobj->ParseFile, xmlFile
  
  Return, 1

END

PRO as__convertscatterbraintosaxs15::createlog

  self.expobj.GetParameters, RAWLOG = log
  
  OPENW, unit, Dialog_Pickfile(), /GET_LUN
  
    FOREACH logline, log DO BEGIN
      PrintF, unit, StrJoin([File_Basename(logline.logline),logline.exptime,logline.I0,'0',logline.IT,'0',logline.IBS,'0'],' ')
    ENDFOREACH
  
  Free_LUN, unit
  
END

PRO as__convertscatterbraintosaxs15::createSAXS

  self.expobj.GetParameters, beamstop = beamstop, frame = frame

  config = 0

  CD, current = current
  saxsFile = Dialog_Pickfile(PATH = current, FILTER = ['*.sax'], DEFAULT_EXTENSION = 'sax', GET_PATH = path)
  CD, path
  
  IF saxsFile EQ '' THEN RETURN

  OPENW, unit, saxsFile , /GET_LUN
  
    IF frame[config].Detector EQ 'Pilatus 1M' THEN det = 'PIL1000K'
    PrintF, unit, 'FRTYPE  ' + det
    PrintF, unit, 'FRFRMT  ' + det

    PrintF, unit, 'FRNFMT  %s%s%4.4d.%s'
    PrintF, unit, 'FRFEXT  tif'
    PrintF, unit, 'FRPSIZ ' + StrCompress(frame[config].psize)
    PrintF, unit, 'FRNCHX ' + StrCompress(frame[config].nxchip)
    PrintF, unit, 'FRNCHY ' + StrCompress(frame[config].nychip)
    PrintF, unit, 'FRNXPX ' + StrCompress(frame[config].nxchip)
    PrintF, unit, 'FRPBIN  1'
    PrintF, unit, 'FRNXPY ' + StrCompress(frame[config].nychip)
    PrintF, unit, 'FRLENG ' + StrCompress(frame[config].len)
    PrintF, unit, 'FRWLEN ' + StrCompress(frame[config].wlen)
    PrintF, unit, 'FRXCEN ' + StrCompress(frame[config].xc)
    PrintF, unit, 'FRYCEN ' + StrCompress(frame[config].yc)
        
  Free_LUN, unit

END

PRO  as__convertscatterbraintosaxs15__define

  void = {as__convertscatterbraintosaxs15, $
             expobj : Obj_New() $
         }

END