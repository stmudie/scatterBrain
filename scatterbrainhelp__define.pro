FUNCTION scatterBrainHelp::init, _REF_EXTRA = extra

  @as_scatterheader.macro

  self.HelpList = 'Overview,Frame_Window,Plot_Control,About,Version_Notes'
  
  RETURN, self.as_helpfile::init(_EXTRA = extra)

END

PRO scatterBrainHelp::Overview

  @as_scatterheader.macro

  helpString = 'scatterBrain is a data acquisition and reduction application for ' + $
               'SAXS/WAXS beamlines. It performs azimuthal integration from 2D ' + $
               'images, and can account for various geometries.'
  
  self.Display, helpString

END

PRO scatterBrainHelp::Frame_Window

  @as_scatterheader.macro

  helpString = [                                                           $
               '*** FRAME WINDOW ***'                                    , $
               '~~~~~~~~~~~~~~~~~~~'                                     , $
               'IMAGE KEYBOARD/MOUSE CONTROLS:'                          , $
               'Zoom In ........................................... Ctrl + Left Mouse Button Drag Box', $
               'Zoom Out ........................................ Ctrl + Right Mouse Button Click'  , $
               'Place Q-Cursor .............................. Shift + Left Click'                  , $
               ''                                                        , $
               'MASK KEYBOARD/MOUSE CONTROLS:'                           , $
               '(These Key combinations only available when "Define Masks" Check box is checked on Mask tab)' , $
               ''                                                        , $
               'Select Mask .................................... Alt + Left Mouse Button Click'           , $
               'Move Mask ..................................... Alt + Left Mouse Button Drag'              , $
               'Select Vertex .................................. Middle Mouse Button Click'             , $
               ''                                                        , $
               '--- For Polygon Mask Type:'                                  , $
               'Move Selected Vertex .................... Right Mouse Button Click at new position', $
               'Add Vertex in selected segment ... Left Mouse Button Click at new position', $
               ''                                                       , $
               '--- For Circle Mask Type:'                                   , $
               'Change Radius ............................... Alt + Middle Mouse Button Drag'        , $
               'Change Annulus Thickness ........... Alt + Right Mouse Button Drag' ]
               
               
  self.Display, helpString

END

PRO scatterBrainHelp::Plot_Control

  @as_scatterheader.macro

  helpString = [                                                      $
               '*** PLOT CONTROL ***'                               , $
               '~~~~~~~~~~~~~~~~'                                   , $
               'Keyboard/Mouse Controls:'                           , $
               'Zoom In ............... Left Mouse Button Drag Box' , $
               'Zoom Out ............ Right Mouse Button Click'     , $
               'Place Q-Cursor .... Shift + Left Click']
  
  self.Display, helpString

END

PRO scatterBrainHelp::About

  @as_scatterheader.macro

  helpString = 'About'
  self.Display, helpString

END

PRO scatterBrainHelp::Version_Notes

  @as_scatterheader.macro

  helpString = AS__SaxsShowVersion()
  self.Display, helpString

END

PRO scatterBrainHelp__Define

  void = {scatterBrainHelp, $
          INHERITS as_helpfile }
  
END