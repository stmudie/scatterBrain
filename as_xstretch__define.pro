;+
; NAME:
;       AS_XSTRETCH
;
; PURPOSE:
;
;       The purpose of this program is to allow the user to perform a variety
;       of image pixel transformations, commonly known as "contrast stretching".
;       The program supports the following stretches:
;
;       LINEAR         Linear stretch between end points.
;       LINEAR 2%      Linear, except 2% of pixels are clipped at either end of histogram.
;       GAMMA          An exponential function.
;       LOG            An S-shaped log function.
;       ASINH          An inverse hyperbolic sine function (strong log function).
;       SQUARE ROOT    Another type of log function.
;       EQUALIZATION   Image histogram is equalized before stretching.
;       GAUSSIAN       A gaussian normal distribution function.
;
;       An image histogram is provided to the user as an aid in manipulating
;       the stretch parameters.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
;       Some changes made by Stephen T. Mudie, Australian Synchrotron.
;
; CATEGORY:
;
;       Image Processing, Widgets
;
; CALLING SEQUENCE:
;
;       XSTRETCH, image
;
; INPUT PARAMETERS:
;
;       image:         The image data to be stretched. It must be 2D array or a
;                      pointer to a 2D array.
;
; KEYWORD PARAMETERS:
;
;       BETA:          The "softening parameter" associated with the ASINH stretch. (See ASINHSCL for
;                      details.) Set to 3 by default.
;
;       BLOCK:         Set this keyword if you wish the program to be a blocking widget.
;
;       COLORS:        A five element string array, listing the FSC_COLORS colors for drawing the
;                      histogram plot. The colors are used as follows:
;
;                      colors[0] : Background color. Default: "ivory".
;                      colors[1] : Axis color. Default: "charcoal".
;                      colors[2] : Min threshold color. Default: "firebrick".
;                      colors[3] : Max threshold  color. Default: "steel blue".
;                      colors[4] : ASinh color. Default: "sea green".
;                      colors[5] : Histogram color. Default: "charcoal".
;
;                      If a particular color is represented as a null string, then the
;                      default for that color is used.
;
;       COLORTABLE:    The index of a colortable you would like to load.
;                      A gray-scale colortable is used if this parameter is not provided.
;
;       EXPONENT:      The "exponent" parameter of a LOG stretch. (See LOGSCL for details.)
;                      Set to 4.0 by default.
;
;       _EXTRA:        This keyword collects any keyword appropriate for the
;                      Plot command, which is used to display the image histogram.
;
;       FILENAME:      If no image is supplied as a positional parameter, this keyword can be
;                      used to specify the name of an image file. The image must be capable of
;                      being read by SELECTIMAGE, so that means these kinds of files with these
;                      file extensions:
;
;                      TYPE      FILE EXTENSION
;                      BMP       *.bmp
;                      DICOM     *.dcm
;                      FITS      *.fits, *.fts (requires NASA ASTRO library on IDL Path)
;                      GIF       *.gif (IDL 6.2 and higher)
;                      JPEG      *.jpg, *.jpeg, *.jpe
;                      JPEG2000  *.jpf, *.jpx, *.jp2, *.j2c, *.j2k
;                      PICT      *.pict
;                      PNG       *.png
;                      TIFF      *.tif, *tiff
;
;       GAMMA:         The gamma scale factor. (See GMASCL for details.) Set to 1.5 by default.
;
;       GROUP_LEADER:  Keyword to assign a group leader (so this program can be
;                      called from within another widget program).
;       MAXTHRESH:
;
;       MAX_VALUE:     Keyword to assign a maximun value for the normalized Histogram Plot.
;                      Images with lots of pixels of one color (e.g. black) skew
;                      the histogram. This helps make a better looking plot. Set by default
;                      to the maximum value of the histogram data.
;
;       MEAN:          The "mean" parameter of a LOG stretch. (See LOGSCL for details.)
;                      Set to 0.5 by default.
;
;       MINTHRESH:
;
;       NEGATIVE:      Set this keyword if you prefer to see a negative image, rather
;                      that the normal positive image.
;
;       NO_WINDOW:     Set this keyword if you do no want the program to display an
;                      image window. This would be the case, for example, if you
;                      are displaying the image in your own window and your program
;                      is being notified of images changes via the NOTIFY_PRO or
;                      NOTIFY_OBJ keywords.
;
;       NOTIFY_OBJ:    Set this keyword to a structure containing the fields OBJECT
;                      and METHOD. When the image is changed, the object identified in
;                      the OBJECT field will have the method identified in the METHOD
;                      field called. The method should be written to accept one positional
;                      parameter. The parameter passed to the method is a structure as defined
;                      below.
;
;       NOTIFY_PRO:    Set this keyword to the name of a procedure that should
;                      be notified when the image is changed. The procedure should
;                      be defined with one positional parameter. The parameter passed
;                      to the procedure is a structure defined as below.
;
;       SIGMA:         The amount of width applied to the Gaussian stretch. Default is 1.
;
;       TITLE:         The title of the histogram window. By default: 'Drag Vertical Lines to STRETCH Image Contrast'.
;
;       TYPE:          The type of stretch to be applied. May be either a string (e.g, 'GAMMA') or a number from
;                      the table below:
;
;           Number   Type of Stretch
;             0         Linear         scaled = BytScl(image, MIN=minThresh, MAX=maxThresh)
;             1         Gamma          scaled = GmaScl(image, MIN=minThresh, MAX=maxThresh, Gamma=gamma)
;             2         Log            scaled = LogScl(image, MIN=minThresh, MAX=maxThresh, Mean=mean, Exponent=exponent)
;             3         Asinh          scaled = AsinhScl(image, MIN=minThresh, MAX=maxThresh, Beta=beta)
;             4         Linear 2%      A linear stretch, with 2 percent of pixels clipped at both the top and bottom
;             5         Square Root    A linear stretch of the square root histogram of the image values.
;             6         Equalization   A linear stretch of the histogram equalized image histogram.
;             7         Gaussian       A Gaussian normal function is applied to the image histogram.
;
;       UPDATECBDATA:  Variables to be passed to the call back function.
;
;       XPOS:          The X position of the histogram window in pixels from upper-left
;                      of display. By default, 100.
;  
;       XSIZE:         The X size of the histogram window.
;  
;       YPOS:          The Y position of the histogram window in pixels from upper-left
;                      of display. By default, 100.
;
;       YSIZE:         The Y size of the histogram window.
;
; OUTPUTS:
;
;       The image and histogram windows can be save as BMP, JPEG, PNG, PICT, TIFF, GIF, and PostScript output
;       files from the Control pull-down menu.
;
;       The stretched image, the stretched image histogram (in 256-element format, suitable for
;       input into HISTOMATCH), and the current stretch parameters can be saved as main-level IDL
;       variables from the Control pull-down menu. If you choose to save "parameters", a structure
;       of this form is saved:
;
;               struct = { minThresh: info.minThresh, $
;                          maxThresh: info.maxThresh, $
;                          gamma: info.gamma, $
;                          beta: info.beta, $
;                          mean: info.mean, $
;                          exponent: info.exponent, $
;                          type: info.type }
;
;      The TYPE field is a string that reflects the current stretch type at the time the
;      parameters were saved. This should be used to tell you which other fields in the structure
;      are pertinent for a stretch operation.
;
;      If you choose to save "everything", the saved variable is a structure similar to the one above,
;      except there are two additional fields: (1) IMAGE contains the stretched image, and HISTOGRAM
;      contains the stretch image histogram.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; NOTIFICATION STRUCTURE:
;
;       If you choose to be notified of changes in the image (NOTIFY_PRO or NOTIFY_OBJ),
;       the following structure will be sent as the first positional parameter:
;
;    struct = { image: stretchedImage, $       ; The stretched image.
;               r: info.r, $                   ; The R color vector associated with the image
;               g: info.g, $                   ; The G color vector associated with the image
;               b: info.b, $                   ; The B color vector associated with the image
;               type: info.type, $             ; The TYPE of stretch applied to the image.
;               minThresh: info.minThresh, $   ; The minimum threshold value.
;               maxThresh: info.maxThresh, $   ; The maximum threshold value.
;               beta: info.beta, $             ; The current BETA value.
;               gamma: info.gamma, $           ; The current GAMMA value.
;               mean: info.mean, $             ; The current MEAN value.
;               exponent: info.exponent, $     ; The current EXPONENT value.
;               sigma: info.sigma, $           ; The current SIGMA value.
;               hist_win: info.Histo_tlb  }    ; The widget ID of the base - so it can be killed from other programs.
;
; DEPENDENCIES:
;
;       Requires a number of files from the Coyote Library:
;
;       http://www.dfanning.com/programs/xstretch.zip
;          or
;       http://www.dfanning.com/programs/coyoteprograms.zip
;
;
; EXAMPLE:
;
;       If you have a 2D image in the variable "image", you can run this
;       program like this:
;
;       XSTRETCH, image
;       XSTRETCH, image, TYPE='GAMMA'
;       XSTRETCH, image, TYPE='LOG', EXPONENT=5.5
;       XSTRETCH, image, TYPE='ASINH', BETA=0.1
;
;       For a good ASINH image, try the FITS files located here:
;
;           http://cosmo.nyu.edu/hogg/visualization/rgb/
;
; MODIFICATION HISTORY:
;
;       Written by: David W. Fanning, April 1996.
;       October, 1996 Fixed a problem with not restoring the color
;          table when the program exited. Substituted a call to XCOLORS
;          instead of XLOADCT.
;       October, 1998. Added NO_BLOCK keyword and modified to work with
;          24-bit color devices.
;       April, 1999. Made lines thicker. Offered default image. DWF.
;       April, 1999. Replaced TV command with TVIMAGE. DWF.
;       April, 1999. Made both windows resizeable. DWF.
;       April, 2000. Made several modifications to histogram plot and to
;          the way colors were handled. Added ability to pass pointer to
;          the image as well as image itself. DWF.
;       February 2001. Removed GIF file support for IDL 5.4 and fixed
;          a problem with cleaning up the pixmap. DWF.
;       October 29, 2002. Added ability to load an image file with GETIMAGE. DWF.
;       Added ability to store stretched image as main-level variable. Oct 30, 2002. DWF.
;       Fixed a problem with the image window aspect ratio being calculated
;          incorrectly. 2 Nov 2002. DWF.
;       Added ability to open formatted images as well as raw data files. 2 Nov 2002. DWF.
;       Fixed a couple of minor problems with resizing the histogram window. 4 Nov 2002. DWF.
;       Added NO_WINDOW and NOTIFY_PRO keywords. 4 Nov 2002. DWF.
;       Fixed a problem with the histogram plot when the minimum image value
;          is greater than 0. 8 Nov 2002. DWF.
;       Added NOTIFY_OBJ and BLOCK keywords. 16 Nov 2002. DWF.
;       Fixed some problems in which images that are NOT byte valued could not be
;          displayed property. 29 Dec 2005. DWF.
;       Added GIF images back in. 29 Dec 2005. DWF.
;       EXTENSIVE modifications, primarily so it would work perfectly with perverse
;          FITS images. 20 Feb. 2006. DWF.
;       Added NEGATIVE keyword. 20 Feb. 2006. DWF.
;       Added GAMMA stretch. 20 Feb. 2006. DWF.
;       Added LOG and ASINH stretches. 25 Feb. 2006. DWF.
;       Added ability to save the stretched image, the stretched image histogram, and
;          image stretch parameters as main-level IDL variables. 1 March 2006. DWF.
;       Removed restrictions for 2D images. Now allows 24-bit images (MxNx3) to
;          be loaded. 3 March 2006.
;       Modified the histogram plot xrange to use the output min and max from
;          the histogram. 3 March 2006.
;       Renamed IMGSCL to GMASCL to avoid name space conflicts. 8 March 2006. DWF.
;       Mofications to GUI for updated ASINHSCL function. Removed ALPHA keyword
;          and changed the definition of BETA. 25 April 2006. DWF.
;       Added LINEAR 2%, SQUARE ROOT, GAUSSIAN and EQUALIZATION stretches. 4 Sept 2007. DWF
;       Added updatecbdata keyword to allow passing of variables to call back routine, and
;          also added tlb ID to return structure so it can be killed programmatically. 13 June 2008. STM
;-
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright � 1996-2007 Fanning Software Consulting.
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################
PRO AS_XSTRETCH_PARAMETERS, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->PARAMETERS, event
END

PRO AS_XSTRETCH_PROCESS_EVENTS, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->PROCESS_EVENTS, event
END

PRO AS_XSTRETCH_MOVELINE, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->MOVELINE, event
END

PRO AS_XSTRETCH_ZOOM, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->ZOOM, event
END

PRO AS_XStretch_STRETCHTYPE, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->STRETCHTYPE, event
END

PRO AS_XSTRETCH_IMAGEWINDOWKILLED, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->IMAGEWINDOWKILLED, event
END

PRO AS_XSTRETCH_SAVETOMAIN, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->SAVETOMAIN, event
END

PRO AS_XSTRETCH_FLIPIMAGE, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->FLIPIMAGE, event
END

PRO AS_XSTRETCH_NEGATIVE, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->NEGATIVE, event
END

PRO AS_XSTRETCH_OPENIMAGE, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->OPENIMAGE, event
END

PRO AS_XSTRETCH_SAVEAS, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->SAVEAS, event
END

PRO AS_XSTRETCH_SAVEHISTOAS, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->SAVEHISTOAS, event
END

PRO AS_XSTRETCH_SETTHRESHOLD, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->SETTHRESHOLD, event
END

PRO AS_XSTRETCH_PRINT, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->PRINT, event
END

PRO AS_XSTRETCH_QUIT, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->QUIT, event
END

PRO AS_XSTRETCH_COLORS, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->COLORS, event
END

PRO AS_XSTRETCH_MAXVALUE, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->MAXVALUE, event
END

PRO AS_XSTRETCH_IMAGE_RESIZE, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  AS_XSTRETCH->IMAGE_RESIZE, event
END

PRO AS_XSTRETCH_HISTOGRAM_RESIZE, event
  Widget_Control, event.top, GET_UVALUE = AS_XSTRETCH 
  IF Tag_Names(event, /STRUCTURE_NAME) EQ 'WIDGET_KILL_REQUEST' THEN AS_XSTRETCH->HistHide, 1 $
                                                                ELSE AS_XSTRETCH->HISTOGRAM_RESIZE, event
END

PRO AS_XSTRETCH::UpdateHisto

  self->HISTOPLOT, WID = self.histo_wid, maxValue = self.maxValue
  self->NotifyOthers
END

FUNCTION AS_XSTRETCH::VALIDATE_THRESHOLD, threshold

  ; Catch any errors.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, Cancel=1
    void = Error_Message(/Traceback)
    RETURN, threshold
  ENDIF
  
  ; Make sure threshold is inside of the plot.
  threshold = self.xmin > threshold < self.xmax
  
  ; Make sure threshold has the same type as the image, unless SQUARE ROOT stretch.
  IF self.type NE 'SQUARE ROOT' THEN $
    threshold = Convert_to_Type(threshold, Size(*self.image, /Type))
  IF Size(threshold, /TNAME) EQ 'BYTE' THEN threshold = Fix(threshold)
  
  RETURN, threshold
END ;--------------------------------------------------------------------

FUNCTION AS_XSTRETCH::SCALEIMAGE

  ; Scales the image data appropriately, depending on scale type.

  ; Catch any errors.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, Cancel=1
    void = Error_Message(/Traceback)
    RETURN, *self.image
  ENDIF
  
  ; Turn floating underflow warnings off.
  thisExcept = !Except
  !Except = 0
  
  CASE self.type OF
  
    'LINEAR': BEGIN
      scaledImage = BytScl(*self.image, Max=self.maxThresh, Min=self.minThresh, /NAN)
      IF self.negative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
    END
    
    'LINEAR 2%': BEGIN
      scaledImage = BytScl(*self.image, Max=self.maxThresh, Min=self.minThresh, /NAN)
      IF self.negative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
    END
    
    '^0.2' : BEGIN
      scaledImage = BytScl((*self.image)^.2, Max=self.maxThresh^.2, Min=self.minThresh^.2, /NAN)
      IF self.negative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
    END
    
    'EQUALIZATION': BEGIN
      scaledImage = BytScl(Hist_Equal(*self.image), Max=self.maxThresh, Min=self.minThresh, /NAN)
      IF self.negative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
    END
    
    'GAMMA': BEGIN
      scaledImage = GmaScl(*self.image, Max=self.maxThresh, Min=self.minThresh, $
        Gamma=self.gamma, Negative=self.negative)
      RETURN, scaledImage
    END
    
    'GAUSSIAN': BEGIN
      scaledImage = GaussScl(*self.image, Max=self.maxThresh, Min=self.minThresh, $
        Sigma=self.sigma, Negative=self.negative)
      RETURN, scaledImage
    END
    
    'SQUARE ROOT': BEGIN
      scaledImage = BytScl(SQRT(*self.image), Max=self.maxThresh, Min=self.minThresh, /NAN)
      IF self.negative THEN RETURN, 255B - scaledImage ELSE RETURN, scaledImage
      RETURN, scaledImage
    END
    
    'LOG': BEGIN
      scaledImage =  LogScl(*self.image, Max=self.maxThresh, Min=self.minThresh, $
        Mean=self.mean, Exponent=self.exponent, Negative=self.negative)
      RETURN, scaledImage
    END
    
    'ASINH' :BEGIN
    scaledImage = ASinhScl(*self.image, Max=self.maxThresh, Min=self.minThresh, $
      BETA=self.beta, Negative=self.negative)
    RETURN, scaledImage
  END
  
ENDCASE

; Turn warning back on.
void = Check_Math()
!Except = thisExcept

END ;--------------------------------------------------------------------



PRO AS_XSTRETCH::IMAGEWINDOWKILLED, imageWindowID

  ; Turn the Save As, Print, and Image Colors buttons off.

  Widget_Control, imageWindowID, Get_UValue=buttonIDs
  IF Widget_Info(buttonIDs[0], /Valid_ID) THEN BEGIN
    Widget_Control, buttonIDs[0], Sensitive=0
    Widget_Control, buttonIDs[1], Sensitive=0
    Widget_Control, buttonIDs[2], Sensitive=0
  ENDIF
  
END ;--------------------------------------------------------------------



PRO AS_XSTRETCH::SAVETOMAIN, event

  ; Handle events from the SAVE to MAIN LEVEL buttons.

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    void = Error_Message()
    RETURN
  ENDIF
  
  Widget_Control, event.id, Get_UValue=buttonValue
  CASE buttonValue OF
  
    'IMAGE': BEGIN
    
      varname = TextBox(Title='Provide Main-Level Variable Name...', Group_Leader=event.top, $
        Label='Image Variable Name: ', Cancel=cancelled, XSize=200, Value='stretched_image')
        
      ; The ROUTINE_NAMES function is not documented in IDL,
      ; so it may not always work. This capability has been
      ; tested in IDL versions 5.3 through 5.6 and found to work.
      ; People with IDL 6.1 and higher should use SCOPE_VARFETCH to
      ; set main-level variables. I use the older, undocumented version
      ; to stay compatible with more users.
        
      IF NOT cancelled THEN BEGIN
        displayImage = self->ScaleImage()
        dummy = Routine_Names(varname, displayImage, Store=1)
      ENDIF
    END
    
    'HISTOGRAM': BEGIN
    
      varname = TextBox(Title='Provide Main-Level Variable Name...', Group_Leader=event.top, $
        Label='Histogram Variable Name: ', Cancel=cancelled, XSize=200, Value='stretched_histogram')
        
      ; The ROUTINE_NAMES function is not documented in IDL,
      ; so it may not always work. This capability has been
      ; tested in IDL versions 5.3 through 5.6 and found to work.
      ; People with IDL 6.1 and higher should use SCOPE_VARFETCH to
      ; set main-level variables. I use the older, undocumented version
      ; to stay compatible with more users.
        
      IF NOT cancelled THEN BEGIN
      
        ; Calculate binsize.
        displayImage = self->ScaleImage()
        maxval = Max(displayImage, MIN=minval)
        range = maxval - minval
        IF Size(displayImage, /TName) EQ 'BYTE' THEN binsize = 1.0 ELSE binsize = range / 256.
        
        ; Normalized pixel density.
        histdata = Histogram(displayImage, Binsize=binsize)
        
        dummy = Routine_Names(varname, histdata, Store=1)
      ENDIF
    END
    
    'PARAMETERS': BEGIN
    
      varname = TextBox(Title='Provide Main-Level Variable Name...', Group_Leader=event.top, $
        Label='Parameter Structure Name: ', Cancel=cancelled, XSize=200, Value='stretched_params')
        
      ; The ROUTINE_NAMES function is not documented in IDL,
      ; so it may not always work. This capability has been
      ; tested in IDL versions 5.3 through 5.6 and found to work.
      ; People with IDL 6.1 and higher should use SCOPE_VARFETCH to
      ; set main-level variables. I use the older, undocumented version
      ; to stay compatible with more users.
        
      IF NOT cancelled THEN BEGIN
      
        struct = { minThresh: self.minThresh, $
          maxThresh: self.maxThresh, $
          gamma: self.gamma, $
          beta: self.beta, $
          mean: self.mean, $
          exponent: self.exponent, $
          type: self.type }
          
        dummy = Routine_Names(varname, struct, Store=1)
      ENDIF
    END
    
    'EVERYTHING': BEGIN
    
      varname = TextBox(Title='Provide Main-Level Variable Name...', Group_Leader=event.top, $
        Label='Stretched Structure Name: ', Cancel=cancelled, XSize=200, Value='stretched_struct')
        
      ; The ROUTINE_NAMES function is not documented in IDL,
      ; so it may not always work. This capability has been
      ; tested in IDL versions 5.3 through 5.6 and found to work.
      ; People with IDL 6.1 and higher should use SCOPE_VARFETCH to
      ; set main-level variables. I use the older, undocumented version
      ; to stay compatible with more users.
        
      IF NOT cancelled THEN BEGIN
      
        displayImage = self->ScaleImage()
        
        ; Calculate binsize.
        maxval = Max(displayImage, MIN=minval)
        range = maxval - minval
        IF Size(displayImage, /TName) EQ 'BYTE' THEN binsize = 1.0 ELSE binsize = range / 256.
        
        ; Normalized pixel density.
        histdata = Histogram(displayImage, Binsize=binsize)
        
        struct = { minThresh: self.minThresh, $
          maxThresh: self.maxThresh, $
          gamma: self.gamma, $
          beta: self.beta, $
          mean: self.mean, $
          exponent: self.exponent, $
          type: self.type, $
          image: displayImage, $
          histogram: histdata }
          
        dummy = Routine_Names(varname, struct, Store=1)
      ENDIF
    END
  ENDCASE
  
    
END ;--------------------------------------------------------------------


PRO AS_XSTRETCH::DRAWLINES, minThresh, maxThresh

  ; Procedure to draw threshold lines and asinh function on histogram plot.

  ; Catch any errors.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, Cancel=1
    void = Error_Message(/Traceback)
    RETURN
  ENDIF
  
  ; Make sure you have the latest values of the parameters
  CASE self.type OF
  
    'GAMMA': BEGIN
      theValue = Widget_Info(self.gamma_comboID, /Combobox_GetText)
      self.gamma = Double(theValue)
    END
    
    'GAUSSIAN': BEGIN
      IF N_Elements(self.sigmaObj -> Get_Value()) NE 0 THEN $
        self.sigma = self.sigmaObj -> Get_Value() ELSE self.sigma = 1.0
      self.sigmaObj -> Set_Value, self.sigma
    END
    
    'LOG': BEGIN
      IF N_Elements(self.param1Obj -> Get_Value()) NE 0 THEN $
        self.mean = self.param1Obj -> Get_Value() ELSE self.mean = 0.5
      self.param1Obj -> Set_Value, self.mean
      IF N_Elements(self.param2Obj -> Get_Value()) NE 0 THEN $
        self.exponent = self.param2Obj -> Get_Value() ELSE self.exponent = 4.0
      self.param2Obj -> Set_Value, self.exponent
    END
    
    'ASINH': BEGIN
      theValue = Widget_Info(self.asinh_comboID, /Combobox_GetText)
      self.beta = Double(theValue)
    END
    
    ELSE:
    
  ENDCASE
  
  ; Make histogram window active..
  IF (!D.Flags AND 256) NE 0 THEN WSet, self.histo_wid
  
  ; Draw threshold lines.
  PlotS, [minThresh, minThresh], [!Y.CRange(0), !Y.CRange(1)], $
    Color=FSC_Color(self.colors[2]), Thick=3
  PlotS, [maxThresh, maxThresh], [!Y.CRange(0), !Y.CRange(1)], $
    Color=FSC_Color(self.colors[3]), Thick=3
    
  ; Label the lines.
  cmax = Convert_Coord(maxThresh, 0, /Data, /To_Normal)
  cmin = Convert_Coord(minThresh, 0, /Data, /To_Normal)
  minThresh = self->Validate_Threshold(minThresh)
  maxThresh = self->Validate_Threshold(maxThresh)
  IF self.type EQ 'SQUARE ROOT' THEN BEGIN
    minThresh = Float(minThresh)
    maxThresh = Float(maxThresh)
  ENDIF
  XYOuts, cmin[0], 0.90, /Normal, Number_Formatter(minThresh, Decimals=3), $
    Color=FSC_Color(self.colors[2]), Alignment=1.0, Font=0
  XYOuts, cmax[0], 0.90, /Normal, Number_Formatter(maxThresh, Decimals=3), $
    Color=FSC_Color(self.colors[3]), Alignment=0.0, Font=0
    
  CASE self.type OF
  
    'LINEAR': BEGIN
      line = BytScl(Findgen(101))
      line = as_scale_vector(line, 0.0, !Y.CRange[1])
      x = as_scale_vector(Findgen(101), minThresh, maxThresh)
      OPlot, x, line, Color=FSC_Color(self.colors[4]), LineStyle=2, Thick=2
    END
    
    'LINEAR 2%': BEGIN
      line = BytScl(Findgen(101))
      line = as_scale_vector(line, 0.0, !Y.CRange[1])
      x = as_scale_vector(Findgen(101), minThresh, maxThresh)
      OPlot, x, line, Color=FSC_Color(self.colors[4]), LineStyle=2, Thick=2
    END
    
    '^0.2' : BEGIN
      line = BytScl(Findgen(101))
      line = as_scale_vector(line, 0.0, !Y.CRange[1])
      x = as_scale_vector(Findgen(101), minThresh, maxThresh)
      OPlot, x, line, Color=FSC_Color(self.colors[4]), LineStyle=2, Thick=2
    END
    
    'EQUALIZATION': BEGIN
      line = BytScl(Findgen(101))
      line = as_scale_vector(line, 0.0, !Y.CRange[1])
      x = as_scale_vector(Findgen(101), minThresh, maxThresh)
      OPlot, x, line, Color=FSC_Color(self.colors[4]), LineStyle=2, Thick=2
    END
    
    'LOG': BEGIN
      line = LogScl(Findgen(101), Mean=self.mean, Exponent=self.exponent)
      line = as_scale_vector(line, 0.0, !Y.CRange[1])
      x = as_scale_vector(Findgen(101), minThresh, maxThresh)
      OPlot, x, line, Color=FSC_Color(self.colors[4]), LineStyle=2, Thick=2
    END
    
    'GAMMA': BEGIN
      ; Draw the gamma function.
      line = as_scale_vector(Findgen(101), 0.0, 1.0)
      line = Double(line)^self.gamma
      line = as_scale_vector(line, 0.0, !Y.CRange[1])
      x = as_scale_vector(Findgen(101), minThresh, maxThresh)
      OPlot, x, line, Color=FSC_Color(self.colors[4]), LineStyle=2, Thick=2
    END
    
    'GAUSSIAN': BEGIN
      ; Draw the gaussian function.
      line = as_scale_vector(Findgen(101), -!PI, !PI)
      line = (1/(2*!PI*self.sigma^2))*EXP(-(line^2/(2*self.sigma^2)))
      line = as_scale_vector(line, 0, !Y.CRange[1])
      x = as_scale_vector(Findgen(101), minThresh, maxThresh)
      OPlot, x, line, Color=FSC_Color(self.colors[4]), LineStyle=2, Thick=2
    END
    
    'SQUARE ROOT': BEGIN
      line = BytScl(Findgen(101))
      line = as_scale_vector(line, 0.0, !Y.CRange[1])
      x = as_scale_vector(Findgen(101), minThresh, maxThresh)
      OPlot, x, line, Color=FSC_Color(self.colors[4]), LineStyle=2, Thick=2
    END
    
    'ASINH': BEGIN
      ; Draw the asinh function.
      line = ASinhScl(Findgen(101), BETA=self.beta)
      line = as_scale_vector(line, 0.0, !Y.CRange[1])
      x = as_scale_vector(Findgen(101), minThresh, maxThresh)
      OPlot, x, line, Color=FSC_Color(self.colors[4]), LineStyle=2, Thick=2
    END
    
  ENDCASE
END ;--------------------------------------------------------------------



PRO AS_XSTRETCH::NOTIFYOTHERS

  ; This is the procedure that notifies others of an image change.

  ; Catch any error in the histogram display routine.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, Cancel=1
    void = Error_Message()
    RETURN
  ENDIF
  
  stretchedImage = self->ScaleImage()
  minThresh = self.minThresh
  maxThresh = self.maxThresh
  IF self.type EQ 'SQUARE ROOT' THEN BEGIN
    minThresh = minThresh^2 
    maxThresh = maxThresh^2
  END
  ; The structure parameter.
  struct = { image: Long(stretchedImage), $       ; The stretched image.
    r: self.r, $                   ; The R color vector associated with the image
    g: self.g, $                   ; The G color vector associated with the image
    b: self.b, $                   ; The B color vector associated with the image
    type: self.type, $             ; The TYPE of stretch applied to the image.
    minThresh: minThresh, $        ; The minimum threshold value.
    maxThresh: maxThresh, $        ; The maximum threshold value.
    beta: self.beta, $             ; The current BETA value.
    gamma: self.gamma, $           ; The current GAMMA value.
    mean: self.mean, $             ; The current MEAN value.
    exponent: self.exponent, $     ; The current EXPONENT value.
    sigma: self.sigma, $           ; The current SIGMA value.
    negative: self.negative, $     ; The current negative setting.
    hist_win : self.Histo_tlb, $   ; The widget ID of the base - so it can be killed from calling program
    hist_draw: self.Histo_draw, $  ; The widget ID of the draw widget.
    xZoom : self.xZoom, $
    visible : Widget_Info(self.Histo_tlb, /MAP)}
    
  ; Notify a procedure.
  IF *self.notify_pro NE "" THEN Call_Procedure, *self.notify_pro, struct, self.updatecbdata
  
  ; Notify an objects.
  validObj = Obj_Valid((*self.notify_obj).object)
  FOR i = 0, N_Elements((*self.notify_obj).object) -1 DO BEGIN 
    IF validObj[i] THEN Call_Method, ((*self.notify_obj).method)[i], ((*self.notify_obj).object)[i], struct
  ENDFOR
  
  ; Update self from changes to struct
    IF ~min(Long(stretchedImage) EQ struct.image) THEN *self.image = struct.image           ; The raw image.
    self.r = struct.r                   ; The R color vector associated with the image
    self.g = struct.g                   ; The G color vector associated with the image
    self.b = struct.b                   ; The B color vector associated with the image
    self.type = struct.type             ; The TYPE of stretch applied to the image.
    self.minThresh = minThresh          ; The minimum threshold value.
    self.maxThresh = maxThresh          ; The maximum threshold value.
    self.beta = struct.beta             ; The current BETA value.
    self.gamma = struct.gamma           ; The current GAMMA value.
    self.mean = struct.mean             ; The current MEAN value.
    self.exponent = struct.exponent     ; The current EXPONENT value.
    self.sigma = struct.sigma           ; The current SIGMA value.
    self.negative = struct.negative     ; The current negative setting.
      
END ;--------------------------------------------------------------------


PRO AS_XSTRETCH::HISTOPLOT, $
    WID=wid, $
    MAXVALUE=maxvalue, $
    _Extra=extra
    
  ; This is a utility program to draw a histogram plot in a
  ; display window.
    
  ; Catch any error in the histogram display routine.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, Cancel=1
    void = Error_Message()
    RETURN
  ENDIF
  
  ; Proper number formatting.
  format = '(F6.2)'
  IF maxvalue LT 0.1 THEN  format = '(F6.3)'
  IF maxvalue LT 0.05 THEN format = '(F6.3)'
  
  ; Calculate binsize.
  orderPixelsBinsize=max(*self.image)/1E6 > 1
  hist = histogram(*self.image, binsize = orderPixelsBinsize, REVERSE_INDICES = rl)
  maxr = (Where(rl[1:rl[0]]-rl[0] GT .9998*N_Elements(*self.image)))[0]*orderPixelsbinsize
  minr = (Where(rl[1:rl[0]]-rl[0] GT .0002*N_Elements(*self.image)))[0]*orderPixelsbinsize
  
  IF self.xZoom[1] LT maxr AND self.xZoom[1] GT minr THEN maxr = self.xZoom[1]
  IF self.xZoom[0] GT minr AND self.xZoom[0] LT maxr THEN minr = self.xZoom[0]
  
  IF maxr LE minr THEN BEGIN
    minr = self.minVal
    maxr = self.maxVal
  ENDIF

  IF self.minThresh GT maxr OR self.minThresh LT minr THEN self.minThresh = minr
  IF self.maxThresh GT maxr OR self.maxThresh LT minr THEN self.maxThresh = maxr
  
  
  ;minr = min(*self.image) > self.xZoom[0] > 1
  IF self.type EQ 'SQUARE ROOT' THEN  BEGIN
    maxr = Max(Double(SQRT(*self.image)), MIN=minr, /NAN) < self.xZoom[1]
    minr = self.xZoom[0] > minr
    ;maxr = SqRt(maxr) > 2
    ;minr = SqRt(minr) > 1
  ENDIF; ELSE BEGIN
    ;maxr = Max(Double(*self.image), MIN=minr, /NAN) < self.xZoom[1]
    ;minr = self.xZoom[0] > minr
  ;ENDELSE
  range = maxr - minr > 1
  numType = Size(*self.image, /TName)
  CASE numType OF
    'BYTE' : binsize = 1.0 
    'INT'  : binsize = 1 > Fix(range / 300., TYPE = 3)
    'LONG' : binsize = 1 > Fix(range / 300., TYPE = 3)
  ELSE : binsize = Fix(range / 300., TYPE = 3)
  ENDCASE
  
  IF self.type EQ 'SQUARE ROOT' THEN binsize = range / 300.
  IF self.type EQ 'EQUALIZATION' THEN binsize = 1.0
  IF self.type EQ 'GAUSSIAN' THEN binsize = 1.0
  
  ; Normalized pixel density.
  CASE self.type OF
  
    'EQUALIZATION': BEGIN
      temp_image = *self.image
        histdata = Histogram(Hist_Equal(*self.image), Binsize=binsize, $
        OMIN=omin, OMAX=omax, MAX = maxr, MIN= minr)/Float(N_Elements(*self.image))
      imageTitle = 'Equalized Image Value'
    END
    'SQUARE ROOT': BEGIN
      histdata = Histogram(SQRT(*self.image), Binsize=binsize, $
        OMIN=omin, OMAX=omax, MAX = maxr, MIN= minr)/Float(N_Elements(*self.image))
      imageTitle = 'Square Root of Image Value'
    END
    
    ELSE: BEGIN
      histdata = Histogram(*self.image, Binsize=binsize, OMIN=omin, OMAX=omax, MAX = maxr, MIN= minr)/Float(N_Elements(*self.image))
      imageTitle = 'Image Value'
    END
  ENDCASE
  
  ; Save the current window index.
  cWinID = !D.Window
  
  ; Plot the histogram of the display image.
  IF N_Elements(wid) NE 0 THEN WSet, wid
  bins = Findgen(N_Elements(histdata)) * binsize + omin
  xrange = [omin, omax]
  
  ;p = plot(bins,histdata,/FILL_BACKGROUND)
  
  Plot, bins, histdata, YTitle='Normalized Pixel Density', $
    Background=FSC_Color(self.colors[0]), Color=FSC_Color(self.colors[1]), /NoData, $
    XRange=xrange, XStyle=1, Max_Value=maxValue, $
    XTitle=imageTitle, _Extra=extra, YRange=[0, Max(histdata) < maxValue], $
    YTickformat=format, Position=[0.15, 0.15, 0.85, 0.85]
    
  ; Draw the boxes of the histogram.
  FOR j=0L,N_Elements(bins)-2 DO BEGIN
    PlotS, [bins[j], bins[j]], $
      [0, histdata[j] < !Y.CRange[1]], Color=FSC_Color(self.colors[5])
  ENDFOR
  
  ; Store the plotting system variables for later recall.
  self.pbang = !P
  self.xbang = !X
  self.ybang = !Y
  self.ymin = !Y.CRange[0]
  self.ymax = !Y.CRange[1]
  self.xmin = !X.CRange[0]
  self.xmax = !X.CRange[1]
  
  ; Validate the threshold.
  self.maxThresh = self->Validate_Threshold(self.maxThresh)
  self.minThresh = self->Validate_Threshold(self.minThresh)
  self.minThreshObj -> Set_Value,$
    Number_Formatter(self->VALIDATE_THRESHOLD(self.minThresh), Decimals=3), /FloatValue
  self.maxThreshObj -> Set_Value,$
    Number_Formatter(self->VALIDATE_THRESHOLD(self.maxThresh), Decimals=3), /FloatValue
    
  ; Restore previous graphics window.
  IF cWinID GT 0 THEN IF (!D.Flags AND 256) NE 0 THEN WSet, cWinID
  
END ;--------------------------------------------------------------------------------


PRO AS_XSTRETCH::PARAMETERS, event

  ; Handle events from the log parameter widgets.

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    void = Error_Message()
    RETURN
  ENDIF
  
  ; Make sure you have the latest values for alpha and beta.
  CASE self.type OF
  
    'LOG': BEGIN
      IF N_Elements(self.param1Obj -> Get_Value()) NE 0 THEN $
        self.mean = self.param1Obj -> Get_Value() ELSE self.mean = 0.5
      self.mean = 0.0 > self.mean < 1.0
      self.param1Obj -> Set_Value, self.mean
      IF N_Elements(self.param2Obj -> Get_Value()) NE 0 THEN $
        self.exponent = self.param2Obj -> Get_Value() ELSE self.exponent = 4.0
      self.param2Obj -> Set_Value, self.exponent
    END
    
    'ASINH': BEGIN
      theText = Widget_Info(self.asinh_comboID, /Combobox_GetText)
      self.beta = Float(theText)
    END
    
    'GAMMA': BEGIN
      theText = Widget_Info(self.gamma_comboID, /Combobox_GetText)
      self.gamma = Float(theText)
    END
    
    'GAUSSIAN': BEGIN
      IF N_Elements(self.sigmaObj -> Get_Value()) NE 0 THEN $
        self.sigma = self.sigmaObj -> Get_Value() ELSE self.sigma = 1.0
      self.sigmaObj -> Set_Value, self.sigma
    END
    
    ELSE:
    
  ENDCASE
  
  ; Display the image after thresholding.
  displayImage = self->ScaleImage()
  IF NOT self.no_window THEN BEGIN
    WSet, self.windex
    WShow, self.windex
    TVLCT, self.r, self.g, self.b
    TVImage, displayImage, /NoInterp
  ENDIF
  self->NotifyOthers
  
  ; Copy histogram from pixmap.
  WSet, self.histo_wid
  Device, Copy=[0, 0, self.pix_xsize, self.pix_ysize, 0, 0, self.pixmap]
  
  ; Draw threshold lines.
  self->DrawLines, self.minThresh, self.maxThresh
  
    
  
END ;--------------------------------------------------------------------


PRO AS_XSTRETCH::FLIPIMAGE, event

  ; Handle events from the "Flip Image" button.

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    void = Error_Message()
    RETURN
  ENDIF
  
  ; Switch the value of the button.
  *self.image = Reverse(*self.image, 2)
  
  ; Display the image after thresholding.
  displayImage = self->ScaleImage()
  IF NOT self.no_window THEN BEGIN
    WSet, self.windex
    WShow, self.windex
    TVLCT, self.r, self.g, self.b
    TVImage, displayImage, /NoInterp
  ENDIF
  self->NotifyOthers
  
  
  
END ;--------------------------------------------------------------------------------

PRO AS_XSTRETCH::GAMMA, event

  ; Handler events from the GAMMA pull-down menu.

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN
  ENDIF
  
  Widget_Control, event.id, Get_UValue=gamma
  self.gamma = gamma
  Widget_Control, self.cgammaID, Set_Button=0
  self.cgammaID = event.id
  Widget_Control, event.id, Set_Button=1
  
  ; Display the image after thresholding.
  displayImage = self->ScaleImage()
  IF NOT self.no_window THEN BEGIN
    WSet, self.windex
    WShow, self.windex
    TVLCT, self.r, self.g, self.b
    TVImage, displayImage, /NoInterp
  ENDIF
  self->NotifyOthers
  
  ; Copy histogram from pixmap.
  WSet, self.histo_wid
  Device, Copy=[0, 0, self.pix_xsize, self.pix_ysize, 0, 0, self.pixmap]
  
  ; Draw threshold lines.
  self->DrawLines, self.minThresh, self.maxThresh
  
    
END ;--------------------------------------------------------------------------------


PRO AS_XSTRETCH::NEGATIVE, event

  ; Handle events from the "Positive Image/Negative Image" button.

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    void = Error_Message()
    RETURN
  ENDIF
  
    
  ; Switch the value of the button.
  Widget_Control, event.id, Get_Value=buttonValue
  CASE buttonValue OF
    'Negative Image': Widget_Control, event.id, Set_Value='Positive Image'
    'Positive Image': Widget_Control, event.id, Set_Value='Negative Image'
  ENDCASE
  self.negative = 1-self.negative
  
  ; Display the image after thresholding.
  displayImage = self->ScaleImage()
  IF NOT self.no_window THEN BEGIN
    WSet, self.windex
    WShow, self.windex
    TVLCT, self.r, self.g, self.b
    TVImage, displayImage, /NoInterp
  ENDIF
  self->NotifyOthers
  
    
END ;--------------------------------------------------------------------------------


PRO AS_XSTRETCH::OPENIMAGE, event

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN
  ENDIF
  
  Widget_Control, event.ID, Get_Value=buttonValue
  needcolors = 0
  
  CASE buttonValue OF
    'Raw Binary Image File...': BEGIN
    
      newImage = GetImage(Group_Leader=event.top, Cancel=cancelled, Catch=0)
      IF cancelled THEN RETURN
    END
    
    'Formatted Image File...': BEGIN
    
      newImage = SelectImage(Cancel=cancelled, Palette=palette, Group_Leader=event.top)
      IF cancelled THEN RETURN
    END
    
  ENDCASE
  
  self.newimage, newimage
  
END

PRO AS_XSTRETCH::NEWIMAGE, newimage, MIN = min, MAX = max

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN
  ENDIF
  
  dims = Image_Dimensions(newimage, XSize=xsize, YSize=ysize, TrueIndex=true)
  IF N_Elements(dims) LT 2 OR N_Elements(dims) GT 3 THEN Message, 'Must pass a 2D or 24-bit image'
  IF true NE -1 THEN BEGIN
    CASE true OF
      0: newimage = Transpose(newimage, [1, 2, 0])
      1: newimage = Transpose(newimage, [0, 2, 1])
      ELSE:
    ENDCASE
  ENDIF
  
  IF N_Elements(palette) NE 0 THEN BEGIN
    self.r = palette[*,0]
    self.g = palette[*,1]
    self.b = palette[*,2]
  ENDIF
  
  ; Restore the color table vectors.
  TVLCT, self.r, self.g, self.b
  
  *self.image = newImage
  
  ; Start with linear stretch on both ends.
  self.maxVal = Max(Double(newImage), Min=minVal)
  IF N_Elements(max) EQ 0 THEN self.maxThresh =  Float(self.maxVal) ELSE self.maxThresh = max
  self.minVal = minVal
  IF N_Elements(min) EQ 0 THEN self.minThresh = Float(self.minVal) ELSE self.minThresh = min
  
  ; Calculate a value to tell you if you are "close" to a threshold line.
  self.close = 0.05 * (self.maxval-self.minval)
  
  cWinID = !D.Window
  WSet, self.histo_wid
  self->Histoplot, WID=self.histo_wid, $
    MaxValue=self.maxValue, _Extra=*self.extra
    
  ; Store the plotting system variables for later recall.
  self.pbang = !P
  self.xbang = !X
  self.ybang = !Y
  self.ymin = !Y.CRange[0]
  self.ymax = !Y.CRange[1]
  self.xmin = !X.CRange[0]
  self.xmax = !X.CRange[1]
  
  ; Validitate the threshold values. Have to do this AFTER setting xmin/xmax.
  self.minThresh = self->VALIDATE_THRESHOLD(self.minThresh)
  self.maxThresh = self->VALIDATE_THRESHOLD(self.maxThresh)
  self.minThreshObj -> Set_Value, Number_Formatter(self.minThresh, Decimals=3), /FloatValue
  self.maxThreshObj -> Set_Value, Number_Formatter(self.maxThresh, Decimals=3), /FloatValue
  
  ; Put the same plot in the pixmap.
  WSet, self.pixmap
  Device, Copy=[0, 0, self.pix_xsize, self.pix_ysize, 0, 0, self.histo_wid]
  
  ; Update the image display by appling the threshold parameters.
  ; Be sure the image draw widget is still around. Make it if it isn't.
  displayImage = self->ScaleImage()
  
  IF NOT self.no_window THEN BEGIN
    IF Widget_Info(self.image_draw, /Valid_ID) THEN BEGIN
      WSet, self.windex
      TVLCT, self.r, self.g, self.b
      imageSize = Size(*self.image)
      xsize = imageSize(1)
      ysize = imageSize(2)
      aspect = Float(xsize)/ysize
      IF xsize GT 512 OR ysize GT 512 THEN BEGIN
        IF xsize NE ysize THEN BEGIN
          aspect = Float(ysize) / xsize
          IF aspect LT 1 THEN BEGIN
            xsize = 512
            ysize = (512 * aspect) < 512
          ENDIF ELSE BEGIN
            ysize = 512
            xsize = (512 / aspect) < 512
          ENDELSE
        ENDIF ELSE BEGIN
          ysize = 512
          xsize = 512
        ENDELSE
      ENDIF
      Widget_Control, self.image_draw, Draw_XSize=xsize, Draw_YSize=ysize
      
    ENDIF ELSE BEGIN
    
      imageSize = Size(*self.image)
      xsize = imageSize(1)
      ysize = imageSize(2)
      aspect = Float(xsize)/ysize
      IF xsize GT 512 OR ysize GT 512 THEN BEGIN
        IF xsize NE ysize THEN BEGIN
          aspect = Float(ysize) / xsize
          IF aspect LT 1 THEN BEGIN
            xsize = 512
            ysize = (512 * aspect) < 512
          ENDIF ELSE BEGIN
            ysize = 512
            xsize = (512 / aspect) < 512
          ENDELSE
        ENDIF ELSE BEGIN
          ysize = 512
          xsize = 512
        ENDELSE
      ENDIF
      Widget_Control, event.top, TLB_Get_Offset=offsets, TLB_Get_Size=sizes
      xoff = offsets[0] + sizes[0] + 20
      yoff = offsets[1]
      image_tlb = Widget_Base(Row=1, Group=event.top, Title='AS_XStretch Image', $
        XOffSet=xoff, YOffSet=yoff, TLB_Size_Events=1, XPad=0, YPad=0)
      image_draw = Widget_Draw(image_tlb, XSize=xsize, YSize=ysize)
      Widget_Control, image_tlb, /Realize, Set_UValue=event.top
      Widget_Control, image_draw, Get_Value=windex
      self.image_draw = image_draw
      self.windex = windex
      
      XManager, 'AS_XStretch_image', image_tlb, Event_Handler='AS_XStretch_Image_Resize', /No_Block
      Widget_Control, self.saveas, Sensitive=1
      Widget_Control, self.printit, Sensitive=1
      Widget_Control, self.colorsID, Sensitive=1
    ENDELSE
  ENDIF
  
  ; Draw threshold lines.
  self->DrawLines, self.minThresh, self.maxThresh
  
  ; Display the image after thresholding.
  displayImage = self->ScaleImage()
  IF NOT self.no_window THEN BEGIN
    WSet, self.windex
    WShow, self.windex
    TVLCT, self.r, self.g, self.b
    TVImage, displayImage, /NoInterp
  ENDIF
  self->NotifyOthers
  
  IF cWinID GT 0 THEN WSet, cWinID
    
END ;--------------------------------------------------------------------------------



PRO AS_XSTRETCH::SAVEAS, event

  ; Errors caused by incorrect IDL versions or missing Coyote files.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    IF N_Elements(thisDevice) NE 0 THEN Set_Plot, thisDevice
    RETURN
  ENDIF
  
  ; Save as various file types.
  
  Widget_Control, event.id, Get_UValue=saveAsType
  
  ; Set the current graphics window.
  
  cWinID = !D.Window
  WSet, self.windex
  TVLCT, self.r, self.g, self.b
  
  ; What kind of file do you want?
  
  filename = 'AS_XStretch'
  CASE saveAsType OF
  
    'JPEG': dummy = TVRead(Filename=filename, /JPEG)
    'PNG': dummy = TVRead(Filename=filename, /PNG)
    'TIFF': dummy = TVRead(Filename=filename, /TIFF)
    'GIF': dummy = TVRead(Filename=filename, /GIF)
    'PICT': dummy = TVRead(Filename=filename, /PICT)
    'BMP': dummy = TVRead(Filename=filename, /BMP)
    'PS': BEGIN
    
      WSet, self.windex
      keys = PSWindow()
      configureIt = PSConfig(Group_Leader=event.top, Cancel=cancelled, $
        Color=1, Filename='AS_XStretch.ps', _Extra=keys)
      IF NOT cancelled THEN BEGIN
        thisDevice = !D.Name
        Set_Plot, 'PS', /Copy
        Device, _Extra=configureIt
        displayImage = self->ScaleImage()
        TVImage, displayImage, /NoInterp
        Device, /Close_File
        Set_Plot, thisDevice
      ENDIF
      
    ENDCASE
  ENDCASE
  
  IF cWinID GT 0 THEN WSet, cWinID
  
END ;-------------------------------------------------------------------



PRO AS_XSTRETCH::SAVEHISTOAS, event

  ; Errors caused by incorrect IDL versions or missing Coyote files.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    IF N_Elements(thisDevice) NE 0 THEN Set_Plot, thisDevice
    RETURN
  ENDIF
  
  ; Save as various file types.
  
  Widget_Control, event.id, Get_UValue=saveAsType
  
  ; Set the current graphics window.
  
  cWinID = !D.Window
  WSet, self.histo_wid
  TVLCT, self.r, self.g, self.b
  
  ; What kind of file do you want?
  
  filename = 'AS_XStretch_histogram'
  CASE saveAsType OF
  
    'JPEG': dummy = TVRead(Filename=filename, /JPEG)
    'PNG': dummy = TVRead(Filename=filename, /PNG)
    'TIFF': dummy = TVRead(Filename=filename, /TIFF)
    'GIF': dummy = TVRead(Filename=filename, /GIF)
    'PICT': dummy = TVRead(Filename=filename, /PICT)
    'BMP': dummy = TVRead(Filename=filename, /BMP)
    'PS': BEGIN
    
      keys = PSWindow()
      configureIt = PSConfig(Group_Leader=event.top, Cancel=cancelled, $
        Color=1, Filename='AS_XStretch_histrogram.ps', _Extra=keys)
      IF NOT cancelled THEN BEGIN
        thisDevice = !D.Name
        thisFont=!P.Font
        !P.Font = 0
        Set_Plot, 'PS', /Copy
        Device, _Extra=configureIt
        self->Histoplot, MaxValue=self.maxValue, _Extra=*self.extra
        self->DrawLines, self.minThresh, self.maxThresh
        Device, /Close_File
        !P.Font = thisFont
        Set_Plot, thisDevice
      ENDIF
      
    ENDCASE
    
    
    
  ENDCASE
  
  IF cWinID GT 0 THEN WSet, cWinID
  
  
END ;-------------------------------------------------------------------

PRO AS_XSTRETCH::SETTHRESHOLDEXTERNAL, MIN = min, MAX = max

    IF N_Elements(min) NE 0 THEN self.minThreshObj -> Set_Value, String(min) 
    IF N_Elements(max) NE 0 THEN self.maxThreshObj -> Set_Value, String(max) 
    IF N_Elements(min) NE 0 OR N_Elements(max) NE 0 THEN self.SETTHRESHOLD, 1

END

PRO AS_XSTRETCH::SETTHRESHOLD, event

  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN
  ENDIF
  
  ; Get the min and max thresholds. Make sure they
  ; don't overlap each other.
  minThresh = self.minThreshObj -> Get_Value()
  self.minThresh = self->VALIDATE_THRESHOLD(minThresh < (self.maxThresh - (2000/200.)))
  maxThresh = self.maxThreshObj -> Get_Value()
  self.maxThresh = self->VALIDATE_THRESHOLD((self.minThresh + (2000/200.)) > maxThresh)
  self.minThreshObj -> Set_Value, Number_Formatter(self.minThresh, Decimals=3), /FloatValue
  self.maxThreshObj -> Set_Value, Number_Formatter(self.maxThresh, Decimals=3), /FloatValue
  
  ; Display the image after thresholding.
  displayImage = self->ScaleImage()
  IF NOT self.no_window THEN BEGIN
    WSet, self.windex
    WShow, self.windex
    TVLCT, self.r, self.g, self.b
    TVImage, displayImage, /NoInterp
  ENDIF
  self->NotifyOthers
  
  ; Copy histogram from pixmap.
  WSet, self.histo_wid
  Device, Copy=[0, 0, self.pix_xsize, self.pix_ysize, 0, 0, self.pixmap]
  
  ; Draw threshold lines.
  self->DrawLines, self.minThresh, self.maxThresh
  
END ;-------------------------------------------------------------------



PRO AS_XSTRETCH::PRINT, event

  ; Error Handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN
  ENDIF
  
  ; Printing and printer setup handled here.
    
  ; Configure printer and print if user OKs.
  result = Dialog_PrinterSetup()
  IF result EQ 1 THEN BEGIN
  
    ; Are you printing the image or the histogram?
    Widget_Control, event.id, Get_Value=TARGET
    
    ; I want the output on the page to have the same aspect ratio
    ; as I see in the display window.
    cWinID = !D.Window
    IF TARGET EQ 'IMAGE' THEN BEGIN
      WSet, self.windex
      TVLCT, self.r, self.g, self.b
    ENDIF ELSE BEGIN
      WSet, self.histo_wid
      
      ; Have to set up drawing colors *before* we go into the PRINTER device.
      FOR j=0,N_Elements(self.colors)-1 DO color = FSC_Color(self.colors[j])
    ENDELSE
    configurePrinter = PSWindow(/Printer)
    
    ; Print the image.
    thisDevice = !D.Name
    Set_Plot, 'PRINTER', /Copy
    Device, _Extra=configurePrinter
    Widget_Control, Hourglass=1
    IF TARGET EQ 'IMAGE' THEN BEGIN
      displayImage = self->ScaleImage()
      TVImage, displayImage, /NoInterp
    ENDIF ELSE BEGIN
      self->Histoplot, MaxValue=self.maxValue, _Extra=*self.extra
      self->DrawLines, self.minThresh, self.maxThresh
    ENDELSE
    Widget_Control, Hourglass=0
    Device, /Close_Document
    Set_Plot, thisDevice
    IF cWinID GT 0 THEN WSet, cWinID
  ENDIF
  
  
END ;-------------------------------------------------------------------



PRO AS_XSTRETCH::PROCESS_EVENTS, event

  ; This event handler ONLY responds to button down events from the
  ; draw widget. If it gets a DOWN event, it does two things: (1) finds
  ; out which threshold line is to be moved, and (2) changes the
  ; event handler for the draw widget to AS_XSTRETCH_MOVELINE.

  ; Error Handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN
  ENDIF
  
  possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
  thisEvent = possibleEventTypes(event.type)
  IF thisEvent NE 'DOWN' THEN RETURN
  
  ; Make sure you have the correct plotting environment.
  current_bangp = !P
  current_bangx = !X
  current_bangy = !Y
  
  !P = self.pbang
  !X = self.xbang
  !Y = self.ybang
  
  ; Convert the device coordinates to data coordinates.
  ; Have to have scaling factors for conversion.
  cWinID = !D.Window
  Wset, self.histo_wid
  TVLCT, self.r, self.g, self.b
  coords = Convert_Coord(event.x, event.y, 0, /Device, /To_Data)
  
  
  ; Click has to be inside the graph in the y direction.
  IF coords(1) LT self.ymin OR coords(1) GT self.ymax THEN BEGIN
    
    ; Put the info structure back into its storage location.; Set things back.
    !P = current_bangp
    !X = current_bangx
    !Y = current_bangy
    IF cWinID GT 0 THEN WSet, cWinID
    RETURN
  ENDIF
  

  
  ; How close to either line are you?
  closemin = Abs(self.minthresh - coords(0))
  closemax = Abs(self.maxthresh - coords(0))
  IF closemin LE closemax THEN self.lineby = 'MIN' ELSE self.lineby = 'MAX'
  
  ; If you are not close to a line, goodbye!
  CASE self.lineby OF
    'MIN': BEGIN
      IF closemin GT self.close THEN BEGIN
                
        ; Put the info structure back into its storage location.; Set things back.
        !P = current_bangp
        !X = current_bangx
        !Y = current_bangy
        IF cWinID GT 0 THEN WSet, cWinID
        RETURN
      ENDIF
    END
    
    'MAX': BEGIN
      IF closemax GT self.close THEN BEGIN
                
        ; Put the info structure back into its storage location.; Set things back.
        !P = current_bangp
        !X = current_bangx
        !Y = current_bangy
        IF cWinID GT 0 THEN WSet, cWinID
        RETURN
      ENDIF
    END
  ENDCASE
  
  ; Change the event handler for the draw widget and turn MOTION
  ; events ON.
  Widget_Control, event.id, Event_Pro='AS_XSTRETCH_MOVELINE', Draw_Motion_Events=1
  

      
  ; Put the info structure back into its storage location.; Set things back.
  !P = current_bangp
  !X = current_bangx
  !Y = current_bangy
  IF cWinID GT 0 THEN WSet, cWinID
     
    
END ; of AS_XSTRETCH_PROCESS_EVENTS *********************************************



PRO AS_XSTRETCH::MOVELINE, event

  ; This event handler continuously draws and erases a threshold line
  ; until it receives an UP event from the draw widget. Then it turns
  ; draw widget motion events OFF and changes the event handler for the
  ; draw widget back to AS_XSTRETCH_PROCESS_EVENTS.

  ; Error Handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN
  ENDIF
  
  ; Get the info structure out of the top-level base.
    
  ; Make sure you have the correct plotting environment.
  IF Size(*self.image, /Type) LE 3 THEN format = '(I0)' ELSE format='(F0)'
  current_bangp = !P
  current_bangx = !X
  current_bangy = !Y
  
  !P = self.pbang
  !X = self.xbang
  !Y = self.ybang
  
  cWinID = !D.Window
  
  ; Load image colors.
  TVLCT, self.r, self.g, self.b
  
  ; What type of an event is this?
  possibleEventTypes = [ 'DOWN', 'UP', 'MOTION', 'SCROLL' ]
  thisEvent = possibleEventTypes(event.type)
  
  IF thisEvent EQ 'UP' THEN BEGIN
  
    ; If this is an UP event, set the draw widget's event handler back
    ; to AS_XSTRETCH_PROCESS_EVENTS, turn MOTION events OFF, and apply the
    ; new threshold parameters to the image.
  
    ; Erase the last theshold line drawn.
    cWinID = !D.Window
    WSet, self.histo_wid
    TVLCT, self.r, self.g, self.b
    Device, Copy = [0, 0, self.pix_xsize, self.pix_ysize, 0, 0, self.pixmap]
    
    ; Turn motion events off and redirect the events to AS_XSTRETCH_PROCESS_EVENTS.
    Widget_Control, event.id, Draw_Motion_Events=0, $
      Event_Pro='AS_XStretch_Process_Events'
      
    ; Convert the event device coordinates to data coordinates.
    coord = Convert_Coord(event.x, event.y, /Device, /To_Data)
    
    ; Make sure the coordinate is between the other line and
    ; still inside the plot.
    range = self.xmax - self.xmin
    closest = range * 0.005
    CASE self.lineby OF
      'MIN': BEGIN
        coord(0) = coord(0) > self.xmin
        coord(0) = coord(0) < (self.maxThresh - closest)
      END
      'MAX': BEGIN
        coord(0) = coord(0) > (self.minThresh + closest)
        coord(0) = coord(0) < self.xmax
      END
    ENDCASE
    
    ; Draw both of the threshold lines again.
    CASE self.lineby OF
      'MIN': BEGIN
        self->DrawLines, coord[0], self.maxThresh
        self.minThresh = coord[0]
        self.minThreshObj -> Set_Value, $
          Number_Formatter(self->VALIDATE_THRESHOLD(self.minThresh), Decimals=3), /FloatValue
      END
      'MAX': BEGIN
        self->DrawLines, self.minThresh, coord[0]
        self.maxThresh = coord[0]
        self.maxThreshObj -> Set_Value, $
          Number_Formatter(self->VALIDATE_THRESHOLD(self.maxThresh), Decimals=3), /FloatValue
      END
    ENDCASE
    
    ; Update the image display by appling the threshold parameters.
    ; Be sure the image draw widget is still around. Make it if it isn't.
    displayImage = self->ScaleImage()
    self->NotifyOthers
    
    IF NOT self.no_window THEN BEGIN
      IF Widget_Info(self.image_draw, /Valid_ID) THEN BEGIN
        WSet, self.windex
        WShow, self.windex
        TVLCT, self.r, self.g, self.b
        TVImage, displayImage, /NoInterp
      ENDIF ELSE BEGIN
      
        imageSize = Size(*self.image)
        xsize = imageSize(1)
        ysize = imageSize(2)
        aspect = Float(xsize)/ysize
        IF xsize GT 512 OR ysize GT 512 THEN BEGIN
          IF xsize NE ysize THEN BEGIN
            aspect = Float(ysize) / xsize
            IF aspect LT 1 THEN BEGIN
              xsize = 512
              ysize = (512 * aspect) < 512
            ENDIF ELSE BEGIN
              ysize = 512
              xsize = (512 / aspect) < 512
            ENDELSE
          ENDIF
        ENDIF
        Widget_Control, event.top, TLB_Get_Offset=offsets, TLB_Get_Size=sizes
        xoff = offsets[0] + sizes[0] + 20
        yoff = offsets[1]
        image_tlb = Widget_Base(Row=1, Group=event.top, Title='AS_XStretch Image', $
          XOffSet=xoff, YOffSet=yoff, TLB_Size_Events=1, XPad=0, YPad=0)
        image_draw = Widget_Draw(image_tlb, XSize=xsize, YSize=ysize)
        Widget_Control, image_tlb, /Realize, Set_UValue=event.top
        Widget_Control, image_draw, Get_Value=windex
        self.image_draw = image_draw
        self.windex = windex
        TVImage, displayImage, /NoInterp
        
        XManager, 'AS_XStretch_image', image_tlb, Event_Handler='AS_XStretch_Image_Resize', /No_Block
        Widget_Control, self.saveas, Sensitive=1
        Widget_Control, self.printit, Sensitive=1
        Widget_Control, self.colorsID, Sensitive=1
      ENDELSE
    ENDIF
    
    ; Update the pixmap with histogram with no threshold lines.
    self->Histoplot, WID=self.pixmap, $
      MaxValue=self.maxValue, _Extra=*self.extra
      
    ; Put the info structure back into its storage location and then,
    ; out of here!
    IF cWinID GT 0 THEN WSet, cWinID
    RETURN
  ENDIF ; thisEvent = UP
  
  
  ; Most of the action in this event handler occurs here while we are waiting
  ; for an UP event to occur. As long as we don't get it, keep erasing the
  ; old threshold line and drawing a new one.
  
  ; Get current window and scaling parameters in order.
  WSet, self.histo_wid
  TVLCT, self.r, self.g, self.b
  !P = self.pbang
  !X = self.xbang
  !Y = self.ybang
  coord = Convert_Coord(event.x, event.y, /Device, /To_Data)
  
  ; Draw the "other" line on the pixmap (so you don't have to draw
  ; it all the time).
  WSet, self.pixmap
  CASE self.lineby OF
    'MIN': BEGIN
      cmax = Convert_Coord(self.maxThresh, 0, /Data, /To_Normal)
      PlotS, [self.maxthresh, self.maxthresh],[self.ymin, self.ymax],  $
        Color=FSC_Color(self.colors[3]), Thick=2
      XYOuts, cmax[0], 0.90, /Normal, Number_Formatter(self->Validate_Threshold(self.maxThresh), Decimals=3), $
        Color=FSC_Color(self.colors[3]), Alignment=0.0, Font=0
    END
    'MAX': BEGIN
      cmin = Convert_Coord(self.minThresh, 0, /Data, /To_Normal)
      PlotS, [self.minthresh, self.minthresh],[self.ymin, self.ymax],  $
        Color=FSC_Color(self.colors[2]), Thick=2
      XYOuts, cmin[0], 0.90, /Normal, Number_Formatter(self->Validate_Threshold(self.minThresh), Decimals=3), $
        Color=FSC_Color(self.colors[2]), Alignment=1.0, Font=0
    END
  ENDCASE
  
  ; Erase the old threshold line.
  WSet, self.histo_wid
  TVLCT, self.r, self.g, self.b
  Device, Copy = [0, 0, self.pix_xsize, self.pix_ysize, 0, 0, self.pixmap]
  
  ; Draw the new line at the new coordinate. Make sure the coordinate
  ; is inside the plot and doesn't go over the other line.
  CASE self.lineby OF
    'MIN': BEGIN
      coord[0] = coord[0] > (self.xmin)
      coord[0] = coord[0] < (self.maxThresh)
      self.minThreshObj -> Set_Value, Number_Formatter(self->VALIDATE_THRESHOLD(coord[0]), Decimals=3), /FloatValue
    END
    'MAX': BEGIN
      coord[0] = coord[0] > (self.minThresh)
      coord[0] = coord[0] < (self.xmax )
      self.maxThreshObj -> Set_Value, Number_Formatter(self->VALIDATE_THRESHOLD(coord[0]), Decimals=3), /FloatValue
    END
  ENDCASE
  
  cmax = Convert_Coord(self.maxThresh, 0, /Data, /To_Normal)
  cmin = Convert_Coord(self.minThresh, 0, /Data, /To_Normal)
  
  theCoord = self->Validate_Threshold(coord[0])
  CASE self.lineby OF
    'MIN': BEGIN
      PlotS, [coord(0), coord(0)],[self.ymin, self.ymax], Color=FSC_Color(self.colors[2]), Thick=2
      XYOuts, Float(event.x)/!D.X_Size, 0.90, /Normal, Number_Formatter(thecoord, Decimals=3), $
        Color=FSC_Color(self.colors[2]), Alignment=1.0, Font=0
    END
    'MAX': BEGIN
      PlotS, [coord(0), coord(0)],[self.ymin, self.ymax], Color=FSC_Color(self.colors[3]), Thick=2
      XYOuts, Float(event.x)/!D.X_Size, 0.90, /Normal,  Number_Formatter(thecoord, Decimals=3), $
        Color=FSC_Color(self.colors[3]), Alignment=0.0, Font=0
    END
  ENDCASE
  
  ; Set things back.
  !P = current_bangp
  !X = current_bangx
  !Y = current_bangy
  
  IF cWinID GT 0 THEN WSet, cWinID
  
END ; of AS_XSTRETCH_MOVELINE **************************************************



PRO AS_XSTRETCH::STRETCHTYPE, event

  ; Handles events from the Stretch Type buttons.

  ; Error Handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN
  ENDIF
  
  ; What is the new type?
  selection = event.self -> GetSelection()
  
  ; Always start min and max thresholds fresh when moving from one stretch to another.
  IF self.type NE selection THEN BEGIN
    CASE StrUpCase(selection) OF
      'SQUARE ROOT': self.minThresh = MIN(SQRT(*self.image), MAX=maxthresh)
      'EQUALIZATION': self.minThresh = MIN(HIST_EQUAL(*self.image), MAX=maxthresh)
      'LINEAR 2%': BEGIN
        ; Calculate binsize.
        maxr = Max(Double(*self.image), MIN=minr, /NAN)
        range = maxr - minr
        IF Size(*self.image, /TName) EQ 'BYTE' THEN binsize = 1.0 ELSE binsize = range / 300.
        h = Histogram(*self.image, BINSIZE=binsize, OMIN=omin, OMAX=omax)
        n = N_Elements(*self.image)
        cumTotal = Total(h, /CUMULATIVE)
        minIndex = Value_Locate(cumTotal, n * 0.02)
        IF minIndex EQ -1 THEN minIndex = 0
        WHILE cumTotal[minIndex] EQ cumTotal[minIndex + 1] DO BEGIN
          minIndex = minIndex + 1
        ENDWHILE
        self.minThresh = minIndex * binsize + omin
        
        maxIndex  = Value_Locate(cumTotal, n * 0.98)
        WHILE cumTotal[maxIndex] EQ cumTotal[maxIndex - 1] DO BEGIN
          maxIndex = maxIndex - 1
        ENDWHILE
        maxThresh = maxIndex * binsize + omin
      END
      ELSE: self.minThresh = MIN(*self.image, MAX=maxthresh)
    ENDCASE
    self.maxThresh = maxthresh
  ENDIF
  
  ; Store the selection type.
  self.type = StrUpCase(selection)
  
  CASE self.type OF
  
    'LINEAR': BEGIN
      IF Widget_Info(self.currentMappedBase, /Valid_ID) THEN $
        Widget_Control, self.currentMappedBase, Map=0
    END
    
    'LINEAR 2%': BEGIN
      IF Widget_Info(self.currentMappedBase, /Valid_ID) THEN $
        Widget_Control, self.currentMappedBase, Map=0
    END
    
    '^0.2' : BEGIN
      IF Widget_Info(self.currentMappedBase, /Valid_ID) THEN $
        Widget_Control, self.currentMappedBase, Map=0
    END
    
    'EQUALIZATION': BEGIN
      IF Widget_Info(self.currentMappedBase, /Valid_ID) THEN $
        Widget_Control, self.currentMappedBase, Map=0
    END
    
    'SQUARE ROOT': BEGIN
      IF Widget_Info(self.currentMappedBase, /Valid_ID) THEN $
        Widget_Control, self.currentMappedBase, Map=0
    END
    
    'GAMMA': BEGIN
      IF Widget_Info(self.currentMappedBase, /Valid_ID) THEN $
        Widget_Control, self.currentMappedBase, Map=0
      Widget_Control, self.gammaBaseID, Map=1
      self.currentMappedBase = self.gammaBaseID
    END
    
    'GAUSSIAN': BEGIN
      IF Widget_Info(self.currentMappedBase, /Valid_ID) THEN $
        Widget_Control, self.currentMappedBase, Map=0
      Widget_Control, self.gaussBaseID, Map=1
      self.currentMappedBase = self.gaussBaseID
    END
    
    'LOG': BEGIN
      IF Widget_Info(self.currentMappedBase, /Valid_ID) THEN $
        Widget_Control, self.currentMappedBase, Map=0
      Widget_Control, self.logBaseID, Map=1
      self.currentMappedBase = self.logBaseID
    END
    
    'ASINH': BEGIN
      IF Widget_Info(self.currentMappedBase, /Valid_ID) THEN $
        Widget_Control, self.currentMappedBase, Map=0
      Widget_Control, self.asinhBaseID, Map=1
      self.currentMappedBase = self.asinhBaseID
    END
    
    
  ENDCASE
  
  
  ; Draw histogram.
  self->Histoplot, WID=self.histo_wid, $
    MaxValue=self.maxValue, _Extra=*self.extra
    
  ; Draw histogram in pixmap
  self->Histoplot, WID=self.pixmap, $
    MaxValue=self.maxValue, _Extra=*self.extra
    
  ; Draw threshold lines.
  self->DrawLines, self.minThresh, self.maxThresh
  
  ; Display the image after thresholding.
  displayImage = self->ScaleImage()
  IF NOT Keyword_Set(self.no_window) THEN BEGIN
    WSet, self.windex
    TVLCT, self.r, self.g, self.b
    WShow, self.windex
    TVImage, displayImage, /NoInterp, _Extra=*self.extra
  ENDIF
  
  ; Notify others of image change.
  self->NotifyOthers
  
     
END ;-------------------------------------------------------------------


PRO AS_XSTRETCH::QUIT, event
  Widget_Control, event.top, /Destroy
END ; of AS_XSTRETCH_QUIT ******************************************************



PRO AS_XSTRETCH::COLORS, event

  ; Error Handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN
  ENDIF
  
  cWinID = !D.Window
  
  thisEvent = Tag_Names(event, /Structure_Name)
  CASE thisEvent OF
  
    'WIDGET_BUTTON': BEGIN
      TVLCT, self.r, self.g, self.b
      XColors, Group=event.top, NotifyID=[event.id, event.top]
    END
    'XCOLORS_LOAD': BEGIN
      Device, Get_Visual_Depth=thisDepth
      IF thisDepth GT 8 THEN BEGIN
        displayImage = self->ScaleImage()
        IF self.no_window EQ 0 THEN BEGIN
          self.r = event.r
          self.g = event.g
          self.b = event.b
          TVLCT, self.r, self.g, self.b
          WShow, self.windex
          WSet, self.windex
          TVImage, displayImage, /NoInterp
        ENDIF
        self.r = event.r
        self.g = event.g
        self.b = event.b
        self->NotifyOthers
        
      ENDIF
    END
  ENDCASE
  IF cWinID GT 0 THEN WSet, cWinID
  
    
END ; of AS_XSTRETCH_COLORS ****************************************************

PRO AS_XSTRETCH::ZOOMEXTERNAL, OUT = out

  IF KeyWord_Set(out) THEN id = Widget_Info(self.Histo_tlb, FIND_BY_UNAME = 'ZOOM OUT') $
                      ELSE id = Widget_Info(self.Histo_tlb, FIND_BY_UNAME = 'ZOOM IN')

  event = {id:id}
  self.zoom, event

END

PRO AS_XSTRETCH::ZOOM, event

  ; Error Handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN
  ENDIF


  Widget_Name = WIDGET_INFO(event.id, /UNAME)

  CASE Widget_Name OF 
    'ZOOM IN' : self.xZoom = [self.minThresh, self.maxThresh]
    'ZOOM OUT': self.xZoom = [self.minVal, self.maxVal]
                
  ENDCASE
  
  self->NotifyOthers
  
  cWinID = !D.Window
  
  ; Update the histogram plot.
  self->Histoplot, WID=self.histo_wid, $
    MaxValue=self.maxValue, _Extra=*self.extra
    
  ; Draw threshold lines on the histogram plot.
  self->DrawLines, self.minthresh, self.maxthresh
  
  ; Update the pixmap with histogram with no threshold lines.
  self->Histoplot, WID=self.pixmap, $
    MaxValue=self.maxValue, _Extra=*self.extra
    
  IF cWinID GT 0 THEN WSet, cWinID

END


PRO AS_XSTRETCH::MAXVALUE, event

  ; Error Handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN
  ENDIF
  
  cWinID = !D.Window
  
  ; Get the new max value.
  Widget_Control, event.id, Get_UValue=maxValue
  self.maxValue = maxValue
  
  ; Update the histogram plot.
  self->Histoplot, WID=self.histo_wid, $
    MaxValue=self.maxValue, _Extra=*self.extra
    
  ; Draw threshold lines on the histogram plot.
  self->DrawLines, self.minthresh, self.maxthresh
  
  ; Update the pixmap with histogram with no threshold lines.
  self->Histoplot, WID=self.pixmap, $
    MaxValue=self.maxValue, _Extra=*self.extra
    
  IF cWinID GT 0 THEN WSet, cWinID
  
    
END ;-------------------------------------------------------------------------



PRO AS_XSTRETCH::IMAGE_RESIZE, event

  ; Error Handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN
  ENDIF
  
  cWinID = !D.Window
      
  ; I would like to maintain the window aspect ratio the same as the
  ; image aspect ratio.
  dims = Size(*self.image, /Dimensions)
  xsize = dims[0]
  ysize = dims[1]
  maxWindowSize = Max([event.x, event.y])
  IF xsize NE ysize THEN BEGIN
    aspect = Float(ysize) / xsize
    IF aspect LT 1 THEN BEGIN
      xsize = maxWindowSize
      ysize = (maxWindowSize * aspect) < maxWindowSize
    ENDIF ELSE BEGIN
      ysize = maxWindowSize
      xsize = (maxWindowSize / aspect) < maxWindowSize
    ENDELSE
  ENDIF ELSE BEGIN
    ysize = maxWindowSize
    xsize = maxWindowSize
  ENDELSE
  
  
  Widget_Control, self.image_draw, Draw_XSize=xsize, Draw_YSize=ysize
  WSet, self.windex
  displayImage = self->ScaleImage()
  TVLCT, self.r, self.g, self.b
  TVImage, displayImage, /NoInterp
  self->NotifyOthers
  
  IF cWinID GT 0 THEN WSet, cWinID
  
END ; of AS_XSTRETCH_IMAGE_RESIZE **********************************************


PRO AS_XSTRETCH::HISTOGRAM_RESIZE, event

  ; Error Handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    void = Error_Message()
    RETURN
  ENDIF
  
  cWinID = !D.Window
  
  Widget_Control, self.histo_draw, Draw_XSize=event.x > self.min_xsize, Draw_YSize=(event.y - self.pbase_ysize) > 150
  
  ; Draw the plot.
  self->Histoplot, WID=self.histo_wid, $
    MaxValue=self.maxValue, _Extra=*self.extra
    
  ; Put the same plot in the pixmap.
  WDelete, self.pixmap
  Window, /Free, XSize=event.x > self.min_xsize, YSize=(event.y - self.pbase_ysize) > 150, /Pixmap
  self.pixmap = !D.Window
  self.pix_xsize = event.x > self.min_xsize
  self.pix_ysize = (event.y - self.pbase_ysize) > 150
  Device, Copy=[0, 0, self.pix_xsize, self.pix_ysize, 0, 0, self.histo_wid]
  
  ; Save the scaling factors for calculating data coordinates.
  self.pbang = !P
  self.xbang = !X
  self.ybang = !Y
  self.ymin = !Y.CRange[0]
  self.ymax = !Y.CRange[1]
  self.xmin = !X.CRange[0]
  self.xmax = !X.CRange[1]
  
  ; Draw threshold lines on the histogram plot.
  self->DrawLines, self.minThresh, self.maxThresh
  
  IF cWinID GT 0 THEN WSet, cWinID
  
   
END ; of AS_XSTRETCH_COLORS ****************************************************

PRO AS_XSTRETCH::HistHide, hide

  Widget_Control, self.Histo_tlb, MAP = ~hide
  self.NOTIFYOTHERS

END


PRO AS_XSTRETCH::CLEANUP, tlb

  IF Widget_Info(self.Histo_tlb, /VALID) THEN Widget_Control, self.Histo_tlb, /DESTROY
  ;self->NOTIFYOTHERS
  IF self.newPointer THEN IF Ptr_Valid(self.image) THEN Ptr_Free, self.image
  IF Ptr_Valid(self.extra) THEN Ptr_Free, self.extra
  WDelete, self.pixmap
  Obj_Destroy, self
    
END ;---------------------------------------------------------------------

FUNCTION AS_XSTRETCH::Init, theImage, $
    Beta=beta, $
    Block=block, $
    Colors=colors, $
    Colortable=ctable, $
    Exponent=exponent, $
    Filename=filename, $
    Gamma=gamma, $
    Group_Leader=group, $
    Max_Value=maxvalue, $
    MaxThresh = maxThresh, $
    Mean=mean, $
    MinThresh = minThresh, $
    Negative=negative, $
    No_Window=no_window, $
    Notify_Obj=notify_obj, $
    Notify_Pro=notify_pro, $
    Title=title, $
    Type=type, $
    XPos=xpos, $
    XSize = xsize, $
    YPos=ypos, $
    YSize = ysize, $
    updatecbdata=updatecbdata, $
    HistWin = Histo_tlb, $
    palette = palette, $
    xZoom = xzoom, $
    NO_MAP_GUI = no_map_gui, $
    _EXTRA=extra
    
  ; Error Handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = Error_Message(/Traceback)
    RETURN, -1
  ENDIF
  
  ; Set up environment.
  Compile_Opt idl2
  
  ; Initial values of some variables.
  cWinID = !D.Window
  
  IF N_Elements(xsize) EQ 0 THEN xsize = 400
  IF N_Elements(ysize) EQ 0 THEN ysize = 220
  
  histXsize = xsize
  histYsize = ysize
  
  no_map_gui = Keyword_Set(no_map_gui)
  
  ; Did you specify a filename?
  IF N_Elements(filename) NE 0 AND N_Elements(theImage) EQ 0 THEN BEGIN
    IF filename NE "" THEN BEGIN
      theImage = SelectImage(Filename=filename, Cancel=cancelled, Palette=palette, /Silent)
      IF cancelled THEN RETURN, -1
    ENDIF
  ENDIF
  
  ; Need an image?
  IF N_Elements(theImage) EQ 0  THEN BEGIN
    file = Filepath(SubDir=['examples', 'data'], 'ctscan.dat')
    theImage = BytArr(256, 256)
    OpenR, lun, file, /GET_LUN
    ReadU, lun, theImage
    Free_LUN, lun
  ENDIF
  
  ; Is image a pointer? If not, make it one.
  IF Size(theImage, /TName) NE 'POINTER' THEN BEGIN
    image = Ptr_New(theImage)
    newPointer = 1
  ENDIF ELSE BEGIN
    image = theImage
    newPointer = 0
  ENDELSE
  
  ; Check for underflow of values near 0. Yuck! Necessary with gnarly data.
  curExcept = !Except
  !Except = 0
  i = Where(*image GT -1e-35 AND *image LT 1e-35, count)
  IF count GT 0 THEN (*image)[i] = 0.0
  void = Check_Math()
  !Except = curExcept
  
  dims = Image_Dimensions(*image, XSize=xsize, YSize=ysize, TrueIndex=true)
  IF N_Elements(dims) LT 2 OR N_Elements(dims) GT 3 THEN Message, 'Must pass a 2D or 24-bit image'
  IF true NE -1 THEN BEGIN
    CASE true OF
      0: *image = Transpose(*image, [1, 2, 0])
      1: *image = Transpose(*image, [0, 2, 1])
      ELSE:
    ENDCASE
  ENDIF
  
  ; Default values for keywords.
  IF N_Elements(beta) EQ 0 THEN beta = 3 ELSE beta = beta > 0.0
  IF N_Elements(colors) EQ 0 THEN BEGIN
    colors = ['ivory', 'charcoal', 'firebrick', 'steel blue', 'sea green', 'charcoal']
  ENDIF ELSE BEGIN
    IF N_Elements(colors) NE 6 THEN Message, 'Incorrect number of colors in COLORS vector.'
    defcolors = ['ivory', 'charcoal', 'firebrick', 'steel blue', 'sea green', 'charcoal']
    i = Where(colors EQ "", count)
    IF count GT 0 THEN colors[i] = defcolors[i]
  ENDELSE
  IF N_Elements(ctable) EQ 0 THEN ctable = 0
  IF N_Elements(exponent) EQ 0 THEN exponent = 4.0
  IF N_Elements(extra) EQ 0 THEN extra = Ptr_New(/Allocate_Heap) ELSE extra = Ptr_New(extra)
  IF N_Elements(gamma) EQ 0 THEN gamma = 1.5
  IF N_Elements(maxvalue) EQ 0 THEN maxvalue = 0.75
  IF N_Elements(mean) EQ 0 THEN mean = 0.5
  mean = 0.0 > mean < 1.0
  
  IF N_Elements(notify_pro) EQ 0 THEN notify_pro = ""
  IF N_Elements(updatecbdata) EQ 0 THEN updatecbdata = ""
  IF N_Elements(negative) EQ 0 THEN negative = 0
  IF N_Elements(notify_obj) EQ 0 THEN notify_obj = {object:Obj_New(), method:""} ELSE BEGIN
    IF Size(notify_obj, /TNAME) NE 'STRUCT' THEN $
      Message, 'NOTIFY_OBJ keyword requires structure variable'
    names = Tag_Names(notify_obj)
    index = Where(names EQ "METHOD", count)
    IF count EQ 0 THEN Message, 'NOTIFY_OBJ structure requires METHOD field.'
    index = Where(names EQ "OBJECT", count)
    IF count EQ 0 THEN Message, 'NOTIFY_OBJ structure requires OBJECT field.'
    validObj = Obj_Valid(notify_obj.object) 
    IF Where(validObj EQ 0) NE -1 THEN Message, 'One of NOTIFY_OBJ object is invalid.'
  ENDELSE
  no_window = Keyword_Set(no_window)
  IF N_Elements(sigma) EQ 0 THEN sigma = 1.0 ELSE sigma = sigma > 0.1
  IF N_Elements(xpos) EQ 0 THEN xpos = 100
  IF N_Elements(ypos) EQ 0 THEN ypos = 100
  IF N_Elements(title) EQ 0 THEN title = 'Drag Vertical Lines to STRETCH Image Contrast'
  
  ; Determine scaling type.
  possibleTypes = ['LINEAR', 'GAMMA', 'LOG', 'ASINH', 'LINEAR 2%', '^0.2', 'SQUARE ROOT', 'EQUALIZATION', 'GAUSSIAN']
  IF N_Elements(type) EQ 0 THEN type = 'LINEAR'
  IF Size(type, /TName) EQ 'STRING' THEN BEGIN
    type = StrUpCase(type)
    index = WHERE(possibleTypes EQ type, count)
    IF count EQ 0 THEN Message, 'Cannot find specified stretch type: ' + type
  ENDIF ELSE BEGIN
    type = 0 > Fix(type) < 7
    type = possibleTypes[type]
  ENDELSE
  
  ; Check for availability of GIF files.
  thisVersion = Float(!Version.Release)
  IF ((thisVersion LT 5.4) OR (thisVersion GE 6.2)) THEN haveGif = 1 ELSE haveGIF = 0
  
  ; Create the histogram widget.
  histo_tlb = Widget_Base(Column=1, Title=title, XPad=0, YPad=0, $
    MBar=menubaseID, TLB_Size_Events=1, XOffset=xpos, YOffset=ypos, Base_Align_Center=1, /TLB_KILL_REQUEST_EVENTS)
  
  ;testWindowWidget = Widget_Window(histo_tlb)
        
  histo_draw = Widget_Draw(histo_tlb, XSize=histXsize, YSize=histYsize, $
    Button_Events=1, Event_Pro='AS_XStretch_Process_Events')
  controlID = Widget_Button(menubaseID, Value='Controls', Event_Pro='AS_XStretch_MaxValue')
  ;openit = Widget_Button(controlID, Value='Open', /MENU)
  ;dummy = Widget_Button(openit, Value='Formatted Image File...', Event_Pro='AS_XStretch_OpenImage')
  ;dummy = Widget_Button(openit, Value='Raw Binary Image File...', Event_Pro='AS_XStretch_OpenImage')
  ;mainsaveID = Widget_Button(controlID, Value='Save as Main IDL Variable', /Menu, Event_Pro='AS_XSTRETCH_SAVETOMAIN')
  ;dummy = Widget_Button(mainsaveID, Value='Stretched Image', UValue='IMAGE')
  ;dummy = Widget_Button(mainsaveID, Value='Stretched Histogram', UValue='HISTOGRAM')
  ;dummy = Widget_Button(mainsaveID, Value='Current Stretch Parameters (structure)', UValue='PARAMETERS')
  ;dummy = Widget_Button(mainsaveID, Value='All Stretch Information (structure)', UValue='EVERYTHING')
  saveAs = Widget_Button(controlID, Value='Save Image As', Event_Pro="AS_XStretch_SaveAs", /Menu)
  dummy = Widget_Button(saveAs, Value='BMP File', UValue='BMP')
  dummy = Widget_Button(saveAs, Value='JPEG File', UValue='JPEG')
  dummy = Widget_Button(saveAs, Value='PNG File', UValue='PNG')
  dummy = Widget_Button(saveAs, Value='PICT File', UValue='PICT')
  dummy = Widget_Button(saveAs, Value='TIFF File', UValue='TIFF')
  IF havegif THEN dummy = Widget_Button(saveAs, Value='GIF File', UValue='GIF')
  dummy = Widget_Button(saveAs, Value='PostScript File', UValue='PS')
  saveHistoAs = Widget_Button(controlID, Value='Save Histogram As', Event_Pro="AS_XStretch_SaveHistoAs", /Menu)
  dummy = Widget_Button(saveHistoAs, Value='BMP File', UValue='BMP')
  dummy = Widget_Button(saveHistoAs, Value='JPEG File', UValue='JPEG')
  dummy = Widget_Button(saveHistoAs, Value='PNG File', UValue='PNG')
  dummy = Widget_Button(saveHistoAs, Value='PICT File', UValue='PICT')
  dummy = Widget_Button(saveHistoAs, Value='TIFF File', UValue='TIFF')
  IF havegif THEN dummy = Widget_Button(saveHistoAs, Value='GIF File', UValue='GIF')
  dummy = Widget_Button(saveHistoAs, Value='PostScript File', UValue='PS')
  printit = Widget_Button(controlID, Value='Print...', Event_Pro='AS_XStretch_Print', /MENU)
  dummy = Widget_Button(printit, Value='Image', UValue='IMAGE')
  dummy = Widget_Button(printit, Value='Histogram', UValue='HISTOGRAM')
  
  maxID = Widget_Button(controlID, Value='Max Pixel Density', /Menu, /Separator)
  dummy = Widget_Button(maxID, Value='0.005', UValue=0.005)
  dummy = Widget_Button(maxID, Value='0.010', UValue=0.010)
  dummy = Widget_Button(maxID, Value='0.025', UValue=0.025)
  dummy = Widget_Button(maxID, Value='0.050', UValue=0.050)
  dummy = Widget_Button(maxID, Value='0.075', UValue=0.075)
  dummy = Widget_Button(maxID, Value='0.100', UValue=0.1)
  dummy = Widget_Button(maxID, Value='0.200', UValue=0.2)
  dummy = Widget_Button(maxID, Value='0.300', UValue=0.3)
  dummy = Widget_Button(maxID, Value='0.400', UValue=0.4)
  dummy = Widget_Button(maxID, Value='0.500', UValue=0.5)
  dummy = Widget_Button(maxID, Value='0.600', UValue=0.6)
  dummy = Widget_Button(maxID, Value='0.700', UValue=0.7)
  dummy = Widget_Button(maxID, Value='0.800', UValue=0.8)
  dummy = Widget_Button(maxID, Value='0.900', UValue=0.9)
  dummy = Widget_Button(maxID, Value='1.000', UValue=1.0)
  colorsID = Widget_Button(controlID, Value='Image Colors...', $
    Event_Pro='AS_XStretch_Colors', /Separator)
  button = Widget_Button(controlID, Value='Negative Image', Dynamic_Resize=1, Event_Pro='AS_XStretch_Negative')
  ;button = Widget_Button(controlID, Value='Flip Image', Event_Pro='AS_XStretch_FlipImage')
  quitter = Widget_Button(controlID, Value='Quit', $
    Event_Pro='AS_XStretch_Quit', /Separator)
    
 
    
    
  ; Stretch TYPE buttons.
  paramBaseID = Widget_Base(histo_tlb, XPAD=0, YPAD=0, Column=1, Base_Align_Left=1)
  rowID = Widget_Base(paramBaseID, XPAD=0, YPAD=0, ROW=1, SPACE=10)
  row2ID = Widget_Base(paramBaseID, XPAD=0, YPAD=0, ROW=1, SPACE=10)
   
  
  types = StrUpCase(['Linear', 'Linear 2%', 'Gamma', 'Log', '^0.2', 'Square Root', 'Asinh', 'Equalization', 'Gaussian'])
  index = Where(types EQ type) ; Necessary for backward compatibility and for my ordering in pull-down.
  scaleID = FSC_Droplist(rowID, Title='Scaling: ', Spaces=1, $
    Value=['Linear', 'Linear 2%', 'Gamma', 'Log', 'Asinh', '^0.2', 'Square Root', 'Equalization', 'Gaussian'], $
    Event_Pro='AS_XStretch_StretchType')
  scaleID -> SetIndex, index[0] > 0
  
  minthreshObj = FSC_InputField(rowID, Title='Min: ', Value=0.0, $
    /FloatValue, Event_Pro='AS_XStretch_SetThreshold', UValue='MINTHRESH', $
    XSize=10, /CR_Only)
    
  maxthreshObj = FSC_InputField(rowID, Title='Max: ', Value=0.0, $
    /FloatValue, Event_Pro='AS_XStretch_SetThreshold', UValue='MAXTHRESH', $
    XSize=10, /CR_Only)
    
  ; Create the control base widgets.
  controlBaseID = Widget_Base(paramBaseID, XPAD=0, YPAD=0)
  
  ; LOG controls.
  logBaseID = Widget_Base(controlBaseID, XPAD=0, YPAD=0, ROW=1, SPACE=10, Map=0)
  param1Obj = FSC_InputField(logBaseID, Title='Mean: ', Value=mean, /Positive, $
    /FoatValue, Event_Pro='AS_XStretch_Parameters', /CR_Only, XSize=10)
  param2Obj = FSC_InputField(logBaseID, Title='Exponent: ', Value=exponent, /Positive, $
    /FloatValue, Event_Pro='AS_XStretch_Parameters', /CR_Only, XSize=10)
    
  ; GAMMA controls.
  gammaBaseID = Widget_Base(controlBaseID, XPAD=0, YPAD=0, ROW=1, SPACE=10, Map=0)
  label = Widget_Label(gammaBaseID, Value='Gamma: ', /Dynamic_Resize)
  gammas = ['-0.25', '0.040', '0.100', '0.200', '0.400', '0.667', '1.00', '1.500', '2.500', '5.000', '10.00', '25.00']
  gamma_comboID = Widget_Combobox(gammaBaseID, /Editable, Value=gammas, UVALUE='GAMMA', $
    Event_Pro='AS_XStretch_Parameters')
  index = Where(gammas EQ gamma, count)
  IF count EQ 0 THEN BEGIN
    Widget_Control, gamma_comboID, Combobox_AddItem=StrTrim(gamma,2), ComboBox_Index=0
  ENDIF ELSE Widget_Control, gamma_comboID, Set_Combobox_Select=index[0]
  
  ; ASINH controls.
  asinhBaseID = Widget_Base(controlBaseID, XPAD=0, YPAD=0, ROW=1, SPACE=10, Map=0)
  label = Widget_Label(asinhBaseID, Value='Beta: ', /Dynamic_Resize)
  betas = ['0.0', '0.1', '0.5', '1.0', '3.0', '5.0', '10.0', '50.0', '100.0']
  asinh_comboID = Widget_Combobox(asinhBaseID, /Editable, Value=betas, $
    UVALUE='ASINH', Event_Pro='AS_XStretch_Parameters')
  index = Where(betas EQ beta, count)
  IF count EQ 0 THEN BEGIN
    Widget_Control, asinh_comboID, Combobox_AddItem=Number_Formatter(beta,Decimals=2), ComboBox_Index=0
  ENDIF ELSE Widget_Control, asinh_comboID, Set_Combobox_Select=index[0]
  
  ; GAUSSIAN controls.
  gaussBaseID = Widget_Base(controlBaseID, XPAD=0, YPAD=0, ROW=1, SPACE=10, Map=0)
  sigmaObj = FSC_InputField(gaussBaseID, Title='Sigma: ', Value=sigma, /Positive, $
    /FoatValue, Event_Pro='AS_XStretch_Parameters', /CR_Only, XSize=10)
    
    
  ; Zoom Controls
  button = Widget_Button(row2ID, VALUE = 'Zoom to thresholds', Event_Pro='AS_XStretch_Zoom', UNAME = 'ZOOM IN')
  button = Widget_Button(row2ID, VALUE = 'Zoom out', Event_Pro='AS_XStretch_Zoom', UNAME = 'ZOOM OUT') 
    
  ; Realize the proper controls.
  CASE type OF
    'LOG': BEGIN
      Widget_Control, logBaseID, Map=1
      currentMappedBase = logBaseID
    END
    'GAMMA': BEGIN
      Widget_Control, gammaBaseID, Map=1
      currentMappedBase = gammaBaseID
    END
    'ASINH': BEGIN
      Widget_Control, asinhBaseID, Map=1
      currentMappedBase = asinhBaseID
    END
    'GAUSSIAN': BEGIN
      Widget_Control, gaussBaseID, Map=1
      currentMappedBase = gaussBaseID
    END
    ELSE: currentMappedBase = -1L
  ENDCASE
  Widget_Control, histo_tlb, /Realize, MAP = ~no_map_gui, SET_UVALUE =self
  
  ; Create a pixmap window for moving and erasing the histogram
  ; threshold bars.
  Window, XSize=histXsize, YSize=histYsize, /Free, /Pixmap
  pixmap = !D.Window
  
  ; Create an image window for displaying the image.
  IF NOT Keyword_Set(no_window) THEN BEGIN
  
    Widget_Control, histo_tlb, TLB_Get_Offset=offsets, TLB_Get_Size=sizes, SET_UVALUE=self
    xoff = offsets[0] + sizes[0] + 20
    yoff = offsets[1]
    aspect = Float(xsize)/ysize
    IF xsize GT 512 OR ysize GT 512 THEN BEGIN
      IF xsize NE ysize THEN BEGIN
        aspect = Float(ysize) / xsize
        IF aspect LT 1 THEN BEGIN
          xsize = 512
          ysize = (512 * aspect) < 512
        ENDIF ELSE BEGIN
          ysize = 512
          xsize = (512 / aspect) < 512
        ENDELSE
      ENDIF ELSE BEGIN
        ysize = 512
        xsize = 512
      ENDELSE
    ENDIF
    image_tlb = Widget_Base(Row=1, Group_Leader=histo_tlb, Title='AS_XStretch Image', $
      XOffSet=xoff, YOffSet=yoff, TLB_Size_Events=1, XPad=0, YPad=0)
    image_draw = Widget_Draw(image_tlb, XSize=xsize, YSize=ysize, $
      Kill_Notify='AS_XStretch_ImageWindowKilled', UValue=[saveAs, printit, colorsID])
    Widget_Control, image_tlb, /Realize
    
    ; Get window index numbers for the draw widgets.
    Widget_Control, image_draw, Get_Value=windex
    
    ; If this window closes, the whole application exits.
    Widget_Control, histo_tlb, Group_Leader=image_tlb
    
  ENDIF ELSE BEGIN
  
    ; Must have values for info structure.
    image_tlb = -1L
    image_draw = -1L
    windex = -1L
    
  ENDELSE
  
;  Widget_Control, testWindowWidget, GET_VALUE = testWin
;  testWin.Select
;  p = plot((dist(100))[*,0],/fill_background, /CURRENT)
  
  ; Need identifier of histogram window.
  Widget_Control, histo_draw, Get_Value=histo_wid
  
  ; Load the color table.
  IF N_Elements(palette) EQ 0 THEN $
    LoadCt, 0 > ctable < 40, /Silent ELSE $
    TVLCT, palette
  TVLCT, r, g, b, /Get
  
  ; Start with no stretch on both ends if not specified as keyword.
  
  maxVal = Max(Double(*image))
  IF ~KeyWord_Set(maxThresh) THEN maxThresh = Float(maxVal)
  minVal = Min(Double(*image))
  IF ~KeyWord_Set(minThresh) THEN minThresh = Float(minVal)
  range = maxVal - minVal
  IF type EQ 'SQUARE ROOT' THEN BEGIN
    minThresh = SqRt(minThresh)
    maxThresh = SqRt(maxThresh)
  ENDIF
  
  IF ~KeyWord_Set(xzoom) THEN xzoom = [minVal,maxVal] 
  
  ; Store the plotting system variables for later recall.
  pbang = !P
  xbang = !X
  ybang = !Y
  ymin = !Y.CRange[0]
  ymax = !Y.CRange[1]
  xmin = !X.CRange[0]
  xmax = !X.CRange[1]
  
  ; Calculate a value to tell you if you are "close" to a threshold line.
  close = 0.05 * (maxval-minval)
  
  ; How big is the parameter base.
  geo = Widget_Info(paramBaseID, /Geometry)
  pbase_ysize = geo.scr_ysize + 2
  min_xsize = geo.scr_xsize
  
  ; Update self structure with all info to run the program.
  self.image = image                   ; A pointer to the image data
  self.minThresh = minThresh           ; The minimum threshold
  self.maxThresh = maxThresh           ; The maximum threshold
  self.maxValue = maxValue             ; The MAX_VALUE for the Histogram Plot.
  self.colors =  colors                ; The histogram plot drawing colors.
  self.histo_wid = histo_wid           ; The histogram window index number
  self.histo_draw = histo_draw         ; The histogram draw widget ID.
  self.image_draw = image_draw         ; The image draw widget ID.
  self.windex = windex                 ; The image window index
  self.ymin = ymin                     ; The ymin in data coordinates
  self.ymax = ymax                     ; The ymax in data coordinates
  self.xmin = xmin                     ; The xmin in data coordinates
  self.xmax = xmax                     ; The xmax in data coordinates
  self.r = r                           ; The R color vector for the image
  self.g = g                           ; The G color vector for the image
  self.b = b                           ; The B color vector for the image
  self.pbang = pbang                   ; The !P system variable.
  self.xbang = xbang                   ; The !X system variable.
  self.ybang = ybang                   ; The !Y system variable.
  self.lineby = 'MIN'                  ; The line you are close to.
  self.linex = minThresh               ; The x coordinate of line (data coords).
  self.pixmap = pixmap                 ; The pixmap window index
  self.minval = minval                 ; The minimum intensity value of the data
  self.maxval = maxval                 ; The maximum intensity value of the data
  self.notify_pro = Ptr_New(notify_pro); The name of a procedure to notify when the image is stretched.
  self.notify_obj = Ptr_New(notify_obj); The object reference and method to notify when image is stretched.
  self.updatecbdata = updatecbdata     ; Variable passed from user that is to be passed to call back routine on update.
  self.no_window = no_window           ; A flag that, if set, means no image window.
  self.extra = extra                   ; The extra keywords for the Plot command.
  self.pix_xsize = histXsize           ; The X size of the pixmap.
  self.pix_ysize = histYsize           ; The Y size of the pixmap.
  self.newPointer = newPointer         ; A flag that indicates if we made a pointer or not.
  self.saveAs = saveAs                 ; The SaveAs button widget identifier.
  self.printIt = printIt               ; The Print button widget identifier.
  self.gamma =  gamma                  ; The gamma value.
  self.beta =  beta                    ; The "softenting parameter" for ASINH scaling.
  self.logBaseID =  logBaseID          ; The base widget ID of the LOG parameters.
  self.gammaBaseID =  gammaBaseID      ; The base widget ID of the GAMMA parameters.
  self.asinhBaseID =  asinhBaseID      ; The base widget ID of the ASINH parameters.
  self.gaussBaseID =  gaussBaseID      ; The base widget ID of the GAUSSIAN parameters.
  self.currentMappedBase =  currentMappedBase ; The current base mapped into the control base.
  self.exponent =  exponent            ; The exponent value.
  self.mean =  mean                    ; The mean value.
  self.param1Obj =  param1Obj          ; The first parameter widget.
  self.param2Obj =  param2Obj          ; The second parameter widget.
  self.gamma_comboID =  gamma_comboID  ; The gamma control combobox widget ID.
  self.asinh_comboID =  asinh_comboID  ; The asinh control combobox widget ID.
  self.pbase_ysize =  pbase_ysize      ; The y size of the parameter base.
  self.min_xsize =  min_xsize          ; The minimum X size for the draw widget.
  self.negative =  negative            ; Want a negative image.
  self.type =  type                    ; The type of scaling requested.
  self.minthreshObj =  minthreshObj    ; The minThresh object widget.
  self.maxthreshObj =  maxthreshObj    ; The maxThresh object widget.
  self.sigmaObj =  sigmaObj            ; The sigma object widget.
  self.sigma =  sigma                  ; The sigma value for Gaussian stretch.
  self.colorsID = colorsID             ; The Image Colors button widget identifier.
  self.range =  range                  ; The image data range.
  self.close = close                   ; A value to indicate closeness to line
  self.Histo_tlb = Histo_tlb           ; The widget ID for the TLB.
  self.xZoom  =  xZoom                 ; 2 element array for zoom extents.


    
  ; Draw the histogram. Keep this in FRONT of storing plotting system variables!
  WSet, histo_wid
  self->Histoplot, WID=histo_wid, $
    MaxValue=maxValue, _Extra=*extra
    
  ; Put the same plot in the pixmap.
  WSet, pixmap
  Device, Copy=[0, 0, histXsize, histYsize, 0, 0, histo_wid]
  
  ; Display the image after thresholding.
  displayImage = self->ScaleImage()
  IF NOT Keyword_Set(no_window) THEN BEGIN
    WSet, windex
    LoadCT, ctable, /Silent
    WShow, windex
    TVImage, displayImage, /NoInterp, _Extra=*extra
  ENDIF
  
  ; Set proper threshold values.
  minThreshObj -> Set_Value, self->VALIDATE_THRESHOLD(minThresh), /FloatValue
  maxThreshObj -> Set_Value, self->VALIDATE_THRESHOLD(maxThresh), /FloatValue
  
  ; Draw threshold lines.
  self->DrawLines, minThresh, maxThresh
  
  ; Notify others of image change.
  self->NotifyOthers
  
  ; Bring the histogram window forward with SHOW.
  Widget_Control, histo_tlb, /Show
  IF cWinID GE 0 THEN WSet, cWinID
  
  XManager, 'AS_XStretch', histo_tlb, Group=group, No_Block=1-Keyword_Set(block), $
  Event_Handler='AS_XStretch_Histogram_Resize'
  
  RETURN, 1

END

PRO AS_XStretch__define

void = { AS_XStretch, $
          image                 : Ptr_New(), $                   ; A pointer to the image data
          minThresh             : 0.0, $           ; The minimum threshold
          maxThresh             : 0.0, $           ; The maximum threshold
          maxValue              : 0.0, $             ; The MAX_VALUE for the Histogram Plot.
          colors                : StrArr(6), $                ; The histogram plot drawing colors.
          histo_wid             : 0L, $           ; The histogram window index number
          histo_draw            : 0L, $         ; The histogram draw widget ID.
          image_draw            : 0L, $         ; The image draw widget ID.
          windex                : 0L, $                 ; The image window index
          ymin                  : 0.0, $                     ; The ymin in data coordinates
          ymax                  : 0.0, $                     ; The ymax in data coordinates
          xmin:0.0, $                     ; The xmin in data coordinates
          xmax:0.0, $                     ; The xmax in data coordinates
          r:IntArr(256), $                           ; The R color vector for the image
          g:IntArr(256), $                           ; The G color vector for the image
          b:IntArr(256), $                           ; The B color vector for the image
          pbang:!P, $                   ; The !P system variable.
          xbang:!X, $                   ; The !X system variable.
          ybang:!Y, $                   ; The !Y system variable.
          lineby:'', $                  ; The line you are close to.
          linex:0.0, $               ; The x coordinate of line (data coords).
          pixmap:0, $                 ; The pixmap window index
          minval:0.0, $                 ; The minimum intensity value of the data
          maxval:0.0, $                 ; The maximum intensity value of the data
          notify_pro:Ptr_New(), $         ; The name of a procedure to notify when the image is stretched.
          notify_obj:Ptr_New(), $         ; The object reference and method to notify when image is stretched.
          updatecbdata:'', $               ; Variable passed from user that is to be passed to call back routine on update.
          no_window:0, $           ; A flag that, if set, means no image window.
          extra:Ptr_New(), $                   ; The extra keywords for the Plot command.
          pix_xsize:0, $           ; The X size of the pixmap.
          pix_ysize:0, $           ; The Y size of the pixmap.
          newPointer:0, $         ; A flag that indicates if we made a pointer or not.
          saveAs:0L, $                 ; The SaveAs button widget identifier.
          printIt:0L, $               ; The Print button widget identifier.
          gamma: 0.0, $                  ; The gamma value.
          beta: 0.0, $                    ; The "softenting parameter" for ASINH scaling.
          logBaseID: 0L, $          ; The base widget ID of the LOG parameters.
          gammaBaseID: 0L, $      ; The base widget ID of the GAMMA parameters.
          asinhBaseID: 0L, $      ; The base widget ID of the ASINH parameters.
          gaussBaseID: 0L, $      ; The base widget ID of the GAUSSIAN parameters.
          currentMappedBase: 0L, $ ;The current base mapped into the control base.
          exponent: 0.0, $            ; The exponent value.
          mean: 0.0, $                    ; The mean value.
          param1Obj: Obj_New(), $          ; The first parameter widget.
          param2Obj: Obj_New(), $          ; The second parameter widget.
          gamma_comboID: 0L, $  ; The gamma control combobox widget ID.
          asinh_comboID: 0L, $  ; The asinh control combobox widget ID.
          pbase_ysize: 0, $      ; The y size of the parameter base.
          min_xsize: 0, $          ; The minimum X size for the draw widget.
          negative: 0, $            ; Want a negative image.
          type: '', $                    ; The type of scaling requested.
          minthreshObj: Obj_New(), $    ; The minThresh object widget.
          maxthreshObj: Obj_New(), $    ; The maxThresh object widget.
          sigmaObj: Obj_New(), $            ; The sigma object widget.
          sigma: 0.0, $                  ; The sigma value for Gaussian stretch.
          colorsID:0L, $             ; The Image Colors button widget identifier.
          range: 0.0, $                  ; The image data range.
          close:0.0, $                   ; A value to indicate closeness to line
          Histo_tlb:0L, $           ; The widget ID for the TLB.
          xZoom : [0d,0d] }       ; 2 element array for zoom extents.

END