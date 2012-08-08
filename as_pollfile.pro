;*************************************************************************
; Copyright (c) 2008 Australian Synchrotron
;*************************************************************************
;+
; NAME:
; AS_PollFile
;
; PURPOSE:
; Checks to see if file is readable.
;
; CATEGORY:
; Files
;
; CALLING SEQUENCE:
;
; Result = AS_PollFile(f_name)
;
; INPUTS:
; f_name:  Name of file to be polled.
;
; OUTPUTS:
; Returns 1 if file can be opened successfully. Returns 0 if not.
;
; MODIFICATION HISTORY:
;   Written by: David J. Cookson
;   16-May-08 (STM) Name changed (from saxs_poll_file) and updated to AS convention.
;                   Removed specific LUN used /GET_LUN to OPENR instead keyword.
;-
FUNCTION AS_PollFile, f_name

        ON_IOERROR, no_file

        OPENR, LUN, f_name, /GET_LUN
        CLOSE, LUN
        RETURN, 1

    no_file: RETURN, 0
END