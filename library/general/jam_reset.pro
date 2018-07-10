;+ ===========================================================================
; NAME:
;       JAM_Reset
;
; PURPOSE:
;       This batch file is used to reset the IDL session. 
;       Note: the name *must* be input in all lower-case!
;
; CATEGORY:
;       Utility
;
; CALLING SEQUENCE:
;       @jam_reset (Must be lower-case)
;       
; PROCEDURE:
;       Call .FULL_RESET_SESSION, set !D.NAME to 'WIN' or 'X' depending on
;       the OS family, and reset !P.MULTI = 0. 
;
; EXAMPLE:
;       To re-initialize the IDL session enter (must be lower-case):
;          IDL> @jam_reset
;
; AUTHOR:
;       Jason A. Marshall
;       CalTech/JPL, 4800 Oak Grove Dr., M/S 169-506, Pasadena, CA 91109
;       jason.marshall@caltech.edu
;
; MODIFICATION HISTORY:
;       Written by: Jason A. Marshall on Oct 01, 2005.
;-
;; ---------------------------------------------------------------------------
; Copyright (C) 2005, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ---------------------------------------------------------------------------

;; IDL executive command reset.
.Full_Reset_Session

;; Reset default output device.
SET_PLOT, (STRUPCASE(!Version.OS_Family) EQ 'WINDOWS') ? 'WIN' : 'X'

;; Reset default !P.Multi
!P.Multi = 0
