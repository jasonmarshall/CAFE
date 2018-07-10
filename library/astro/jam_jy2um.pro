;+ ===========================================================================
; NAME:
;       Jam_Jy2um
;
; PURPOSE:
;       This function converts a flux vector from [Jy]->[erg s-1 cm-2 um-1].
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_Jy2um()
;
; AUTHOR:
;       Jason A. Marshall
;       Department of Astronomy, Cornell University, Ithaca, NY 14853
;       jam258@cornell.edu
;
; MODIFICATION HISTORY:
;       Written by: Jason A. Marshall, 19 Nov 2006.
;-
;; ***************************************************************************
; Copyright (C) 2006, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ***************************************************************************

FUNCTION Jam_Jy2um, wave, flux
  
  Compile_Opt IDL2, Hidden
  On_Error, 2
  
  scale = 3D-9 ;; = 3D14 * 1D-23
  RETURN, scale / wave^2 * flux
  
END;; ------------------------------------------------------------------------
