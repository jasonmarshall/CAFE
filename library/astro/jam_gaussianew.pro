;+ ===========================================================================
; NAME:
;       Jam_GaussianEW
;
; PURPOSE:
;       This function calculates the equivalent width (EW) of a series of
;       Gaussian profiles.
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_GaussianEW(Wave, Continuum, Gauss)
;
; AUTHOR:
;       Jason A. Marshall
;       Department of Astronomy, Cornell University, Ithaca, NY 14853
;       jam258@cornell.edu
;
; MODIFICATION HISTORY:
;       Written by: Jason A. Marshall, 17 Nov 2006.
;-
;; ***************************************************************************
; Copyright (C) 2006, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ***************************************************************************

FUNCTION Jam_GaussianEW, wave, continuum, gauss, EGauss=eGauss, Covar=covar, $
                         EContinuum=eContinuum, EEW=eEW, Lines=lines
  
  Compile_Opt IDL2
  On_Error

  ;; Calculate line power.
  power = Jam_GaussianPower(gauss, EGauss=eGauss, Covar=covar, $
                            EPower=ePower, Lines=lines)
  
  ;; Calculate EW.
  N_LINE = (N_Elements(lines) EQ 0) ? N_Elements(gauss.Wave0) : 1
  logWave = ALog(wave)
  wave0 = (N_Elements(lines) EQ 0) ? gauss.Wave0 : Mean(gauss.Wave0[lines])
  logWave0 = ALog(wave0)
  continuum0 = Interpol(continuum, logWave, logWave0)
  EW = power / continuum0
   
  ;; Calculate EW error.
  IF (Arg_Present(eEW)) THEN BEGIN
     eContinuum0 = (N_Elements(eContinuum) NE 0) ? $
                   Interpol(eContinuum, logWave, logWave0) : DblArr(N_LINE)
     eEW = Sqrt((ePower / continuum0)^2 + $
                (power / continuum0^2 * eContinuum0)^2)
  ENDIF
  
  RETURN, EW
  
END ;; -----------------------------------------------------------------------
