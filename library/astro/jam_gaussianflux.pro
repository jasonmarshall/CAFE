;+ ===========================================================================
; NAME:
;       Jam_GaussianFlux
;
; PURPOSE:
;       This function calculates the flux from a series of Gaussian profiles.
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_GaussianFlux(Wave, Gauss)
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
; Copyright (C) 2006, 2007, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ***************************************************************************

FUNCTION Jam_GaussianFlux, wave, gauss, EGauss=eGauss, Covar=covar, $
                           EFlux=eFlux, Lines=lines
  
  Compile_Opt IDL2
  On_Error, 2

  ;; Get Gaussian profile parameters.
  IF (Jam_Check(gauss, TName='STRUCT')) THEN BEGIN
     N_LINE = N_Elements(gauss.Wave0)
     _wave0 = gauss.Wave0
     _gamma = gauss.Gamma
     _peak = gauss.Peak
  ENDIF $
  ELSE BEGIN
     N_LINE = N_Elements(gauss) / 3
     _wave0 = gauss[0:N_LINE-1]
     _gamma = gauss[N_LINE:2*N_LINE-1]
     _peak = gauss[2*N_LINE:3*N_LINE-1]
  ENDELSE

  ;; Create default 'Lines' index vector.
  IF (N_Elements(lines) EQ 0) THEN BEGIN
     lines = LIndGen(N_LINE)
  ENDIF ELSE $
     N_LINE = N_Elements(lines)
  wave0 = _wave0[lines]
  gamma = _gamma[lines]
  peak = _peak[lines]
  
  ;; Calculate set of Gaussian profile parameters.
  FWHM = wave0 * (gamma > 1D-5)
  sigma = FWHM / (2.D * Sqrt(2.D * ALog(2.D)))
  A0 = peak
  A1 = wave0
  A2 = sigma

  ;; Calculate flux.
  flux = DblArr(N_Elements(wave))
  FOR i=0,N_LINE-1 DO BEGIN
     thisFlux = A0[i] * Exp(-0.5D * ((wave - A1[i]) / A2[i])^2)
     flux += thisFlux
  ENDFOR
  
  ;; Calculate flux error.
  IF (Arg_Present(eFlux)) THEN BEGIN
     _gauss = [_wave0, _gamma, _peak]
     IF (N_Elements(eGauss) NE 0) THEN $
        _eGauss = [eGauss.Wave0, eGauss.Gamma, eGauss.Peak]
     index = [lines, lines+N_LINE, lines+2*N_LINE]
     derivs = Jam_Deriv('Jam_GaussianFlux', wave, _gauss, EGauss=_eGauss, $
                        Covar=covar, Index=index, Lines=lines)
     eFlux = Jam_EFunc(derivs, _eGauss, Covar=covar, Index=index)
  ENDIF

  RETURN, flux
  
END ;; -----------------------------------------------------------------------
