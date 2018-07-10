;+ ===========================================================================
; NAME:
;       Jam_DrudeFlux
;
; PURPOSE:
;       This function calculates the flux from a series of Drude profiles.
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_DrudeFlux(Wave, Drude)
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

FUNCTION Jam_DrudeFlux, wave, drude, EDrude=eDrude, Covar=covar, $
                        EFlux=eFlux, Lines=lines

  Compile_Opt IDL2
  On_Error, 2

  ;; Get Drude profile parameters.
  IF (Jam_Check(drude, TName='STRUCT')) THEN BEGIN
     N_LINE = N_Elements(drude.Wave0)
     _wave0 = drude.Wave0
     _gamma = drude.Gamma
     _peak = drude.Peak
  ENDIF $
  ELSE BEGIN
     N_LINE = N_Elements(drude) / 3
     _wave0 = drude[0:N_LINE-1]
     _gamma = drude[N_LINE:2*N_LINE-1]
     _peak = drude[2*N_LINE:3*N_LINE-1]
  ENDELSE

  ;; Create default 'Lines' index vector.
  IF (N_Elements(lines) EQ 0) THEN BEGIN
     lines = LIndGen(N_LINE)
  ENDIF ELSE $
     N_LINE = N_Elements(lines)
  wave0 = _wave0[lines]
  gamma = _gamma[lines]
  peak = _peak[lines]
  
  ;; Calculate set of Drude profile parameters.
  A0 = gamma^2 * peak
  A1 = wave0
  A2 = gamma

  ;; Calculate flux.
  flux = DblArr(N_Elements(wave))
  FOR i=0,N_LINE-1 DO BEGIN
     thisFlux = A0[i] / (((wave / A1[i]) - (A1[i] / wave))^2 + A2[i]^2)
     flux += thisFlux
  ENDFOR
  
  ;; Calculate flux error.
  IF (Arg_Present(eFlux)) THEN BEGIN
     _drude = [_wave0, _gamma, _peak]
     IF (N_Elements(eDrude) NE 0) THEN $
        _eDrude = [eDrude.Wave0, eDrude.Gamma, eDrude.Peak]
     index = [lines, lines+N_LINE, lines+2*N_LINE]
     derivs = Jam_Deriv('Jam_DrudeFlux', wave, _drude, EDrude=_eDrude, $
                        Covar=covar, Index=index, Lines=lines)
     eFlux = Jam_EFunc(derivs, _eDrude, Covar=covar, Index=index)
  ENDIF

  RETURN, flux
  
END ;; -----------------------------------------------------------------------
