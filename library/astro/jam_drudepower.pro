;+ ===========================================================================
; NAME:
;       Jam_DrudePower
;
; PURPOSE:
;       This function calculates the power from a series of Drude profiles
;       [erg s-1 cm-2].
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_DrudePower(Gauss)
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

FUNCTION Jam_DrudePower, drude, EDrude=eDrude, Covar=covar, EPower=ePower, $
                         Lines=lines
  
  Compile_Opt IDL2
  On_Error, 2

  ;; Get Drude profile parameters.
  IF (Jam_Check(drude, TName='STRUCT')) THEN BEGIN
     N_LINE = N_Elements(drude.Wave0)
     wave0 = drude.Wave0
     gamma = drude.Gamma
     peak = drude.Peak
  ENDIF $
  ELSE BEGIN
     N_LINE = N_Elements(drude) / 3
     wave0 = drude[0:N_LINE-1]
     gamma = drude[N_LINE:2*N_LINE-1]
     peak = drude[2*N_LINE:3*N_LINE-1]
  ENDELSE

  ;; Convert peak vector units from [Jy] to [erg s-1 cm-2 um-1].
  peak = Jam_Jy2um(wave0, peak)

  ;; Calculate power.
  FWHM = wave0 * gamma
  power = !DPi / 2.D * FWHM * peak

  ;; Calculate total power of complex if 'Lines' index vector is given.
  IF (N_Elements(lines) NE 0) THEN power = Total(power[lines])

  ;; Calculate power error.
  IF (Arg_Present(ePower)) THEN BEGIN
     _drude = [wave0, gamma, peak]
     IF (N_Elements(eDrude) NE 0) THEN $
        _eDrude = [eDrude.Wave0, eDrude.Gamma, eDrude.Peak]
     IF (N_Elements(lines) NE 0) THEN BEGIN
        index = [lines, lines+N_LINE, lines+2*N_LINE]
        derivs = Jam_Deriv('Jam_DrudePower', _drude, EDrude=_eDrude, $
                           Covar=covar, Index=index, Lines=lines)
        ePower = Jam_EFunc(derivs, _eDrude, Covar=covar, Index=index)
     ENDIF $
     ELSE BEGIN
        ePower = DblArr(N_LINE)
        FOR i=0,N_LINE-1 DO BEGIN
           index = [i, i+N_LINE, i+2*N_LINE]
           derivs = Jam_Deriv('Jam_DrudePower', _drude, EDrude=_eDrude, $
                              Covar=covar, Index=index, Lines=i)
           ePower[i] = Jam_EFunc(derivs, _eDrude, Covar=covar, Index=index)
        ENDFOR
     ENDELSE
  ENDIF

  RETURN, power

END ;; -----------------------------------------------------------------------
