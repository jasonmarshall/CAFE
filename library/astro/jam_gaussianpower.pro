;+ ===========================================================================
; NAME:
;       Jam_GaussianPower
;
; PURPOSE:
;       This function calculates the power from a series of Gaussian profiles.
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_GaussianPower(Gauss)
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

FUNCTION Jam_GaussianPower, gauss, EGauss=eGauss, Covar=covar, $
                            EPower=ePower, Lines=lines
  
  Compile_Opt IDL2
  On_Error, 2

  ;; Get Gaussian profile parameters.
  IF (Jam_Check(gauss, TName='STRUCT')) THEN BEGIN
     N_LINE = N_Elements(gauss.Wave0)
     wave0 = gauss.Wave0
     gamma = gauss.Gamma
     peak = gauss.Peak
  ENDIF $
  ELSE BEGIN
     N_LINE = N_Elements(gauss) / 3
     wave0 = gauss[0:N_LINE-1]
     gamma = gauss[N_LINE:2*N_LINE-1]
     peak = gauss[2*N_LINE:3*N_LINE-1]
  ENDELSE

  ;; Convert peak vector units from [Jy] to [erg s-1 cm-2 um-1].
  peak = Jam_Jy2um(wave0, peak)
  
  ;; Calculate power.
  FWHM = wave0 * gamma
  power = Sqrt(!DPi / (4.D * ALog(2.D))) * FWHM * peak

  ;; Calculate total power of complex if 'Lines' index vector is given.
  IF (N_Elements(lines) NE 0) THEN power = Total(power[lines])
  
  ;; Calculate power error.
  IF (Arg_Present(ePower)) THEN BEGIN
     _gauss = [wave0, gamma, peak]
     IF (N_Elements(eGauss) NE 0) THEN $
        _eGauss = [eGauss.Wave0, eGauss.Gamma, eGauss.Peak]
     IF (N_Elements(lines) NE 0) THEN BEGIN
        index = [lines, lines+N_LINE, lines+2*N_LINE]
        derivs = Jam_Deriv('Jam_GaussianPower', _gauss, EGauss=_eGauss, $
                           Covar=covar, Index=index, Lines=lines)
        ePower = Jam_EFunc(derivs, _eGauss, Covar=covar, Index=index)
     ENDIF $
     ELSE BEGIN
        ePower = DblArr(N_LINE)
        FOR i=0,N_LINE-1 DO BEGIN
           index = [i, i+N_LINE, i+2*N_LINE]
           derivs = Jam_Deriv('Jam_GaussianPower', _gauss, EGauss=_eGauss, $
                              Covar=covar, Index=index, Lines=i)
           ePower[i] = Jam_EFunc(derivs, _eGauss, Covar=covar, Index=index)
        ENDFOR
     ENDELSE
  ENDIF
    
  RETURN, power
  
END ;; -----------------------------------------------------------------------
