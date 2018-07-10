;+ ===========================================================================
; NAME:
;       Jam_FeaturePower
;
; PURPOSE:
;       This function calculates the power from a series of Gaussian and
;       Drude profiles [erg s-1 cm-2].
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_FeaturePower(Params)
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

FUNCTION Jam_FeaturePower, params, EParams=eParams, Covar=covar, $
                           EPower=ePower, Features=features, Profile=profile
  
  Compile_Opt IDL2
  On_Error, 2

  ;; Get profile parameters.
  IF (Jam_Check(params, TName='STRUCT')) THEN BEGIN
     N_FEAT = N_Elements(params.Wave0)
     IF (N_Elements(profile) EQ 0) THEN BEGIN
        tags = Tag_Names(params)
        idx = Where(tags EQ 'PROFILE', cnt)
        profile = (cnt EQ 1) ? params.(idx) : Replicate('G', N_FEAT)
     ENDIF
     wave0 = params.Wave0
     gamma = params.Gamma
     peak = params.Peak
  ENDIF $
  ELSE BEGIN
     N_FEAT = N_Elements(params) / 3
     wave0 = params[0:N_FEAT-1]
     gamma = params[N_FEAT:2*N_FEAT-1]
     peak = params[2*N_FEAT:3*N_FEAT-1]
     IF (N_Elements(profile) EQ 0) THEN profile = Replicate('G', N_FEAT)
  ENDELSE

  ;; Convert peak vector units from [Jy] to [erg s-1 cm-2 um-1].
  peak = Jam_Jy2um(wave0, peak)
  
  ;; Calculate power.
  idxG = Where(profile EQ 'G', cntG)
  idxD = Where(profile EQ 'D', cntD)
  FWHM = wave0 * gamma
  IF (cntG GT 0) THEN $
     powerG = Sqrt(!DPi / (4.D * ALog(2.D))) * FWHM[idxG] * peak[idxG]
  IF (cntD GT 0) THEN $
     powerD = !DPi / 2.D * FWHM[idxD] * peak[idxD]
  power = DblArr(N_FEAT)
  IF (cntG GT 0) THEN power[idxG] = powerG
  IF (cntD GT 0) THEN power[idxD] = powerD

  ;; Calculate total power of complex if 'Features' index vector is given.
  IF (N_Elements(features) NE 0) THEN power = Total(power[features])
  
  ;; Calculate power error.
  IF (Arg_Present(ePower)) THEN BEGIN
     _params = [wave0, gamma, peak]
     IF (N_Elements(eParams) NE 0) THEN $
        _eParams = [eParams.Wave0, eParams.Gamma, eParams.Peak]
     IF (N_Elements(features) NE 0) THEN BEGIN
        index = [features, features+N_FEAT, features+2*N_FEAT]
        derivs = Jam_Deriv('Jam_FeaturePower', _params, EParams=_eParams, $
                           PError=_eParams, Covar=covar, Index=index, $
                           Features=features, Profile=profile)
        ePower = Jam_EFunc(derivs, _eParams, Covar=covar, Index=index)
     ENDIF $
     ELSE BEGIN
        ePower = DblArr(N_FEAT)
        FOR i=0,N_FEAT-1 DO BEGIN
           index = [i, i+N_FEAT, i+2*N_FEAT]
           derivs = Jam_Deriv('Jam_FeaturePower', _params, EParams=_eParams, $
                              PError=_eParams, Covar=covar, Index=index, $
                              Features=i, Profile=profile, /Check)
           ePower[i] = Jam_EFunc(derivs, _eParams, Covar=covar, Index=index)
        ENDFOR
     ENDELSE
  ENDIF

  RETURN, power
  
END ;; -----------------------------------------------------------------------
