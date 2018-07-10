;+ ===========================================================================
; NAME:
;       Jam_FeatureFlux
;
; PURPOSE:
;       This function calculates the flux from a series of Gaussian and
;       Drude profiles.
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_FeatureFlux(Wave, Params)
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

FUNCTION Jam_FeatureFlux, wave, params, EParams=eParams, Covar=covar, $
                          EFlux=eFlux, Features=features, Profile=_profile, $
                          ExtDrude=extDrude

  Compile_Opt IDL2
  On_Error, 2

  ;; Get profile parameters.
  IF (Jam_Check(params, TName='STRUCT')) THEN BEGIN
     N_FEAT = N_Elements(params.Wave0)
     IF (N_Elements(_profile) EQ 0) THEN BEGIN
        tags = Tag_Names(params)
        idx = Where(tags EQ 'PROFILE', cnt)
        _profile = (cnt EQ 1) ? params.(idx) : Replicate('G', N_FEAT)
     ENDIF
     _wave0 = params.Wave0
     _gamma = params.Gamma
     _peak = params.Peak
  ENDIF $
  ELSE BEGIN
     N_FEAT = N_Elements(params) / 3
     _wave0 = params[0:N_FEAT-1]
     _gamma = params[N_FEAT:2*N_FEAT-1]
     _peak = params[2*N_FEAT:3*N_FEAT-1]
     IF (N_Elements(_profile) EQ 0) THEN _profile = Replicate('G', N_FEAT)
  ENDELSE

  ;; Create default 'Features' index vector.
  IF (N_Elements(features) EQ 0) THEN features = LIndGen(N_FEAT)
  wave0 = _wave0[features]
  gamma = _gamma[features]
  peak = _peak[features]
  profile = _profile[features]
  
  ;; Calculate flux from Gaussian and Drude profile.
  N_WAVE = N_Elements(wave)
  fluxG = DblArr(N_WAVE)
  fluxD = DblArr(N_WAVE)
  idxG = Where(profile EQ 'G', cntG)
  idxD = Where(profile EQ 'D', cntD)
  IF (cntG GT 0) THEN BEGIN
     FWHM = wave0 * (gamma > 1D-5)
     sigma = FWHM / (2.D * Sqrt(2.D * ALog(2.D)))
     A0 = peak[idxG]
     A1 = wave0[idxG]
     A2 = sigma[idxG]
     FOR i=0,cntG-1 DO BEGIN
        thisFlux = A0[i] * Exp(-0.5D * ((wave - A1[i]) / A2[i])^2)
        fluxG += thisFlux
     ENDFOR
  ENDIF
  IF (cntD GT 0) THEN BEGIN
     A0 = gamma[idxD]^2 * peak[idxD]
     A1 = wave0[idxD]
     A2 = gamma[idxD]
     FOR i=0,cntD-1 DO BEGIN
        thisFlux = A0[i] / (((wave / A1[i]) - (A1[i] / wave))^2 + A2[i]^2)
        fluxD += thisFlux
     ENDFOR
     IF (N_Elements(extDrude) NE 0) THEN fluxD *= extDrude
  ENDIF
  flux = fluxG + fluxD
  
  ;; Calculate flux error.
  IF (Arg_Present(eFlux)) THEN BEGIN
     _params = [_wave0, _gamma, _peak]
     IF (N_Elements(eParams) NE 0) THEN $
        _eParams = [eParams.Wave0, eParams.Gamma, eParams.Peak]
     index = [features, features+N_FEAT, features+2*N_FEAT]
     derivs = Jam_Deriv('Jam_FeatureFlux', wave, _params, PError=_eParams, $
                        EParams=_eParams, Covar=covar, Index=index, $
                        Features=features, Profile=_profile)
     eFlux = Jam_EFunc(derivs, _eParams, Covar=covar, Index=index)
  ENDIF

  RETURN, flux
  
END ;; -----------------------------------------------------------------------
