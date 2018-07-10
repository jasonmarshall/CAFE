;+ ===========================================================================
; NAME:
;       Jam_GammaIRS
;
; PURPOSE:
;       This function calculates the effective gamma (= FWHM / wave0) at
;       wavelength positions along the Spitzer/IRS low-res detectors.
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_GammaIRS(Wave0, Redshift)
;
; AUTHOR:
;       Jason A. Marshall
;       Department of Astronomy, Cornell University, Ithaca, NY 14853
;       jam258@cornell.edu
;
; MODIFICATION HISTORY:
;       Written by: Jason A. Marshall, 04 Mar 2006.
;-
;; ---------------------------------------------------------------------------
; Copyright (C) 2006, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ---------------------------------------------------------------------------

FUNCTION Jam_GammaIRS, _wave0, redshift, Debug=debug

  ;; Set compiler options and error handling.
  Compile_Opt IDL2
  IF (~Keyword_Set(debug)) THEN On_Error, 2

  ;; Set debugging options.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  ;; Set default redshift.
  IF (N_Elements(redshift) EQ 0) THEN redshift = 0.D

  ;; Calculate wavelength vectors for each IRS module and order.
  minIRS = 5.2D
  maxIRS = 40.D
  waveIRS = Jam_Dist(380, minIRS, maxIRS)
  limSL2 = [5.2, 7.4]
  idxSL2 = Where(waveIRS LT limSL2[1])
  waveSL2 = waveIRS[idxSL2]
  limSL1B = [7.4, 14.25]
  idxSL1B = Where((waveIRS GE limSL1B[0]) AND (waveIRS LT limSL1B[1]))
  waveSL1B = waveIRS[idxSL1B]
  limLL2 = [14.25, 19.5]
  idxLL2 = Where((waveIRS GE limLL2[0]) AND (waveIRS LT limLL2[1]))
  waveLL2 = waveIRS[idxLL2]
  limLL1B = [19.5, 40.1]
  idxLL1B = Where(waveIRS GE limLL1B[0])
  waveLL1B = waveIRS[idxLL1B]

  ;; Calculate R for the different IRS modules and orders.
  R_SL2 = 16.5333D * waveSL2
  R_SL1B = 8.2667D * waveSL1B
  R_LL2 = 5.9048D * waveLL2
  R_LL1B = 2.9524 * waveLL1B
  R_IRS = [R_SL2, R_SL1B, R_LL2, R_LL1B]

  ;; Calculate the corresponding values of gammaIRS (= 1 / R_IRS).
  gammaIRS = 1.D / R_IRS

  ;; Calculate the observed-frame input wavelengths.
  wave0 = (1.D + redshift) * _wave0

  ;; Interpolate to find gamma values at input wavelengths.
  gamma = Interpol(gammaIRS, waveIRS, wave0)

  ;; Find the effective gamma (i.e. the value that will give the correct
  ;; FWHM when multiplied by the rest-wavelength).
  gamma *= (wave0 / _wave0)

  ;; Exclude observed wavelengths less-than or greater-than the IRS range.
  idx = Where((wave0 LT minIRS) OR (wave0 GT maxIRS), cnt)
  IF (cnt GT 0) THEN gamma[idx] = 0.D

  ;; Restore debugging options.
  !Except = except

  RETURN, gamma

END
