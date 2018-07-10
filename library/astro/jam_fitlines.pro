;+ ===========================================================================
; NAME:
;       Jam_FitLines
;
; PURPOSE:
;       This function performs a least-squares fit to mid-IR (~5-35 microns) 
;       emission lines in a continuum subtracted spectrum.
;
; CATEGORY:
;       Spitzer/IRS, Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_FitLines(Wave, Flux[, Sigma])
;
; INPUTS:
;       Wave -
;       Flux -
;       Sigma -
;
; KEYWORD PARAMETERS:
;       Redshift -
;       ...
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

FUNCTION Jam_FitLines, wave, flux, sigma, Weights=weights, $
                       Redshift=redshift, MinWave=minWave, MaxWave=maxWave, $
                       Atomic=atomic, Molecular=molecular, $
                       Wave0=_wave0, Gamma=_gamma, Gauss=_gauss, $
                       InitGauss=initGauss, Initialize=initialize, $
                       FixWave0=fixWave0, FixGamma=fixGamma, FixPeak=fixPeak, $
                       EPSWave0=EPSWave0, EPSGamma=EPSGamma, EPSPeak=EPSPeak, $
                       FTol=fTol, MaxIter=maxIter, Covar=covar, $
                       ChiSqr=chiSqr, DOF=DOF, Flux=_flux, $
                       EFlux=_eFlux, Power=power, EPower=ePower, $
                       EGauss=eGauss, Verbose=verbose

  Compile_Opt IDL2
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() LT 2) THEN $
     Message, Jam_Message('Syntax', Jam_Syntax('Jam_FitLines', /Is_Function))

  ;; Define default sigma and weights.
  N_WAVE = N_Elements(wave)
  IF (N_Elements(sigma) EQ 0) THEN sigma = Replicate(1.D, N_WAVE)
  IF (N_Elements(weights) EQ 0) THEN weights = 1.D / sigma^2

  ;; Set default mininum and maximum fit wavelengths.
  IF (N_Elements(minWave) EQ 0) THEN minWave = Min(wave)
  IF (N_Elements(maxWave) EQ 0) THEN maxWave = Max(wave)

  ;; Create index of wavelengths to fit.
  idxFit = Where((wave GE minWave) AND (wave LE maxWave))
  waveFit = wave[idxFit]
  fluxFit = flux[idxFit]
  sigmaFit = sigma[idxFit]
  weightsFit = weights[idxFit]

  ;; Set-up line parameter vectors.
  IF (N_Elements(_gauss) NE 0) THEN BEGIN

     name = _gauss.Name
     wave0 = _gauss.Wave0
     gamma = _gauss.Gamma
     peak = _gauss.Peak
     idx = Where((wave0 GE minWave) AND (wave0 LE maxWave), cnt)
     IF (cnt GT 0) THEN BEGIN
        name = name[idx]
        wave0 = wave0[idx]
        gamma = gamma[idx]
        peak = peak[idx]
     ENDIF ELSE $
        wave0 = [-1.D]

  ENDIF $
  ELSE BEGIN

     IF (N_Elements(_wave0) GT 0) THEN BEGIN
        idx = Where((_wave0 GE minWave) AND (_wave0 LE maxWave), N_LINE)
        IF (N_LINE GT 0) THEN BEGIN
           wave0 = _wave0[idx]
           name = StrCompress('P' + String(IndGen(N_LINE)), /Remove_All)
        ENDIF
     ENDIF

     ;; Create atomic line profiles.
     IF (Keyword_Set(atomic)) THEN BEGIN
        path = FilePath('lines.atomic.txt', Root_Dir=!Jam.Path.Tables)
        data = Jam_ReadData(path)
        idx_ATO = Where((data.Mask EQ 0) AND $
                        (data.Wave0 GE minWave) AND $
                        (data.Wave0 LE maxWave), cnt_ATO)
        IF (cnt_ATO GT 0) THEN BEGIN
           wave0_ATO = data.Wave0[idx_ATO]
           name_ATO = data.Name[idx_ATO]
           wave0 = (N_Elements(wave0) EQ 0) ? wave0_ATO : [wave0, wave0_ATO]
           name = (N_Elements(name) EQ 0) ? name_ATO : [name, name_ATO]
        ENDIF
     ENDIF

     ;; Create molecular line profiles.
     IF (Keyword_Set(molecular)) THEN BEGIN
        path = FilePath('lines.molecular.txt', Root_Dir=!Jam.Path.Tables)
        data = Jam_ReadData(path)
        idx_MOL = Where((data.Mask EQ 0) AND $
                        (data.Wave0 GE minWave) AND $
                        (data.Wave0 LE maxWave), cnt_MOL)
        IF (cnt_MOL GT 0) THEN BEGIN
           wave0_MOL = data.Wave0[idx_MOL]
           name_MOL = data.Name[idx_MOL]
           wave0 = (N_Elements(wave0) EQ 0) ? wave0_MOL : [wave0, wave0_MOL]
           name = (N_Elements(name) EQ 0) ? name_MOL : [name, name_MOL]
        ENDIF
     ENDIF

     ;; Estimate gamma for each line.
     gamma = Jam_GammaIRS(wave0, redshift)
     N_GAMMA = N_Elements(_gamma)
     IF (N_GAMMA GT 0) THEN BEGIN
        gamma[0:N_LINE-1] = (N_GAMMA EQ N_LINE) ? $
                            _gamma: Replicate(_gamma[0], N_Elements(_wave0))
     ENDIF

     ;; Estimate peak for each line.
     FWHM = gamma * wave0
     N_SIGMA = 2.5D
     scale = N_SIGMA / (2.D * Sqrt(2.D * ALog(2.D)))
     waveMin = wave0 - scale * FWHM
     waveMax = wave0 + scale * FWHM
     fluxMin = Interpol(fluxFit, waveFit, waveMin)
     fluxMax = Interpol(fluxFit, waveFit, waveMax)
     cont = 0.5D * (fluxMin + fluxMax)
     limit = ((Min(sigmaFit) EQ Max(sigmaFit)) AND (sigmaFit[0] EQ 1.D)) ? $
             0.D : Interpol(sigmaFit, waveFit, wave0)
     peak = (Interpol(fluxFit, waveFit, wave0) - cont) > 0.D;; > limit

  ENDELSE

  ;; Sort lines by wavelength.
  N_LINE = N_Elements(wave0)
  IF (wave0[0] EQ -1.D) THEN N_LINE = 0
  IF (N_LINE GT 0) THEN BEGIN
     idx = Sort(wave0)
     name = name[idx]
     wave0 = wave0[idx]
     gamma = gamma[idx]
     peak = peak[idx]
     limit = limit[idx]
  ENDIF

  ;; Return if initializing parameters.
  IF (Keyword_Set(initialize)) THEN BEGIN
     RETURN, (N_LINE EQ 0) ? $
             { Name:[''], Wave0:[-1.D], Gamma:[-1.D], Peak:[-1.D] } : $
             { Name:name, Wave0:wave0, Gamma:gamma, Peak:peak }
  ENDIF

  ;; Make sure line list isn't empty.
  IF (N_LINE EQ 0) THEN $
     Message, Jam_Message('', 'No lines have been defined!')

  ;; Define default parameter variation tolerances.
  IF (N_Elements(EPSWave0) EQ 0) THEN EPSWave0 = 2.5D-3
  IF (N_Elements(EPSGamma) EQ 0) THEN EPSGamma = 0.15D
  IF (N_Elements(EPSPeak) EQ 0) THEN EPSPeak = -1

  ;; Create original parameter values vector.
  IF (N_Elements(initGauss) NE 0) THEN BEGIN
     ;; Wave0
     IF (EPSWave0 NE -1) THEN BEGIN
        _EPSWave0 = (EPSWave0 - Abs(1.D - (wave0 / initGauss.Wave0))) > 0.D
        idx = Where(_EPSWave0 LT 1D-5, cnt)
        IF (cnt GT 0) THEN _EPSWave0[idx] = 1D-5
     ENDIF ELSE $
        _EPSWave0 = Replicate(-1, N_LINE)
     ;; Gamma
     IF (EPSGamma NE -1) THEN BEGIN
        _EPSGamma = (EPSGamma - Abs(1.D - (gamma / initGauss.Gamma))) > 0.D
        idx = Where(_EPSGamma LT 1D-5, cnt)
        IF (cnt GT 0) THEN _EPSGamma[idx] = 1D-5
     ENDIF ELSE $
        _EPSGamma = Replicate(-1, N_LINE)
     ;; Peak
     IF (EPSPeak NE -1) THEN BEGIN
        _EPSPeak = (EPSPeak - Abs(1.D - (peak / initGauss.Peak))) > 0.D
        idx = Where(_EPSPeak LT 1D-5, cnt)
        IF (cnt GT 0) THEN _EPSPeak[idx] = 1D-5
     ENDIF ELSE $
        _EPSPeak = Replicate(-1, N_LINE)
  ENDIF $
  ELSE BEGIN
     _EPSWave0 = Replicate(EPSWave0, N_LINE)
     _EPSGamma = Replicate(EPSGamma, N_LINE)
     _EPSPeak = Replicate(EPSPeak, N_LINE)
  ENDELSE

  ;; Create MPFit parameter info structure.
  pi0 = { ParName:'', Limited:[1,1], Limits:[0.D,0.D], Value:1.D, Fixed:0 }
  pi = Replicate(pi0, 3*N_LINE)
  FOR i=0,N_LINE-1 DO BEGIN
     ;; Wave0
     pi[i].ParName = 'Wave0[' + name[i] + ']'
     pi[i].Value = wave0[i]
     IF (_EPSWave0[i] EQ -1) THEN BEGIN
        pi[i].Limited[1] = 0
     ENDIF $
     ELSE BEGIN
        pi[i].Limits = wave0[i] * $
                       [(1.D - _EPSWave0[i]) > 0.D, 1.D + _EPSWave0[i]]
     ENDELSE
     pi[i].Fixed = Keyword_Set(fixWave0)
     ;; Gamma
     pi[N_LINE+i].ParName = 'Gamma[' + name[i] + ']'
     pi[N_LINE+i].Value = gamma[i]
     IF (_EPSGamma[i] EQ -1) THEN BEGIN
        pi[N_LINE+i].Limited[1] = 0
     ENDIF $
     ELSE BEGIN
        pi[N_LINE+i].Limits = gamma[i] * $
                              [(1.D - _EPSGamma[i]) > 0.D, 1.D + _EPSGamma[i]]
     ENDELSE
     pi[N_LINE+i].Fixed = Keyword_Set(fixGamma)
     ;; Peak
     pi[2*N_LINE+i].ParName = 'Peak[' + name[i] + ']'
     pi[2*N_LINE+i].Value = peak[i]
     IF (_EPSPeak[i] EQ -1) THEN BEGIN
        pi[2*N_LINE+i].Limited[1] = 0
     ENDIF $
     ELSE BEGIN
        thisEPSPeak = _EPSPeak[i] > 1D-5
        minLimit = peak[i] * ((1.D - thisEPSPeak) > 0.D)
        maxLimit = (peak[i] * (1.D + thisEPSPeak)) > limit[i]
        pi[2*N_LINE+i].Limits = [minLimit, maxLimit]
     ENDELSE
     pi[2*N_LINE+i].Fixed = Keyword_Set(fixPeak)
  ENDFOR

  ;; Perform the fit with MPFitFun.
  IF (N_Elements(fTol) EQ 0) THEN fTol = 1D-5
  IF (N_Elements(maxIter) EQ 0) THEN maxIter = 200
  quiet = (~Keyword_Set(verbose))
  params = MPFitFun('Jam_GaussianFlux', waveFit, fluxFit, ParInfo=pi, $
                    Status=status, ErrMsg=errMsg, Weights=weightsFit, $
                    FTol=fTol, MaxIter=maxIter, PError=pError, $
                    BestNorm=chiSqr, DOF=DOF, Covar=covar, Quiet=quiet)

  ;; Check for fit errors.
  IF (status LE 0) THEN BEGIN
     Print, 'MPFitFun: ' + errMsg
     Message, Jam_Message('MPFitFun', $
                          StrCompress('Status = ' + String(status)))
  ENDIF

  ;; Create output parameter value and error structures.
  wave0 = params[0:N_LINE-1]
  gamma = params[N_LINE:2*N_LINE-1]
  peak = params[2*N_LINE:3*N_LINE-1]
  eWave0 = pError[0:N_LINE-1]
  eGamma = pError[N_LINE:2*N_LINE-1]
  ePeak = pError[2*N_LINE:3*N_LINE-1]
  gauss = { Name:name, Wave0:wave0, Gamma:gamma, Peak:peak }
  eGauss = { Name:name, Wave0:eWave0, Gamma:eGamma, Peak:ePeak }

  ;; Calculate flux and flux error.
  IF (Arg_Present(_eFlux)) THEN BEGIN
     _flux = Jam_GaussianFlux(wave, gauss, EGauss=eGauss, Covar=covar, $
                              EFlux=_eFlux)
  ENDIF ELSE $
     IF (Arg_Present(_flux)) THEN _flux = Jam_GaussianFlux(wave, gauss)

  ;; Calculate power and power error.
  IF (Arg_Present(ePower)) THEN BEGIN
     power = Jam_GaussianPower(gauss, EGauss=eGauss, Covar=covar, $
                               EPower=ePower)
  ENDIF ELSE $
     IF (Arg_Present(power)) THEN power = Jam_GaussianPower(gauss)

  RETURN, gauss

END ;; -----------------------------------------------------------------------
