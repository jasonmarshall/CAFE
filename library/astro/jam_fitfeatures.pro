;+ ===========================================================================
; NAME:
;
;       Jam_FitFeatures
;
; PURPOSE:
;
;       This function performs a least-squares fit to mid-IR (~5-35 microns) 
;       emission lines and PAH features in a continuum subtracted spectrum.
;
; CATEGORY:
;
;       Astrophysics
;
; CALLING SEQUENCE:
;
;       Result = Jam_FitFeatures(Wave, Flux[, Sigma])
;
; INPUTS:
;
;       Wave -
;
;       Flux -
;
;       Sigma -
;
; KEYWORD PARAMETERS:
;
;       Redshift -
;
;       ...
;
; AUTHOR:
;
;       Jason A. Marshall
;       Department of Astronomy, Cornell University, Ithaca, NY 14853
;       jam258@cornell.edu
;
; MODIFICATION HISTORY:
;
;       Written by: Jason A. Marshall, Nov 2006.
;-
;; ***************************************************************************
; Copyright (C) 2006, 2007, 2008, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ***************************************************************************

FUNCTION Jam_FitFeatures, wave, flux, sigma, $
                          SigDat=sigDat,$
                          Weights=weights, $
                          ExtPAH=extPAH, $
                          EExtPAH=eExtPAH, $
                          Redshift=redshift, $
                          F0_PAH=f0_PAH, $
                          MinWave=minWave, $
                          MaxWave=maxWave, $
                          Wave0PAH0=wave0PAH0, $
                          NoPAH3=noPAH3, $
                          FixWave0_PAH=fixWave0_PAH, $
                          FixGamma_PAH=fixGamma_PAH, $
                          FixPeak_PAH=fixPeak_PAH, $
                          FixWave0_LIN=fixWave0_LIN, $
                          FixGamma_LIN=fixGamma_LIN, $
                          FixPeak_LIN=fixPeak_LIN, $
                          EPSWave0_PAH=EPSWave0_PAH, $
                          EPSGamma_PAH=EPSGamma_PAH, $
                          EPSPeak_PAH=EPSPeak_PAH, $
                          EPSWave0_LIN=EPSWave0_LIN, $
                          EPSGamma_LIN=EPSGamma_LIN, $
                          EPSPeak_LIN=EPSPeak_LIN, $
                          Initialize=initialize, $
                          FTol=fTol, $
                          MaxIter=maxIter, $
                          Covar=covar, $
                          ChiSqr=chiSqr, $
                          DOF=DOF, $
                          Flux=_flux, $
                          EFlux=_eFlux, $
                          fLIN=fLIN, $
                          eFLIN=eFLIN, $
                          fPAH=fPAH, $
                          eFPAH=eFPAH, $
                          Power=power, $
                          EPower=ePower, $
                          PowLIN=powLIN, $
                          EPowLIN=ePowLIN, $
                          PowPAH=powPAH, $   ;; ...PAH are intrinsic values...
                          EPowPAH=ePowPAH, $
                          Pow0PAH=pow0PAH, $ ;; ...0PAH are apparent values...
                          EPow0PAH=ePow0PAH, $
                          PowComPAH=powComPAH, $
                          EPowComPAH=ePowComPAH, $
                          Pow0ComPAH=pow0ComPAH, $
                          EPow0ComPAH=ePow0ComPAH, $
                          EParams=eParams, $
                          Gauss=gauss, $
                          EGauss=eGauss, $
                          Drude=drude, $
                          EDrude=eDrude, $
                          RatioPAH=ratioPAH, $
                          ERatioPAH=eRatioPAH, $
                          Ratio0PAH=ratio0PAH, $
                          ERatio0PAH=eRatio0PAH, $
                          Verbose=verbose

  Compile_Opt IDL2
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() LT 2) THEN BEGIN
     Message, Jam_Message('Syntax', $
                          Jam_Syntax('Jam_FitFeatures', /Is_Function))
  ENDIF

  ;; Define default sigma and weights.
  N_WAVE = N_Elements(wave)
  IF (N_Elements(sigma) EQ 0) THEN sigma = Replicate(1.D, N_WAVE)
  IF (N_Elements(weights) EQ 0) THEN weights = 1.D / sigma^2

  ;; Set default mininum and maximum fit wavelengths.
  waveEPS = 0.005D
  IF (N_Elements(minWave) EQ 0) THEN minWave = (1.D + waveEPS) * Min(wave, /NaN)
  IF (N_Elements(maxWave) EQ 0) THEN maxWave = (1.D - waveEPS) * Max(wave, /NaN)

  ;; Set default PAH extinction.
  IF (N_Elements(extPAH) EQ 0) THEN extPAH = 1.D

  ;; Create index of wavelengths to fit.
  idxFit = Where((wave GE minWave) AND (wave LE maxWave), cntFit)
  waveFit = wave[idxFit]
  fluxFit = flux[idxFit]
  sigmaFit = sigma[idxFit]
  weightsFit = weights[idxFit]
  sigmaDat = (N_Elements(sigDat) EQ 0) ? $
             Replicate(0.D, cntFit) : sigDat[idxFit]

  ;; Create atomic line profiles.
  path = FilePath('lines.atomic.txt', Root_Dir=!Jam.Path.Tables)
  data = Jam_ReadData(path)
  idx_ATO = Where((data.Mask EQ 0) AND $
                  (data.Wave0 GT (0.98*minWave)) AND $
                  (data.Wave0 LT (1.02*maxWave)), cnt_ATO)
  IF (cnt_ATO GT 0) THEN BEGIN
     wave0 = data.Wave0[idx_ATO]
     name = data.Name[idx_ATO]
     profile = Replicate('G', cnt_ATO)
  ENDIF

  ;; Create molecular line profiles.
  path = FilePath('lines.molecular.txt', Root_Dir=!Jam.Path.Tables)
  data = Jam_ReadData(path)
  idx_MOL = Where((data.Mask EQ 0) AND $
                  (data.Wave0 GT (0.98*minWave)) AND $
                  (data.Wave0 LT (1.02*maxWave)), cnt_MOL)
  IF (cnt_MOL GT 0) THEN BEGIN
     wave0_MOL = data.Wave0[idx_MOL]
     name_MOL = data.Name[idx_MOL]
     prof_MOL = Replicate('G', cnt_MOL)
     wave0 = (N_Elements(wave0) EQ 0) ? wave0_MOL : [wave0, wave0_MOL]
     name = (N_Elements(name) EQ 0) ? name_MOL : [name, name_MOL]
     profile = (N_Elements(profile) EQ 0) ? prof_MOL : [profile, prof_MOL]
  ENDIF

  ;; Calculate widths and peaks.
  thisFlux = fluxFit
  IF ((cnt_ATO + cnt_MOL) GT 0) THEN BEGIN

     ;; Estimate gamma for each line.
     gamma = Jam_GammaIRS(wave0, redshift)

     ;; Iteratively estimate peaks for each line.
     FWHM = gamma * wave0
     N_SIGMA = 2.5D
     scale = N_SIGMA / (2.D * Sqrt(2.D * ALog(2.D)))
     waveMin = wave0 - scale * FWHM
     waveMax = wave0 + scale * FWHM
     N_WAVE0 = N_Elements(wave0)
     peak = DblArr(N_WAVE0)
     N_REPEAT = 5
     FOR i=0,N_REPEAT-1 DO BEGIN
        fluxMin = Interpol(thisFlux, waveFit, waveMin)
        fluxMax = Interpol(thisFlux, waveFit, waveMax)
;;         cont = DblArr(N_WAVE0)
;;         FOR j=0,N_WAVE0-1 DO cont[j] = Min([fluxMin[j], fluxMax[j]])
        cont = 0.5D * (fluxMin + fluxMax)
        thisPeak = Interpol(thisFlux, waveFit, wave0) - cont
        peak += thisPeak
        thisGauss = { Wave0:wave0, Gamma:gamma, Peak:thisPeak }
        thisFlux -= Jam_GaussianFlux(waveFit, thisGauss)
     ENDFOR
     peak = peak > 0.D
     complex = Replicate(-1, N_Elements(peak))
     ;; Subtract line fluxes from input flux for calculating PAH peaks.
     gauss = { Wave0:wave0, Gamma:gamma, Peak:peak }
     thisFlux -= Jam_GaussianFlux(waveFit, gauss)

  ENDIF

  ;; Create PAH Drude profiles.
  void = Jam_PAHDrude(DrudeSB=drude)
  wave0PAH = drude.Wave0
  gammaPAH = drude.Gamma
  _peakPAH = drude.Peak
  namePAH = Replicate('PAH', N_Elements(drude.Wave0))
  profPAH = Replicate('D', N_Elements(drude.Wave0))

  ;; Scale PAH profiles to feature.
  idxPAH6 = (Where((drude.Wave0 GE 6.1D) AND (drude.Wave0 LE 6.3D)))[0]
  wave0PAH6 = (drude.Wave0)[idxPAH6]
;;   gammaPAH6 = (drude.Gamma)[idxPAH6]
  IF (N_Elements(wave0PAH0) EQ 0) THEN wave0PAH0 = wave0PAH6
  idxGamma = (Where((drude.Wave0 GE (0.98*wave0PAH0)) AND $
                    (drude.Wave0 LE (1.02*wave0PAH0))))[0]
  gammaPAH0 = (drude.Gamma)[idxGamma]
  FWHM = gammaPAH0 * wave0PAH0
  N_SIGMA = 2.5
  scale = N_SIGMA / (2.D * Sqrt(2.D * ALog(2.D)))
  waveMin = wave0PAH0 - scale * FWHM
  waveMax = wave0PAH0 + scale * FWHM
  thisFlux /= extPAH
  peakPAH0 = 0.D
  N_REPEAT = 10
  FOR i=0,N_REPEAT-1 DO BEGIN
     fluxMin = Interpol(thisFlux, waveFit, waveMin)
     fluxMax = Interpol(thisFlux, waveFit, waveMax)
     cont = 0.5D * (fluxMin + fluxMax)
     thisPeak0 = Interpol(thisFlux, waveFit, wave0PAH0) - cont
     peakPAH0 += thisPeak0
     thisDrude = { Wave0:wave0PAH0, Gamma:gammaPAH0, Peak:thisPeak0 }
     thisFlux -= Jam_DrudeFlux(waveFit, thisDrude)
  ENDFOR
  peakPAH0 = peakPAH0 > 0.D
  wavePAH = Jam_Dist(500, 4.D, 20.D)
  fluxPAH = Jam_DrudeFlux(wavePAH, drude)
  fluxPAH0 = Interpol(fluxPAH, wavePAH, wave0PAH0)
  peakPAH = (peakPAH0 / fluxPAH0) * _peakPAH
  drude.Peak = peakPAH

  ;; Scale PAHs including limiting value.
  peakPAH0 = peakPAH0 > Interpol(Abs(sigmaDat), waveFit, wave0PAH0)
  IF (N_Elements(f0_PAH) NE 0) THEN peakPAH0 *= f0_PAH
  peakPAH = (peakPAH0 / fluxPAH0) * _peakPAH
  drude.Peak = peakPAH

  ;; Add PAH features to line list.
  name = (N_Elements(name) EQ 0) ? [namePAH] : [name, namePAH]
  profile = (N_Elements(profile) EQ 0) ? [profPAH] : [profile, profPAH]
  complex = (N_Elements(complex) EQ 0) ? $
            [drude.Complex] : [complex, drude.Complex]
  wave0 = (N_Elements(wave0) EQ 0) ? [wave0PAH] : [wave0, wave0PAH]
  gamma = (N_Elements(gamma) EQ 0) ? [gammaPAH] : [gamma, gammaPAH]
  peak = (N_Elements(peak) EQ 0) ? [peakPAH] : [peak, peakPAH]

  ;; Calculate limiting peaks.
  limit = ((Min(sigmaFit, /NaN) EQ Max(sigmaFit, /NaN)) AND $
           (sigmaFit[0] EQ 1.D)) ? $
          0.D : Interpol(Abs(sigmaFit), waveFit, wave0)

  ;; Sort lines by wavelength.
  N_FEAT = N_Elements(wave0)
  IF (wave0[0] EQ -1.D) THEN N_FEAT = 0
  IF (N_FEAT GT 0) THEN BEGIN
     idx = Sort(wave0)
     name = name[idx]
     wave0 = wave0[idx]
     gamma = gamma[idx]
     peak = peak[idx]
     limit = limit[idx]
     profile = profile[idx]
     complex = complex[idx]
  ENDIF

  ;; Create parameter structure.
  idxG = Where(profile EQ 'G', cntG)
  idxD = Where(profile EQ 'D', cntD)
  params = { Name:name, Wave0:wave0, Gamma:gamma, Peak:peak, $
             Profile:profile, Complex:complex }
  IF (cntG GT 0) THEN BEGIN
     gauss = { Name:name[idxG], Wave0:wave0[idxG], Gamma:gamma[idxG], $
               Peak:peak[idxG] }
  ENDIF
  IF (cntD GT 0) THEN BEGIN
     drude = { Wave0:wave0[idxD], Gamma:gamma[idxD], Peak:peak[idxD], $
               Complex:complex[idxD] }
  ENDIF

  ;; Return if initializing parameters.
  IF (Keyword_Set(initialize)) THEN BEGIN
     RETURN, (N_FEAT GT 0) ? params : $
             { Name:[''], Wave0:[-1.D], Gamma:[-1.D], Peak:[-1.D] }
  ENDIF

  ;; Make sure line list isn't empty.
  IF (N_FEAT EQ 0) THEN Message, Jam_Message('', 'No lines have been defined!')

  ;; Define default parameter variation tolerances.
  IF (N_Elements(EPSWave0_PAH) EQ 0) THEN EPSWave0_PAH = 2.5D-3
  IF (N_Elements(EPSGamma_PAH) EQ 0) THEN EPSGamma_PAH = 0.15D
  IF (N_Elements(EPSPeak_PAH) EQ 0) THEN EPSPeak_PAH = -1.D
  IF (N_Elements(EPSWave0_LIN) EQ 0) THEN EPSWave0_LIN = 2.5D-3
  IF (N_Elements(EPSGamma_LIN) EQ 0) THEN EPSGamma_LIN = 0.15D
  IF (N_Elements(EPSPeak_LIN) EQ 0) THEN EPSPeak_LIN = -1.D

  ;; Create MPFit parameter info structure.
  pi0 = { ParName:'', Limited:[1,1], Limits:[0.D,0.D], Value:1.D, Fixed:0 }
  pi = Replicate(pi0, 3*N_FEAT)
  FOR i=0,N_FEAT-1 DO BEGIN
     IF ((params.Profile)[i] EQ 'D') THEN BEGIN
        ;; Wave0
        pi[i].ParName = 'Wave0[' + name[i] + ']'
        pi[i].Value = wave0[i]
        IF (EPSWave0_PAH EQ -1.D) THEN BEGIN
;;        IF ((EPSWave0_PAH EQ -1.D) OR (i EQ 0)) THEN BEGIN
           pi[i].Limited[1] = 0
        ENDIF $
        ELSE BEGIN
           pi[i].Limits = wave0[i] * [(1.D - EPSWave0_PAH) > 0.D, $
                                      1.D + EPSWave0_PAH]
        ENDELSE
        pi[i].Fixed = Keyword_Set(fixWave0_PAH)
        ;; Gamma
        pi[N_FEAT+i].ParName = 'Gamma[' + name[i] + ']'
        pi[N_FEAT+i].Value = gamma[i]
        IF (EPSGamma_PAH EQ -1.D) THEN BEGIN
           pi[N_FEAT+i].Limited[1] = 0
        ENDIF $
        ELSE BEGIN
           pi[N_FEAT+i].Limits = gamma[i] * $
                                 [(1.D - EPSGamma_PAH) > $
                                  0.D, 1.D + EPSGamma_PAH]
        ENDELSE
        pi[N_FEAT+i].Fixed = Keyword_Set(fixGamma_PAH)
        ;; Peak
        pi[2*N_FEAT+i].ParName = 'Peak[' + name[i] + ']'
        pi[2*N_FEAT+i].Value = peak[i]
        IF (EPSPeak_PAH EQ -1.D) THEN BEGIN
           pi[2*N_FEAT+i].Limited[1] = 0
        ENDIF $
        ELSE BEGIN
           thisEPSPeak = EPSPeak_PAH > 1D-5
           minLimit = 0.D;;peak[i] * ((1.D - thisEPSPeak) > 0.D)
           IF ((minLimit GT 0.D) AND (minLimit LT 1D-5)) THEN minLimit = 1D-5
           maxLimit = (peak[i] * (1.D + thisEPSPeak)) > limit[i]
           pi[2*N_FEAT+i].Limits = [minLimit, maxLimit]
        ENDELSE
        pi[2*N_FEAT+i].Fixed = Keyword_Set(fixPeak_PAH)
        IF ((Keyword_Set(noPAH3)) AND (i EQ 0)) THEN BEGIN
           pi[2*N_FEAT+i].Value = 0.D
           pi[2*N_FEAT+i].Fixed = 1
        ENDIF 
     ENDIF $
     ELSE BEGIN ;; ...LINES...
        ;; Wave0
        pi[i].ParName = 'Wave0[' + name[i] + ']'
        pi[i].Value = wave0[i]
        IF (EPSWave0_LIN EQ -1.D) THEN BEGIN
           pi[i].Limited[1] = 0
        ENDIF $
        ELSE BEGIN
           pi[i].Limits = wave0[i] * [(1.D - EPSWave0_LIN) > 0.D, $
                                      1.D + EPSWave0_LIN]
        ENDELSE
        pi[i].Fixed = Keyword_Set(fixWave0_LIN)
        ;; Gamma
        pi[N_FEAT+i].ParName = 'Gamma[' + name[i] + ']'
        pi[N_FEAT+i].Value = gamma[i]
        IF (EPSGamma_LIN EQ -1.D) THEN BEGIN
           pi[N_FEAT+i].Limited[1] = 0
        ENDIF $
        ELSE BEGIN
           pi[N_FEAT+i].Limits = gamma[i] * $
                                 [(1.D - EPSGamma_LIN) > $
                                  0.D, 1.D + EPSGamma_LIN]
        ENDELSE
        pi[N_FEAT+i].Fixed = Keyword_Set(fixGamma_LIN)
        ;; Peak
        pi[2*N_FEAT+i].ParName = 'Peak[' + name[i] + ']'
        pi[2*N_FEAT+i].Value = peak[i]
        IF (EPSPeak_LIN EQ -1.D) THEN BEGIN
           pi[2*N_FEAT+i].Limited[1] = 0
        ENDIF $
        ELSE BEGIN
           thisEPSPeak = EPSPeak_LIN > 1D-5
           minLimit = 0.D
           maxLimit = (peak[i] * (1.D + thisEPSPeak)) > limit[i]
           pi[2*N_FEAT+i].Limits = [minLimit, maxLimit]
        ENDELSE
        pi[2*N_FEAT+i].Fixed = Keyword_Set(fixPeak_LIN)
     ENDELSE
  ENDFOR

  ;; Fix parameters for PAHs outside the fitted wavelength range.
  idx = Where(((wave0 LT (0.98*minWave)) OR (wave0 GT (1.02*maxWave))) AND $
              (profile EQ 'D'), cnt)
  IF (cnt GT 0) THEN BEGIN
     pi[idx].Fixed = 1
     pi[N_FEAT+idx].Fixed = 1
     pi[2*N_FEAT+idx].Fixed = 1
     pi[2*N_FEAT+idx].Value = 0.D
     limit[idx] = 0.D ;; ...ensure that exterior features stay small...
  ENDIF

  ;; Ensure that all values are within limits.
  FOR i=0,(3*N_FEAT)-1 DO BEGIN
     IF ((pi[i].Limited)[0]) THEN pi[i].Value = pi[i].Value > (pi[i].Limits)[0]
     IF ((pi[i].Limited)[1]) THEN pi[i].Value = pi[i].Value < (pi[i].Limits)[1]
     ;; IF ((pi[i].Limits)[1] GT (pi[i].Limits)[0]) THEN $
;;        pi[i].Value = pi[i].Value > (pi[i].Limits)[0] < (pi[i].Limits)[1]
  ENDFOR

  ;; Perform the fit with MPFitFun.
  IF (N_Elements(fTol) EQ 0) THEN fTol = 1D-5
  IF (N_Elements(maxIter) EQ 0) THEN maxIter = 200
  quiet = (~Keyword_Set(verbose))
  functArgs = { Profile:profile, ExtDrude:extPAH }
  params = MPFitFun('Jam_FeatureFlux', waveFit, fluxFit, ParInfo=pi, $
                    FunctArgs=functArgs, Status=status, ErrMsg=errMsg, $
                    Weights=weightsFit, FTol=fTol, MaxIter=maxIter, $
                    PError=pError, BestNorm=chiSqr, DOF=DOF, Covar=covar, $
                    Quiet=quiet)

  ;; Check for fit errors.
  IF (status LE 0) THEN BEGIN
     Print, 'MPFitFun: ' + errMsg
     Message, Jam_Message('MPFitFun', $
                          StrCompress('Status = ' + String(status)))
  ENDIF

  ;; Create output parameter value and error structures.
  wave0 = params[0:N_FEAT-1]
  gamma = params[N_FEAT:2*N_FEAT-1]
  peak = params[2*N_FEAT:3*N_FEAT-1]
  eWave0 = pError[0:N_FEAT-1]
  eGamma = pError[N_FEAT:2*N_FEAT-1]
  ePeak = pError[2*N_FEAT:3*N_FEAT-1]
  params = { Name:name, Wave0:wave0, Gamma:gamma, Peak:peak, $
             Complex:complex, Profile:profile }
  eParams = { Name:name, Wave0:eWave0, Gamma:eGamma, Peak:ePeak, $
              Complex:complex, Profile:profile }
  powParams = params
  ePowParams = eParams
  powParams.Peak = peak > limit
  ePowParams.Peak = ePeak > limit
  IF (cntG GT 0) THEN BEGIN
     gauss = { Name:name[idxG], Wave0:wave0[idxG], Gamma:gamma[idxG], $
               Peak:peak[idxG] }
     eGauss = { Name:name[idxG], Wave0:eWave0[idxG], Gamma:eGamma[idxG], $
                Peak:ePeak[idxG] }
  ENDIF
  IF (cntD GT 0) THEN BEGIN
     drude = { Wave0:wave0[idxD], Gamma:gamma[idxD], Peak:peak[idxD], $
               Complex:complex[idxD] }
     eDrude = { Wave0:eWave0[idxD], Gamma:eGamma[idxD], Peak:ePeak[idxD], $
                Complex:complex[idxD] }
  ENDIF

  ;; Calculate flux and flux error.
  fLIN = Jam_FeatureFlux(wave, params, EParams=eParams, Covar=covar, $
                         EFlux=eFLIN, Features=idxG)
  fPAH0 = Jam_FeatureFlux(wave, params, EParams=eParams, Covar=covar, $
                          EFlux=eFPAH0, Features=idxD)
  fPAH = extPAH * fPAH0
  IF (N_Elements(eExtPAH) EQ 0) THEN eExtPAH = 0.D
  eFPAH = Sqrt((eExtPAH * fPAH0)^2 + (extPAH * eFPAH0)^2)
  _flux = fLIN + fPAH
  _eFlux = Sqrt(eFLIN^2 + eFPAH^2)

  ;; Calculate power and power error.
  IF ((Arg_Present(power)) OR $
      (Arg_Present(powLIN)) OR $
      (Arg_Present(powPAH))) THEN BEGIN
     power = Jam_FeaturePower(powParams, EParams=ePowParams, Covar=covar, $
                              EPower=ePower)
  ENDIF
  IF (Arg_Present(powLIN)) THEN BEGIN
     powLIN = power[idxG]
     ePowLIN = ePower[idxG]
  ENDIF
  IF (Arg_Present(powPAH)) THEN BEGIN
     powPAH = power[idxD]
     ePowPAH = ePower[idxD]
  ENDIF
  nDrude = N_Elements(drude.Wave0)
  logWave = ALog(wave)
  IF (Arg_Present(pow0PAH)) THEN BEGIN
     pow0PAH = DblArr(nDrude)
     ePow0PAH = DblArr(nDrude)
     FOR i=0,nDrude-1 DO BEGIN
        f0 = Jam_FeatureFlux(wave, powParams, EParams=ePowParams, Covar=covar, $
                             EFlux=eF0, Features=idxD[i])
        f0_Tot = extPAH * f0
        ef0_Tot = Sqrt((extPAH * eF0)^2 + (f0 * eExtPAH)^2)
        pow0PAH[i] = TSum(logWave, wave * Jam_Jy2um(wave, f0_Tot))
        ePow0PAH[i] = TSum(logWave, wave * Jam_Jy2um(wave, ef0_Tot))
     ENDFOR
  ENDIF

  ;; Calculate PAH complex power and error.
  IF ((Arg_Present(powComPAH)) OR (Arg_Present(ratioPAH)) OR $
      (Arg_Present(eRatioPAH))) THEN BEGIN
     idx6 = Where(powParams.Complex EQ 3)
     idx7 = Where(powParams.Complex EQ 5)
     idx11 = Where(powParams.Complex EQ 9)
     idx12 = Where(powParams.Complex EQ 11)
     idx17 = Where(powParams.Complex EQ 16)
     powComPAH6 = Jam_FeaturePower(powParams, EParams=ePowParams, Covar=covar, $
                                   EPower=ePowComPAH6, Features=idx6)
     powComPAH7 = Jam_FeaturePower(powParams, EParams=ePowParams, Covar=covar, $
                                   EPower=ePowComPAH7, Features=idx7)
     powComPAH11 = Jam_FeaturePower(powParams, EParams=ePowParams,Covar=covar, $
                                    EPower=ePowComPAH11, Features=idx11)
     powComPAH12 = Jam_FeaturePower(powParams, EParams=ePowParams,Covar=covar, $
                                    EPower=ePowComPAH12, Features=idx12)
     powComPAH17 = Jam_FeaturePower(powParams, EParams=ePowParams,Covar=covar, $
                                    EPower=ePowComPAH17, Features=idx17)
     powComPAH = { PAH6:powComPAH6, PAH7:powComPAH7, PAH11:powComPAH11, $
                   PAH12:powComPAH12, PAH17:powComPAH17 }
     ePowComPAH = { PAH6:ePowComPAH6, PAH7:ePowComPAH7, PAH11:ePowComPAH11, $
                    PAH12:ePowComPAH12, PAH17:ePowComPAH17 }
  ENDIF
  IF ((Arg_Present(pow0ComPAH)) OR (Arg_Present(ratio0PAH)) OR $
      (Arg_Present(eRatio0PAH))) THEN BEGIN
     idx6 = Where(powParams.Complex EQ 3)
     idx7 = Where(powParams.Complex EQ 5)
     idx11 = Where(powParams.Complex EQ 9)
     idx12 = Where(powParams.Complex EQ 11)
     idx17 = Where(powParams.Complex EQ 16)
     f6 = Jam_FeatureFlux(wave, powParams, EParams=ePowParams, Covar=covar, $
                          EFlux=eF6, Features=idx6)
     f7 = Jam_FeatureFlux(wave, powParams, EParams=ePowParams, Covar=covar, $
                          EFlux=eF7, Features=idx7)
     f11 = Jam_FeatureFlux(wave, powParams, EParams=ePowParams, Covar=covar, $
                           EFlux=eF11, Features=idx11)
     f12 = Jam_FeatureFlux(wave, powParams, EParams=ePowParams, Covar=covar, $
                           EFlux=eF12, Features=idx12)
     f17 = Jam_FeatureFlux(wave, powParams, EParams=ePowParams, Covar=covar, $
                           EFlux=eF17, Features=idx17)
     f6_Tot = extPAH * f6
     f7_Tot = extPAH * f7
     f11_Tot = extPAH * f11
     f12_Tot = extPAH * f12
     f17_Tot = extPAH * f17
     ef6_Tot = Sqrt((extPAH * eF6)^2 + (f6 * eExtPAH)^2)
     ef7_Tot = Sqrt((extPAH * eF7)^2 + (f7 * eExtPAH)^2)
     ef11_Tot = Sqrt((extPAH * eF11)^2 + (f11 * eExtPAH)^2)
     ef12_Tot = Sqrt((extPAH * eF12)^2 + (f12 * eExtPAH)^2)
     ef17_Tot = Sqrt((extPAH * eF17)^2 + (f17 * eExtPAH)^2)
     pow0ComPAH6 = TSum(logWave, wave * Jam_Jy2um(wave, f6_Tot))
     pow0ComPAH7 = TSum(logWave, wave * Jam_Jy2um(wave, f7_Tot))
     pow0ComPAH11 = TSum(logWave, wave * Jam_Jy2um(wave, f11_Tot))
     pow0ComPAH12 = TSum(logWave, wave * Jam_Jy2um(wave, f12_Tot))
     pow0ComPAH17 = TSum(logWave, wave * Jam_Jy2um(wave, f17_Tot))
     ePow0ComPAH6 = TSum(logWave, wave * Jam_Jy2um(wave, ef6_Tot))
     ePow0ComPAH7 = TSum(logWave, wave * Jam_Jy2um(wave, ef7_Tot))
     ePow0ComPAH11 = TSum(logWave, wave * Jam_Jy2um(wave, ef11_Tot))
     ePow0ComPAH12 = TSum(logWave, wave * Jam_Jy2um(wave, ef12_Tot))
     ePow0ComPAH17 = TSum(logWave, wave * Jam_Jy2um(wave, ef17_Tot))
     pow0ComPAH = { PAH6:pow0ComPAH6, PAH7:pow0ComPAH7, PAH11:pow0ComPAH11, $
                    PAH12:pow0ComPAH12, PAH17:pow0ComPAH17 }
     ePow0ComPAH = { PAH6:ePow0ComPAH6, PAH7:ePow0ComPAH7, PAH11:ePow0ComPAH11, $
                     PAH12:ePow0ComPAH12, PAH17:ePow0ComPAH17 }
  ENDIF

  ;; Calculate PAH ratios.
  IF (Arg_Present(ratioPAH)) THEN BEGIN
     ratioPAH6 = powComPAH6 / powComPAH7
     ratioPAH11 = powComPAH11 / powComPAH7
     ratioPAH12 = powComPAH12 / powComPAH7
     ratioPAH17 = powComPAH17 / powComPAH7
     ratioPAH = { PAH6:ratioPAH6, PAH11:ratioPAH11, PAH12:ratioPAH12, $
                  PAH17:ratioPAH17 }
  ENDIF
  IF (Arg_Present(eRatioPAH)) THEN BEGIN
     eRatioPAH6 = Sqrt((ePowComPAH6 / powComPAH7)^2 + $
                       (powComPAH6 / powComPAH7^2 * ePowComPAH7)^2)
     eRatioPAH11 = Sqrt((ePowComPAH11 / powComPAH7)^2 + $
                        (powComPAH11 / powComPAH7^2 * ePowComPAH7)^2)
     eRatioPAH12 = Sqrt((ePowComPAH12 / powComPAH7)^2 + $
                        (powComPAH12 / powComPAH7^2 * ePowComPAH7)^2)
     eRatioPAH17 = Sqrt((ePowComPAH17 / powComPAH7)^2 + $
                        (powComPAH17 / powComPAH7^2 * ePowComPAH7)^2)
     eRatioPAH = { PAH6:eRatioPAH6, PAH11:eRatioPAH11, PAH12:eRatioPAH12, $
                   PAH17:eRatioPAH17 }
  ENDIF
  IF (Arg_Present(ratio0PAH)) THEN BEGIN
     ratio0PAH6 = pow0ComPAH6 / pow0ComPAH7
     ratio0PAH11 = pow0ComPAH11 / pow0ComPAH7
     ratio0PAH12 = pow0ComPAH12 / pow0ComPAH7
     ratio0PAH17 = pow0ComPAH17 / pow0ComPAH7
     ratio0PAH = { PAH6:ratio0PAH6, PAH11:ratio0PAH11, PAH12:ratio0PAH12, $
                  PAH17:ratio0PAH17 }
  ENDIF
  IF (Arg_Present(eRatio0PAH)) THEN BEGIN
     eRatio0PAH6 = Sqrt((ePow0ComPAH6 / pow0ComPAH7)^2 + $
                        (pow0ComPAH6 / pow0ComPAH7^2 * ePow0ComPAH7)^2)
     eRatio0PAH11 = Sqrt((ePow0ComPAH11 / pow0ComPAH7)^2 + $
                         (pow0ComPAH11 / pow0ComPAH7^2 * ePow0ComPAH7)^2)
     eRatio0PAH12 = Sqrt((ePow0ComPAH12 / pow0ComPAH7)^2 + $
                         (pow0ComPAH12 / pow0ComPAH7^2 * ePow0ComPAH7)^2)
     eRatio0PAH17 = Sqrt((ePow0ComPAH17 / pow0ComPAH7)^2 + $
                         (pow0ComPAH17 / pow0ComPAH7^2 * ePow0ComPAH7)^2)
     eRatio0PAH = { PAH6:eRatio0PAH6, PAH11:eRatio0PAH11, PAH12:eRatio0PAH12, $
                    PAH17:eRatio0PAH17 }
  ENDIF

  RETURN, params

END ;; -----------------------------------------------------------------------
