;+ ===========================================================================
; NAME:
;       EFluxIRS
;
; PURPOSE:
;       This function calculates the 1-sigma flux uncertainty vector of an
;       IRS spectrum from two spectra at different nod positions.
;
; CALLING SEQUENCE:
;	Result = EFluxIRS(Wave, FluxNod1, FluxNod2)
;  -or-
;       Result = EFluxIRS(SED=SED)
;
; INPUT PARAMETERS:
;	Wave - Wavelength vector.
;	FluxNod1 - Flux vector from 1st nod position.
;	FluxNod2 - Flux vector from 2nd nod position.
;
; KEYWORD PARAMETERS:
;	SED - SED structure containing wavelength and flux vectors.
;	VERBOSE - Set this keyword to produce a plot showing the results.
;	          Solid lines indicate 1- and 3-sigma error contours and dots
;                 indicate the error at each wavelength as calculated from the
;                 difference of the nods.
;	WINDOW - Set this keyword equal to the scalar integer window width
;                over which to calculate the standard deviation for each point.
;
; OUTPUTS:
; 	This function returns a vector containing the standard deviation of the
;       flux for each input wavelength.
;
; EXAMPLE:
; 	To calculate the flux errors enter:
;	   IDL> wave = ...
;	   IDL> fluxNod1 = ...
;	   IDL> fluxNod2 = ...
;	   IDL> sigma = EFluxIRS(wave, fluxNod1, fluxNod2)
;
; REQUIREMENTS:
;       AstroLib routines MEDSMOOTH and RESISTANT_MEAN.
;
; AUTHOR:
;	Jason A. Marshall, Astronomy Dept., Cornell Univ., Ithaca, NY 14853
;	<jam258@cornell.edu>
;
; MODIFICATION HISTORY:
;	Written by: Jason A. Marshall, 01 Jun 2005.
;-
;; ...........................................................................
; Copyright (C) 2005, 2006, 2007, Jason A. Marshall
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or un-
; modified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;; ...........................................................................

FUNCTION EFluxIRS_StdDev, vector, window, Debug=debug

  ;; This function calculates the standard deviation at each position by
  ;; calculating sigma over a window of points and assigning the value to the
  ;; central point.

  Compile_Opt IDL2, Hidden
  IF (~Keyword_Set(debug)) THEN On_Error, 2

  ;; Create window index vector.
  idxWindow = LIndGen(window) - (window / 2)

  ;; Calculate number of reduced-window end-points.
  nReduced = ((window - 1) / 2) - 1

  ;; Find standard deviation for full-window points.
  nVector = N_Elements(vector)
  stdDev = DblArr(nVector)
  sigmaCut = 3.D
  FOR i=nReduced+1,(nVector-nReduced-1)-1 DO BEGIN
     Resistant_Mean, vector[idxWindow+i], sigmaCut, thisMean, thisSigma, nRej
     stdDev[i] = thisSigma * Sqrt((window - nRej) - 1.D)
  ENDFOR

  ;; Find standard deviation for points nearing ends.
  IF (nReduced GT 0) THEN BEGIN
     FOR i=1,nReduced DO BEGIN
        length = 2 * i + 1
        Resistant_Mean, vector[0:length-1], sigmaCut, thisMean, thisSigma, nRej
        stdDev[i] = thisSigma * Sqrt((length - nRej) - 1.D)
        Resistant_Mean, $
           vector[(nVector-1)-length+1:(nVector-1)], $
           sigmaCut, thisMean, thisSigma, nRej
        stdDev[(nVector-1)-i] = thisSigma * Sqrt((length - nRej) - 1.D)
     ENDFOR
  ENDIF

  ;; Estimate standard deviation at end points (use the same method as
  ;; AstroLib function MedSmooth).
  value = 3.D * vector[0] - 2.D * vector[1]
  Resistant_Mean, [value, vector[0], vector[1]], $
                  sigmaCut, thisMean, thisSigma, nRej
  stdDev[0] = thisSigma * Sqrt((3.D - nRej) - 1.D)
  value = 3.D * vector[(nVector-1)] - 2.D * vector[(nVector-1)-1]
  Resistant_Mean, [value, vector[(nVector-1)], vector[(nVector-1)-1]], $
                  sigmaCut, thisMean, thisSigma, nRej
  stdDev[(nVector-1)] = thisSigma * Sqrt((3.D - nRej) - 1.D)

  RETURN, stdDev

END ;; -----------------------------------------------------------------------

FUNCTION EFluxIRS, wave, fluxNod1, fluxNod2, SED=SED, CErrIdx=cErrIdx, $
                   dFlux=dFlux, ErrIdx=errIdx, N_Sigma=N_Sigma, $
                   Verbose=verbose, Window=window, Debug=debug

  Compile_Opt IDL2
  Catch, theError
  IF (theError NE 0) THEN BEGIN
     Catch, /Cancel
     ok = Error_Message(/Error)
     !Except = except
     RETURN, ''
  ENDIF

  ;; Setup debugging.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  ;; Check syntax.
  syntax = 'sigma = EFluxIRS(wave, fluxNod1, fluxNod2) -or- EFluxIRS(SED=SED)'
  IF (~((N_Params() EQ 0) AND (Keyword_Set(SED))) $
      AND (N_Params() NE 3)) THEN Message, syntax

  ;; Get flux vectors from SED structure.
  IF (N_Elements(SED) NE 0) THEN BEGIN
     wave = SED.Wave
     fluxNod1 = SED.Nod1Flux.Jy
     fluxNod2 = SED.Nod2Flux.Jy
     IF ((Total(fluxNod1) EQ 0.D) OR (Total(fluxNod2) EQ 0.D)) THEN BEGIN
        dF = SED.Flux.Jy - Smooth(SED.Flux.Jy, 5)
        fluxNod1 = SED.Flux.Jy + dF
        fluxNod2 = SED.Flux.Jy - dF
     ENDIF
  ENDIF

  ;; Ensure that window is odd, >= 3, and <= length of wave vector.
  IF (N_Elements(window) EQ 0) THEN window = 21
  IF ((window MOD 2) EQ 0) THEN window++
  window = window > 3 < N_Elements(wave)
  IF ((window MOD 2) EQ 0) THEN window--

  ;; Calculate the 'uncertainty' at each wavelength position from the
  ;; difference of the two nods. The two factors of Sqrt(2) are included to:
  ;;   [1] Correct for the introduction of noise from the subtraction, and
  ;;   [2] Account for the reduction in noise in the final flux vector
  ;;       from averaging the two nods.
  dFlux = (fluxNod1 - fluxNod2) / 2.D

  ;; Look for NaN's, and see if they only exist in one of the nod positions.
  ;; If so, calculate a new uncertainty estimate by subtracting the good nod
  ;; position from a smoothed version of itself. This should only leave
  ;; isolated (and hopefully rare) NaN's.
  ;; NOTE: Divide by only one factor of Sqrt[2] since the final flux vector
  ;;       is not the average of two nods.
  idxNaN = Where(Finite(dFlux, /NaN), cntNaN, NComplement=cnt)
  IF (cnt EQ 0) THEN RETURN, Replicate(!Values.D_NaN, cntNaN)
  nod1 = (fluxNod1 - MedSmooth(fluxNod1, window));; / Sqrt(2.D)
  nod2 = (fluxNod2 - MedSmooth(fluxNod2, window));; / Sqrt(2.D)
  FOR i=0,cntNaN-1 DO BEGIN
     IF (Finite(nod1[idxNaN[i]])) THEN dFlux[idxNaN[i]] = nod1[idxNaN[i]]
     IF (Finite(nod2[idxNaN[i]])) THEN dFlux[idxNaN[i]] = nod2[idxNaN[i]]
  ENDFOR

  ;; Extract non-NaN data.
  idxNaN = Where(Finite(dFlux, /NaN), cntNaN, Complement=idx, NComplement=cnt)
  dFlux = dFlux[idx]

  ;; Calculate the standard deviation of the points in a window around each
  ;; wavelength position, and assign that value to the central wavelength.
  sigma0 = EFluxIRS_StdDev(dFlux, window, Debug=debug)

  ;; Smooth sigma0 vector.
  sigma0 = Smooth(sigma0, window, /NaN)

  ;; Scale errors so that 68% of points are within 1-sigma.
  FRAC_LT_1SIG = 0.68D
  MIN_DELTA = 1D-3
  MAX_ITER = 20
  delta = 0.D
  nIter = 1
  scale = 1.D
  cntTot = N_Elements(sigma0)
  REPEAT BEGIN
     scale *= (1.D + delta)
     idx1sig = Where(Sqrt(dFlux^2) LE (1.D * scale * sigma0), cnt1sig)
     fracLT1sig = Double(cnt1sig) / cntTot
     delta = (FRAC_LT_1SIG - fracLT1sig) / FRAC_LT_1SIG
     nIter++
  ENDREP UNTIL ((Abs(delta) LT MIN_DELTA) OR (nIter GE MAX_ITER))
  sigma0 *= scale

  ;; Interpolate to estimate errors for isolated NaN data.
  sigma = DblArr(N_Elements(wave))
  sigma[idx] = sigma0
  IF (cntNaN GT 0) THEN $
     sigma[idxNaN] = Exp(Interpol(ALog(sigma0), ALog(wave[idx]), $
                                  ALog(wave[idxNaN])))

  ;; Find indices of 'non-statistical' errors for which the error from the
  ;; difference of the nods is greater than N_SIGMA sigma.
  IF (N_Elements(N_SIGMA) EQ 0) THEN N_SIGMA = 2.5D
  errIdx = Where((Sqrt(dFlux^2) GT (N_SIGMA * sigma)), Complement=cErrIdx)

  ;; Print the results and plot the errors.
  IF (Keyword_Set(verbose)) THEN BEGIN
     Print, StrCompress('Scale = ' + String(scale, Format='(F5.2)'))
     idx1sig = Where(Sqrt(dFlux^2) LT (1.D * sigma), cnt1sig)
     idx2sig = Where(Sqrt(dFlux^2) LT (2.D * sigma), cnt2sig)
     idx3sig = Where(Sqrt(dFlux^2) LT (3.D * sigma), cnt3sig)
     Print, StrCompress("N(<1sig)/NTot = " + String(Double(cnt1sig) / cntTot))
     Print, StrCompress("N(<2sig)/NTot = " + String(Double(cnt2sig) / cntTot))
     Print, StrCompress("N(<3sig)/NTot = " + String(Double(cnt3sig) / cntTot))
     Window, /Free, Title=('EFlux - ' + SED.Object)
     minWave = Min(wave, Max=maxWave)
     minSigma = Min(sigma)
     maxSigma = Max(3.D * sigma)
     Plot, wave, sigma, /XLog, /YLog, $
           XStyle=1, XRange=[minWave, maxWave], $
           YStyle=1, YRange=[minSigma, maxSigma], $
           XTitle='Rest wavelength [microns]', $
           YTitle='Flux error [Jy]'
     OPlot, wave, 3.D * sigma
     OPlot, wave, Sqrt(dFlux^2), PSym=3
  ENDIF

  idx = Where(sigma GT 0.D, Complement=cIdx, NComplement=cCnt)
  IF (cCnt GT 0) THEN sigma[cIdx] = Min(sigma[idx])

  !Except = except
  RETURN, sigma

END ;; -----------------------------------------------------------------------
