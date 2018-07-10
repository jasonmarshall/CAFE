
FUNCTION Jam_FitGauss_Flux, wave, params, Gauss0=gauss0

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Calculate gaussian profile parameters.
  gauss = Jam_FitGauss_Gauss(params, gauss0)

  ;; Calculate line flux.
  flux = Jam_GaussianFlux(wave, gauss)

  RETURN, flux

END


PRO Jam_FitGauss_Plot, wave, flux, error, gauss, fGauss, eFGauss, YMax=yMax

  Compile_Opt IDL2, Hidden
  On_Error, 2
  
  ;; Get colors.
  colors = FSC_Color(/AllColors, ColorStructure=colorStruct)

  ;; Set colors.
  colors = { $
    BCK: colorStruct.Face, $
    AXS: colorStruct.Black, $
    DAT: colorStruct.Black, $
    FIT: colorStruct.Orange, $
    ERR: colorStruct.SlateGray, $
    FER: colorStruct.MediumGray, $
    ANN: colorStruct.SlateGray $
  }
  
  ;; Plot axes.
  xRange = MinMax(wave)
  yRange = MinMax(flux)
  IF (N_Elements(yMax) NE 0) THEN yRange[1] = yMax
  Plot, [1], [1], XStyle=1, XRange=xRange, YStyle=1, YRange=yRange, $
    XTitle=TeXtoIDL('Rest wavelength [\mum]'), YTitle='Flux density [Jy]', $
    Background=colors.BCK, Color=colors.AXS
  
  ;; Overplot gauss profile error.
  IF ((N_Elements(fGauss) NE 0) AND (N_Elements(eFGauss) NE 0)) THEN BEGIN
    xFill = [wave, Reverse(wave)]
    yFill = [fGauss + eFGauss, Reverse(fGauss - eFGauss)]
    PolyFill, xFill > xRange[0] < xRange[1], yFill > yRange[0] < yRange[1], $
      Color=colors.FER
  ENDIF
  
  ;; Overplot data error.
  xFill = [wave, Reverse(wave)]
  yFill = [flux + error, Reverse(flux - error)]
  PolyFill, xFill > xRange[0] < xRange[1], yFill > yRange[0] < yRange[1], $
    Color=colors.ERR
  
  ;; Overplot data.
  OPlot, wave, flux, Color=colors.DAT
  
  ;; Overplot fit.
  IF (N_Elements(fGauss) NE 0) THEN OPlot, wave, fGauss, Color=colors.FIT
  
  ;; Overplot gauss profiles.
  IF (N_Elements(gauss) NE 0) THEN BEGIN

    ;; Overplot line wavelengths.
    wave0 = gauss.Wave0
    FOR i=0,N_Elements(wave0)-1 DO $
      PlotS, [wave0[i], wave0[i]], MinMax(flux), Color=colors.ANN

    ;; Overplot individual profiles.
    FOR i=0,N_Elements(wave0)-1 DO BEGIN
       line = Jam_GaussianFlux(wave, gauss.A0[i], gauss.Wave0[i], $
                               gauss.Sigma[i])
      OPlot, wave, line, LineStyle=1, Color=colors.FIT
    ENDFOR

  ENDIF

  RETURN

END


FUNCTION Jam_FitGauss_Fit, wave, flux, gauss0, EPS, weights, PError=pError, $
  FixA0=fixA0, FixWave0=fixWave0, FixSigma=fixSigma, ChiSqr=chiSqr, DOF=DOF

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get number of lines.
  N_WAVE0 = N_Elements(gauss0.Wave0)
  
  ;; Set limits arrays.
  sigmaLimits = (EPS.Sigma EQ -1.D) ? $
                [0.D, 100.D] : [1.D - EPS.Sigma, 1.D + EPS.Sigma]
  A0Limits = [0.D, 1.2D]; [1.D - EPS.A0, 1.D + EPS.A0]
  
  ;; Create MPFit parameter info structure.
  pi = Replicate({ Limited:[1,1], Limits:[0.D,0.D], ParName:'', Value:0.D, $
    Fixed:0 }, 3 * N_WAVE0)
  FOR i=0,N_WAVE0-1 DO BEGIN
     pi[3*i].ParName = StrCompress('A0_' + String(i+1), /Remove_All)
     pi[3*i].Limits = A0Limits
     pi[3*i].Value = 1.D
     pi[3*i].Fixed = Keyword_Set(fixA0)
     pi[3*i + 1].ParName = StrCompress('Wave0_' + String(i+1), /Remove_All)
     pi[3*i + 1].Limits = [1.D - EPS.Wave0, 1.D + EPS.Wave0]
     pi[3*i + 1].Value = 1.D
     pi[3*i + 1].Fixed = Keyword_Set(fixWave0)
     pi[3*i + 2].ParName = StrCompress('Sigma_' + String(i+1), /Remove_All)
     pi[3*i + 2].Limits = sigmaLimits
     pi[3*i + 2].Value = 1.D
     pi[3*i + 2].Fixed = Keyword_Set(fixSigma)
  ENDFOR

  ;; Perform the fit with MPFitFun.
  functArgs = { Gauss0:gauss0 }
  params = MPFitFun('Jam_FitGauss_Flux', wave, flux, FunctArgs=functArgs, $
    ParInfo=pi, Status=status, Weights=weights, Quiet=1, FTol=1D-6, $
    MaxIter=50, PError=pError, BestNorm=chiSqr, DOF=DOF)

  ;; Check for fit errors.
  IF (status LE 0) THEN $
    Message, Jam_Message('MPFitFun', StrCompress('Status = ' + String(status)))

  ;; Return parameters.
  RETURN, params

END


FUNCTION Jam_FitGauss_Gauss, params, gauss0

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Return gaussian parameters structure.
  N_WAVE0 = N_Elements(gauss0.Wave0)
  A0 = DblArr(N_WAVE0, /NoZero)
  wave0 = DblArr(N_WAVE0, /NoZero)
  sigma = DblArr(N_WAVE0, /NoZero)
  FOR i=0,N_WAVE0-1 DO BEGIN
    A0[i] = params[3*i] * gauss0.A0[i]
    wave0[i] = params[3*i + 1] * gauss0.Wave0[i]
    sigma[i] = params[3*i + 2] * gauss0.Sigma[i]
  ENDFOR

  RETURN, { A0:A0, Wave0:wave0, Sigma:sigma }

END


FUNCTION Jam_FitGauss, _wave, _flux, _error, Wave0=wave0, Sigma=sigma, $
  EPS_A0=EPS_A0, EPS_Wave0=EPS_Wave0, EPS_Sigma=EPS_Sigma, FixA0=FixA0, $
  FixWave0=fixWave0, FixSigma=fixSigma, YMax=yMax, Weights=weights, $
  Quiet=quiet, Flux=flux, ErrFlux=errFlux, ErrGauss=errGauss, $
  ChiSqr=chiSqr, DOF=DOF

  Compile_Opt IDL2
  On_Error, 2
  
  ;; Define default errors and weights.
  N_WAVE = N_Elements(_wave)
  IF (N_Elements(_error) EQ 0) THEN _error = Replicate(1.D, N_WAVE)
  IF (N_Elements(weights) EQ 0) THEN weights = Replicate(1.D, N_WAVE)
  weightsTot = weights / _error^2
  
  ;; Get number of gaussian profile lines.
  N_WAVE0 = N_Elements(wave0)
  
  ;; Define default parameter variation tolerances.
  IF (N_Elements(sigma) EQ 0) THEN EPS_Sigma = -1.D
  IF (N_Elements(EPS_A0) EQ 0) THEN EPS_A0 = 0.05D
  IF (N_Elements(EPS_Wave0) EQ 0) THEN EPS_Wave0 = 0.005D
  IF (N_Elements(EPS_Sigma) EQ 0) THEN EPS_Sigma = 0.25D
  EPS = { A0:EPS_A0, Wave0:EPS_Wave0, Sigma:EPS_Sigma }
  
  ;; Perform fit to initial lines.
  GAMMA0 = 0.01D
  IF (N_WAVE0 GT 0) THEN BEGIN
    A0 = Interpol(_flux, _wave, wave0)
    IF (N_Elements(sigma) EQ 0) THEN $
      sigma = GAMMA0 * wave0 / (2.D * Sqrt(2.D * ALog(2.D)))
    gauss0 = { A0:A0, Wave0:wave0, Sigma:sigma }
    params = Jam_FitGauss_Fit(_wave, _flux, gauss0, EPS, weightsTot, $
      FixA0=fixA0, FixWave0=fixWave0, FixSigma=fixSigma, PError=pError, $
      ChiSqr=chiSqr, DOF=DOF)
    gauss = Jam_FitGauss_Gauss(params, gauss0)
    eGauss = Jam_FitGauss_Gauss(pError, gauss0)
    fGauss = Jam_GaussianFlux(_wave, gauss.A0, gauss.Wave0, gauss.Sigma)
    derivs = Jam_Deriv('Jam_FitGauss_Flux', _wave, params, Gauss0=gauss0, $
      PError=pError)
    eFGauss = Jam_EFunc(derivs, pError)
  ENDIF
  
  ;; Perform interactive fitting.
  IF (~Keyword_Set(quiet)) THEN BEGIN
  
    ;; Create window.
    screen = Get_Screen_Size()
    xSize = 800;;0.9D * screen[0]
    ySize = 0.6D * screen[1]
    Window, /Free, XSize=xSize, YSize=ySize, Title='Gaussian Profile Fit'
  
    ;; Plot spectra + fit.
    Jam_FitGauss_Plot, _wave, _flux, _error, gauss, fGauss, eFGauss, YMax=yMax
  
    ;; Prompt for more lines.
    again = 1
    WHILE (again) DO BEGIN

      ;; Prompt if a line should be added.
      test = ''
      Read, test, Prompt='Add line (y or n)?'
      IF (StrUpCase(test) EQ 'N') THEN BEGIN
        again = 0
        BREAK
      ENDIF ELSE $
        N_WAVE0++

      ;; Select line wavelength.
      Print, 'Left click on central wavelength...'
      Cursor, thisWave0, void, /Down, /Data

      ;; Fit with new line.
      thisA0 = Interpol(_flux, _wave, thisWave0)
      thisSigma = (N_Elements(sigma) NE 0) ? sigma : $
        GAMMA0 * thisWave0 / (2.D * Sqrt(2.D * ALog(2.D)))
      IF (N_Elements(gauss) NE 0) THEN BEGIN
        thisA0 = [gauss.A0, thisA0]
        thisWave0 = [gauss.Wave0, thisWave0]
        thisSigma = [gauss.Sigma, thisSigma]
      ENDIF $
      ELSE BEGIN
        thisA0 = [thisA0]
        thisWave0 = [thisWave0]
        thisSigma = [thisSigma]
      ENDELSE
      gauss0 = { A0:thisA0, Wave0:thisWave0, Sigma:thisSigma }
      params = Jam_FitGauss_Fit(_wave, _flux, gauss0, EPS, weightsTot, $
        FixA0=fixA0, FixWave0=fixWave0, FixSigma=fixSigma, PError=pError, $
        ChiSqr=thisChiSqr, DOF=thisDOF)
      derivs = Jam_Deriv('Jam_FitGauss_Flux', _wave, params, Gauss0=gauss0, $
        PError=pError)
      
      ;; Plot new fit.
      thisGauss = Jam_FitGauss_Gauss(params, gauss0)
      thisEGauss = Jam_FitGauss_Gauss(pError, gauss0)
      thisFGauss = Jam_GaussianFlux(_wave, thisGauss.A0, thisGauss.Wave0, $
        thisGauss.Sigma)
      thsiEFGauss = Jam_EFunc(derivs, pError)
      Jam_FitGauss_Plot, _wave, _flux, _error, thisGauss, thisFGauss, $
        thisEFGauss, YMax=yMax

      ;; Prompt if this line should be saved.
      Read, test, Prompt='Keep line (y or n)?'

      ;; Save or discard line.
      IF (StrUpCase(test) EQ 'Y') THEN BEGIN

        ;; Update variables.
        gauss = thisGauss
        eGauss = thisEGauss
        fGauss = thisFGauss
        eFGauss = thisEFGauss
        chiSqr = thisChiSqr
        DOF = thisDOF

        ;; Print line parameters.
        Print, ''
        FOR i=0,N_WAVE0-1 DO BEGIN
          thisA0 = gauss.A0[i]
          thisWave0 = gauss.Wave0[i]
          thisSigma = gauss.Sigma[i]
          thisFWHM = thisSigma * 2.D * Sqrt(2.D * ALog(2.D))
          Print, StrCompress('--- Line #' + String(i+1) + ' ---')
          Print, StrCompress(' * A0     = ' + String(thisA0) + ' Jy')
          Print, StrCompress(' * Wave0  = ' + String(thisWave0) + ' um')
          Print, StrCompress(' * Sigma  = ' + String(thisSigma))
          Print, StrCompress(' * FWHM   = ' + String(thisFWHM) + ' um')
        ENDFOR

      ENDIF ELSE $
        N_WAVE0--

      ;; Plot fit.
      Jam_FitGauss_Plot, _wave, _flux, _error, gauss, fGauss, eFGauss, $
        YMax=yMax

    ENDWHILE
    
  ENDIF
    
  ;; Sort gaussian profile parameters by wavelength.
  IF (N_Elements(gauss) NE 0) THEN BEGIN
    index = Sort(gauss.Wave0)
    gauss.A0 = gauss.A0[index]
    gauss.Wave0 = gauss.Wave0[index]
    gauss.Sigma = gauss.Sigma[index]
    errGauss = Create_Struct('A0', Abs(eGauss.A0[index]), $
      'Wave0', Abs(eGauss.Wave0[index]), 'Sigma', Abs(eGauss.Sigma[index]))
  ENDIF
  
  ;; Calculate flux and eFlux.
  IF (N_Elements(gauss) EQ 0) THEN gauss = { Wave0:[-1.D] }
  flux = (N_Elements(fGauss) EQ 0) ? 0.D * _wave : fGauss
  errFlux = (N_Elements(eFGauss) EQ 0) ? 0.D * _wave : eFGauss
  
  RETURN, gauss

END
