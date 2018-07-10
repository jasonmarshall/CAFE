
PRO Jam_FitDrude_Plot, wave, flux, sigma, drude, fluxDrude, errFluxDrude, $
  YMax=yMax

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
    XTitle=TeXtoIDL('\lambda_{rest} [\mum]'), YTitle=TeXtoIDL('f_\nu [Jy]'), $
    Background=colors.BCK, Color=colors.AXS

  ;; Overplot drude profile error.
  IF ((N_Elements(fluxDrude) NE 0) AND $
      (N_Elements(errFluxDrude) NE 0)) THEN BEGIN
    xFill = [wave, Reverse(wave)]
    yFill = [fluxDrude + errFluxDrude, Reverse(fluxDrude - errFluxDrude)]
    PolyFill, xFill > xRange[0] < xRange[1], yFill > yRange[0] < yRange[1], $
      Color=colors.FER
  ENDIF

  ;; Overplot data errors.
  xFill = [wave, Reverse(wave)]
  yFill = [flux + sigma, Reverse(flux - sigma)]
  PolyFill, xFill > xRange[0] < xRange[1], yFill > yRange[0] < yRange[1], $
    Color=colors.ERR

  ;; Overplot data.
  OPlot, wave, flux, Color=colors.DAT

  ;; Overplot fit.
  IF (N_Elements(fluxDrude) NE 0) THEN $
    OPlot, wave, fluxDrude, Color=colors.FIT

  ;; Overplot drude profiles.
  IF (N_Elements(drude) NE 0) THEN BEGIN

    ;; Overplot line wavelengths.
    wave0 = drude.Wave0
    FOR i=0,N_Elements(wave0)-1 DO $
      PlotS, [wave0[i], wave0[i]], MinMax(flux), Color=colors.ANN

    ;; Overplot individual profiles.
    FOR i=0,N_Elements(wave0)-1 DO BEGIN
      thisDrude = Jam_DrudeFlux(wave, drude, Index=i)
      OPlot, wave, thisDrude, LineStyle=2, Color=colors.FIT
    ENDFOR

  ENDIF

  RETURN

END


FUNCTION Jam_FitDrude_Fit, wave, flux, thisDrude, EPS_Wave0, weights, $
  FixWave0=fixWave0, PError=pError, ChiSqr=chiSqr, DOF=DOF

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get number of lines.
  N_WAVE0 = N_Elements(thisDrude.Wave0)

  ;; Create MPFit parameter info structure.
  pi = Replicate({ Limited:[1,0], Limits:[0.D,0.D], ParName:'', Value:0.D, $
    Fixed:0 }, 3 * N_WAVE0)
  FOR i=0,N_WAVE0-1 DO BEGIN
     pi[3*i].ParName = StrCompress('Wave0_' + String(i+1), /Remove_All)
     pi[3*i].Limited = 1
     pi[3*i].Limits = (thisDrude.Wave0)[i] * [1.D - EPS_Wave0, 1.D + EPS_Wave0]
     pi[3*i].Value = (thisDrude.Wave0)[i]
     pi[3*i].Fixed = Keyword_Set(fixWave0)
     pi[3*i + 1].ParName = StrCompress('Gamma_' + String(i+1), /Remove_All)
     pi[3*i + 1].Value = (thisDrude.Gamma)[i]
     pi[3*i + 2].ParName = StrCompress('Peak_' + String(i+1), /Remove_All)
     pi[3*i + 2].Value = (thisDrude.Peak)[i]
  ENDFOR

  ;; Perform the fit with MPFitFun.
  params = MPFitFun('Jam_FitDrude_Flux', wave, flux, ParInfo=pi, $
    Status=status, Weights=weights, Quiet=1, FTol=1D-6, MaxIter=100, $
    PError=pError, BestNorm=chiSqr, DOF=DOF)

  ;; Check for fit errors.
  IF (status LE 0) THEN $
    Message, Jam_Message('MPFitFun', StrCompress('Status = ' + String(status)))

  ;; Return parameters.
  RETURN, params

END


FUNCTION Jam_FitDrude_Flux, wave, params, _Extra=extra

  Compile_Opt IDL2, Hidden
  On_Error, 2

  drude = Jam_FitDrude_Drude(params)
  flux = Jam_DrudeFlux(wave, drude, /NoConst)

  RETURN, flux

END


FUNCTION Jam_FitDrude_Drude, params

  Compile_Opt IDL2, Hidden
  On_Error, 2

  N_WAVE0 = Fix(N_Elements(params) / 3.D)
  wave0 = DblArr(N_WAVE0, /NoZero)
  gamma = DblArr(N_WAVE0, /NoZero)
  peak = DblArr(N_WAVE0, /NoZero)
  FOR i=0,N_WAVE0-1 DO BEGIN
    wave0[i] = params[3*i]
    gamma[i] = params[3*i + 1]
     peak[i] = params[3*i + 2]
  ENDFOR

  RETURN, { Wave0:wave0, Gamma:gamma, Peak:peak }

END


FUNCTION Jam_FitDrude, _wave, _flux, _sigma, YMax=yMax, Gamma0=gamma0, $
  EPS_Wave0=EPS_Wave0, FixWave0=fixWave0, Weights=weights, ChiSqr=chiSqr, $
  DOF=DOF, ErrDrude=errDrude, Flux=flux, ErrFlux=errFlux

  Compile_Opt IDL2
  On_Error, 2

  ;; Define default sigma and weights.
  N_WAVE = N_Elements(_wave)
  IF (N_Elements(_sigma) EQ 0) THEN _sigma = Sqrt(_flux / Min(_flux))
  IF (N_Elements(weights) EQ 0) THEN weights = Replicate(1.D, N_WAVE)
  weightsTot = weights / _sigma^2

  ;; Define default parameter variation tolerance.
  IF (N_Elements(EPS_Wave0) EQ 0) THEN EPS_Wave0 = 0.01D

  ;; Define default drude profile width.
  IF (N_Elements(gamma0) EQ 0) THEN gamma0 = 0.01D

  ;; Create window.
  screen = Get_Screen_Size()
  xSize = 0.9D * screen[0]
  ySize = 0.6D * screen[1]
  Window, /Free, XSize=xSize, YSize=ySize, Title='Drude Profile Fit'

  ;; Plot spectra + fit.
  Jam_FitDrude_Plot, _wave, _flux, _sigma, YMax=yMax

  ;; Prompt for more lines.
  again = 1
  test = ''
  N_WAVE0 = 0
  WHILE (again) DO BEGIN

    N_WAVE0++

    ;; Select line wavelength.
    Print, 'Left click on central wavelength...'
    Cursor, thisWave0, void, /Down, /Data

    ;; Fit with new line.
    thisGamma = gamma0
    thisPeak = (N_Elements(fluxDrude) EQ 0) ? $
      Interpol(_flux, _wave, thisWave0) > 0.D : $
      Interpol(_flux - fluxDrude, _wave, thisWave0) > 0.D
    IF (N_Elements(drude) NE 0) THEN BEGIN
      thisWave0 = [drude.Wave0, thisWave0]
      thisGamma = [drude.Gamma, thisGamma]
      thisPeak = [drude.Peak, thisPeak]
    ENDIF $
    ELSE BEGIN
      thisWave0 = [thisWave0]
      thisGamma = [thisGamma]
      thisPeak = [thisPeak]
    ENDELSE
    thisDrude = { Wave0:thisWave0, Gamma:thisGamma, Peak:thisPeak }
    params = Jam_FitDrude_Fit(_wave, _flux, thisDrude, EPS_Wave0, weightsTot, $
      FixWave0=fixWave0, PError=pError, ChiSqr=thisChiSqr, DOF=thisDOF)
    derivs = Jam_Deriv('Jam_FitDrude_Flux', _wave, params, PError=pError)

    ;; Plot new fit.
    thisDrude = Jam_FitDrude_Drude(params)
    thisErrDrude = Jam_FitDrude_Drude(pError)
    thisFluxDrude = Jam_DrudeFlux(_wave, thisDrude)
    thisErrFluxDrude = Jam_EFunc(derivs, pError)
    Jam_FitDrude_Plot, _wave, _flux, _sigma, thisDrude, thisFluxDrude, $
      thisErrFluxDrude, YMax=yMax

    ;; Prompt if this line should be saved.
    Read, test, Prompt='Keep line (y or n)?'

    ;; Save or discard line.
    IF (StrUpCase(test) EQ 'Y') THEN BEGIN

      ;; Update variables.
      drude = thisDrude
      errDrude = thisErrDrude
      fluxDrude = thisFluxDrude
      errFluxDrude = thisErrFluxDrude
      chiSqr = thisChiSqr
      DOF = thisDOF

      ;; Print line parameters.
      Print, ''
      idx = Sort(drude.Wave0)
      FOR i=0,N_WAVE0-1 DO BEGIN
        thisWave0 = drude.Wave0[idx[i]]
        thisGamma = drude.Gamma[idx[i]]
        thisPeak = drude.Peak[idx[i]]
        thisFWHM = thisWave0 * thisGamma
        Print, StrCompress('--- Line #' + String(i+1) + ' ---')
        Print, StrCompress(' * Wave  = ' + String(thisWave0) + ' um')
        Print, StrCompress(' * FWHM  = ' + String(thisFWHM) + ' um')
      ENDFOR

    ENDIF ELSE $
      N_WAVE0--

    ;; Plot fit.
    Jam_FitDrude_Plot, _wave, _flux, _sigma, drude, fluxDrude, errFluxDrude, $
      YMax=yMax

    ;; Prompt if a line should be added.
    Read, test, Prompt='Add line (y or n)?'
    IF (StrUpCase(test) EQ 'N') THEN again = 0

  ENDWHILE

  ;; Sort drude profile parameters by wavelength.
  IF (N_Elements(drude) NE 0) THEN BEGIN
    idx = Sort(drude.Wave0)
    drude.Wave0 = drude.Wave0[idx]
    drude.Gamma = drude.Gamma[idx]
    drude.Peak = drude.Peak[idx]
    errDrude = Create_Struct('Wave0', errDrude.Wave0[idx], $
      'Gamma', errDrude.Gamma[idx], 'Peak', errDrude.Peak[idx])
  ENDIF

  ;; Calculate flux and eFlux.
  IF (N_Elements(drude) EQ 0) THEN drude = { Wave0:[-1.D] }
  flux = (N_Elements(fluxDrude) EQ 0) ? 0.D * _wave : fluxDrude
  errFlux = (N_Elements(errFluxDrude) EQ 0) ? 0.D * _wave : errFluxDrude

  RETURN, drude

END
