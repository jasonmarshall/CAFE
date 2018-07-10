;+ ===================================================================
; NAME:
; CAFE
;
; PURPOSE:
; This is the Continuum and Feature Extractor (aka, CAFE) for fitting the
; SEDs of astronomical sources (in particular those with dusty mid-IR spectra).
; See Marshall et al. (2007) for a description of the fitting procedure, and
; see the 'cafe.txt' input parameters file for a description of the settings
; and parameters that may be adjusted for a given run of CAFE.
;
; CATEGORY:
; Astrophysics.
;
; CALLING SEQUENCE:
; CAFE
;
; INPUTS:
; object - The name of the source to fit. The path to the input data file is
;          taken from the !CAFE.In system variable. This system variable
;          should be created/set in the user's 'startup.pro' file before
;          running CAFE. This is done by adding this line to 'startup.pro':
;
;          DefSysV, '!CAFE', { In:<insert input directory>, Out:<insert output dir> }
;
;          The !CAFE.In directory is the input data file path; !CAFE.Out is
;          is the default location to save files which are output from CAFE.
;          Data is read using the "SED" procedure using the line:
;
;          SED, object, Path_SED=inPath
;
;          where inPath is an optional keyword parameter that overides !CAFE.In.
;          See the procedure "sed.pro" for more details about the structure of
;          the input IDL binary (.xdr) data file, and see the procedure
;          "sed_import.pro" for information about how to create the correctly
;          formatted data file from a wavelength, flux density, and flux density
;          uncertainty array. After using this routine to create input files for
;          sources, be sure to put them in "inPath" or !CAFE.In so that they will
;          be found by this routine.
;
; KEYWORDS:
; SED - An output keyword to obtain a structure of both the input data as well as
;       the output of CAFE. The returned structure is exactly as you would obtain
;       from "SED, object, SED=SED", but with an additional element called "CAFE".
;       This element is itself a structure containing all the output from the fit.
;       This CAFE structure is defined near the end of this "Cafe" procedure.
;
;   There are a number of additional optional input keyword parameters defined
;   to allow for the over-riding of default behaviour. There are too many to list,
;   so I will leave their perusal as an exercise to any interested parties.
;
; NOTE:
; As can be seen from a quick glance, this procedure was built over time by
; appending piece upon piece upon piece...and thus, at this point, it is far
; from optimally efficient (or well-designed). It would greatly benefit from
; a complete reorganization and modularization.
;
; EXAMPLE:
; To analyze an SED enter:
;   IDL> CAFE
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
; 2018 July 08 - JAM - Cleaned-up for "public release."
;-
;; ....................................................................
; Copyright (C) 2005, 2018 Jason A. Marshall
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or un-
; modified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;; ....................................................................

;;+ ===================================================================
; NAME:
; CAFE_PARAMS
;
; PURPOSE:
; This routine creates a structure of the fit parameters.
;
; INPUTS:
; params - array of current floating-point model values
;   tied - array of integers giving index of the model parameter that each
;          parameter should be tied (or fixed) to.
;
; DESCRIPTION:
; The names of the elements in the returned structure are the parameter names,
; the values are the current parameter values (possibly after being "tied" to
; one of the other parameters). If fit parameters are added or removed from the
; model (and in 'cafe.txt'), changes must be made here as well to reflect this.
; This is, obviously, poor software design...a modern implementation of this
; would want to construct this structure from the parameters listed in cafe.txt
; directly (and not require them to be re-listed and synchronized here).
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

FUNCTION Cafe_Params, params, tied

  Compile_Opt IDL2, Hidden
  On_Error, 2

  N_PAR = N_Elements(params)
  IF (N_Elements(tied) EQ 0) THEN tied = LIndGen(N_PAR)

  names = [ 'CIR_FLX', $
            'CIR_TMP', $

            'CLD_FLX', $
            'CLD_TMP', $

            'COO_FLX', $
            'COO_TMP', $
            'COO_TAU', $
            'COO_MIX', $
            'COO_COV', $

            'WRM_FLX', $
            'WRM_TMP', $
            'WRM_TAU', $
            'WRM_MIX', $
            'WRM_COV', $

            'HOT_FLX', $
            'HOT_TMP', $
            'HOT_TAU', $
            'HOT_MIX', $
            'HOT_COV', $

            'STR_FLX', $
            'STR_TAU', $
            'STR_MIX', $
            'STR_COV', $

            'STB_FLX', $
            'STB_TAU', $
            'STB_MIX', $
            'STB_COV', $
            'STB_100', $
            'STB_010', $

            'DSK_FLX', $
            'DSK_TAU', $
            'DSK_COV', $

            'PAH_FLX', $
            'PAH_TAU', $
            'PAH_MIX', $
            'PAH_COV', $

            'TAU_ICE', $
            'TAU_HAC' ]

  struct = Create_Struct(names[0], params[tied[0]])
  FOR i=1,N_PAR-1 DO struct = Create_Struct(struct, names[i], params[tied[i]])

  RETURN, struct

END

;;+ ===================================================================
; NAME:
; CAFE_WEIGHTS
;
; PURPOSE:
; This routine calculates the vector of weights (one per wavelength point)
; for the fit.
;
; INPUTS:
; wave - Wavelength array in microns
; index - Structure whose elements contain indices into the wavelength
;         vector for the optical ("OPT"), NIR, MIR, IRS, FIR, and radio
;         ("RAD") regions of the spectrum.
;         OPT: wavelength < 1 micron
;         NIR: 1 micron < wavelength < 5 micron (or start of IRS region)
;         MIR: 5 micron < wavelength < 40 micron
;         FIR: 40 micron < wavelength < 1300 micron
;         RAD: wavelength > 1300 micron
; input - Structure of data from cafe.txt
;
; DESCRIPTION:
; The weighting implemented in this routine is where the "art" of fitting
; unevenly sampled data enters the picture. Data points in the IRS region are
; sampled at much higher resolution than the photometric points. As such, if
; we weigh each data point evenly across the entire wavelength range, the IRS
; data will dominate the fit (simply because there are many more data points).
; In this routine, we remedy this by creating a "weights" vector which weighs
; different data points differently--less weight is given to highly-sampled
; spectral data and more to sparsely sampled photometry. We do this by
; dividing the wavelength space up into (logarithmically spaced) bins, assigning
; an even weight to each *bin*, and then distributing the bin's weight over
; the data points falling within that bin. If no data points fall within a bin,
; the bin's weight is distributed to the data points falling within the bins
; on either side of the empty bin. The returned weight array is normalized.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
; 2005 Sep 13 - JAM - Modified to weigh photometry by the wavelength range covered.
;;-

FUNCTION Cafe_Weights, wave, index, input

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Create total photometry + IRS weights vector.
  weights = Replicate(0.D, N_Elements(wave))

  ;; Get indices.
  idxOPT = index.OPT
  idxNIR = index.NIR
  idxMIR = index.MIR
  idxIRS = index.IRS
  idxFIR = index.FIR
  idxRAD = index.RAD
  N_OPT = (idxOPT[0] EQ -1L) ? 0 : N_Elements(idxOPT)
  N_NIR = (idxNIR[0] EQ -1L) ? 0 : N_Elements(idxNIR)
  N_MIR = (idxMIR[0] EQ -1L) ? 0 : N_Elements(idxMIR)
  N_IRS = (idxIRS[0] EQ -1L) ? 0 : N_Elements(idxIRS)
  N_FIR = (idxFIR[0] EQ -1L) ? 0 : N_Elements(idxFIR)
  N_RAD = (idxRAD[0] EQ -1L) ? 0 : N_Elements(idxRAD)

  ;; Calculate number of IRS samples per logarithmic interval.
  minIRS = (N_IRS GT 0) ? Min(wave[idxIRS]) : 5.D
  maxIRS = (N_IRS GT 0) ? Max(wave[idxIRS]) : 40.D
  minWave = Min(wave, Max=maxWave)
  N_Per_Dex = (N_IRS EQ 0) ? 1.D : (N_IRS / ALog10(maxIRS / minIRS))

  ;; Calculate min and max wavelength for each bin.
  N_BIN = input.WeightBins ;; Number of logarithmic bins to divide wavelength space into.
  waveMinBin = 10^(ALog10(minWave) + DIndGen(N_BIN) * $
                   ALog10(maxWave / minWave) / Double(N_BIN))
  waveMaxBin = 10^(ALog10(minWave) + (1.D + DIndGen(N_BIN)) * $
                   ALog10(maxWave / minWave) / Double(N_BIN))

  ;; Calculate weight per bin.
  weightPerBin = N_Per_Dex * ALog10(waveMaxBin / waveMinBin)

  ;; Create vector containing number of data points per bin.
  N_Per_Bin = DblArr(N_BIN)
  EPS = 1D-5
  FOR i=0,N_BIN-1 DO BEGIN
     idxBin = Where((wave GE (1.D - EPS) * waveMinBin[i]) AND $
                    (wave LE (1.D + EPS) * waveMaxBin[i]), cntBin)
     N_Per_Bin[i] = cntBin
  ENDFOR
  idxGT0 = Where(N_Per_Bin GT 0)

  ;; Loop over each bin.
  FOR i=0,N_BIN-1 DO BEGIN

     ;; Find indices of points in bin.
     idxBin = Where((wave GE ((1.D - EPS) * waveMinBin)[i]) AND $
                    (wave LE ((1.D + EPS) * waveMaxBin)[i]), cntBin)

     ;;; If the bin has data in it, assign weights to points in bin.
     IF (cntBin GT 0) THEN BEGIN
        weights[idxBin] = weightPerBin[i] / N_Per_Bin[i]
     ENDIF $
     ;;; Otherwise, assign bin weight to surrounding bins with points.
     ELSE BEGIN

        ;; Find nearest bins with data points above and below current bin.
        idx = Where(idxGT0 GT i)
        binAbove = Min(idxGT0[idx])
        idx = Where(idxGT0 LT i)
        binBelow = Max(idxGT0[idx])

        ;; Calculate relative distribution of weight to be assigned to
        ;; each bin -- based upon how far away the bins are.
        distAbove = Double(binAbove - i)
        distBelow = Double(i - binBelow)
        weightAbove = 1.D - (distAbove / (distAbove + distBelow))
        weightBelow = 1.D - (distBelow / (distAbove + distBelow))

        ;; Assign weight to points in surrounding bins.
        idxAbove = Where((wave GE ((1.D - EPS) * waveMinBin)[binAbove]) AND $
                         (wave LE ((1.D + EPS) * waveMaxBin)[binAbove]))
        idxBelow = Where((wave GE ((1.D - EPS) * waveMinBin)[binBelow]) AND $
                         (wave LE ((1.D + EPS) * waveMaxBin)[binBelow]))
        weights[idxAbove] = weightAbove * weightPerBin[i] / N_Per_Bin[binAbove]
        weights[idxBelow] = weightBelow * weightPerBin[i] / N_Per_Bin[binBelow]

     ENDELSE

  ENDFOR

  ;; Normalize weights.
  weights *= (Double(N_Elements(weights)) / Total(weights))

  RETURN, weights

END

;;+ ===================================================================
; NAME:
; CAFE_FLUX
;
; PURPOSE:
; This routine calculates the model flux.
;
; NOTE:
; See Marshall et al. (2007) for a detailed description of the model
; and parameters.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

FUNCTION Cafe_Flux, wave, model, Input=input, FilterDat=filterDat, $
						  Filters=filters, z=z, Wave0=wave0, Flux0=flux0, $
						  WaveMOD=waveMOD, K_ICE=K_ICE, K_HAC=K_HAC, $
						  K_DST=K_DST, K_EXT=K_EXT, E_HOT=E_HOT, E_WRM=E_WRM, $
						  E_COO=E_COO, E_CLD=E_CLD, E_CIR=E_CIR, Gauss=gauss, $
						  SourceSTR=sourceSTR, Source2Myr=source2Myr, $
						  Source10Myr=source10Myr, Source100Myr=source100Myr, $
						  SourceDSK=sourceDSK, E0=E0, Ext=ext, Com=com, ModCom=modCom, $
						  Drude=drude, Tau0=tau0, Debug=debug, fPAH0_Tot=fPAH0_Tot

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get model parameter structure.
  p = Cafe_Params(model)

  ;; Get variations on wavelength vector.
  logWave = ALog(wave)
  N_WAVE = N_Elements(wave)
  logWaveMOD = ALog(waveMOD)
  N_WAVEMOD = N_Elements(waveMOD)
  fixWave = ~Array_Equal(wave, waveMOD)

  ;; Calculate total normalized dust opacity.
  K_AbsTot = K_DST.Carb + K_DST.Sil.Amo.Tot
  K_ExtTot = K_EXT.Carb + K_EXT.Sil.Amo.Tot
  idxMAX = Where((waveMOD GT 8.5D) AND (waveMOD LT 11.D))
  K_AbsTot0 = Max(K_AbsTot[idxMAX])
  K_ExtTot0 = Max(K_ExtTot[idxMAX])
  K_AbsTot /= K_AbsTot0
  K_ExtTot /= K_ExtTot0
  K_Tot = K_AbsTot
  K_Tot0 = K_AbsTot0
  K_Tot_DSK = (StrUpCase(input.ExtOrAbs) EQ 'ABS') ? K_AbsTot : K_ExtTot

  ;; Additional opacity sources:
  tauICE = p.TAU_ICE * K_ICE + p.TAU_HAC * K_HAC

  ;; CIR component flux.
  IF (p.CIR_FLX GT 0.D) THEN BEGIN
     jCIR = Jam_GrainTotEmissivity(waveMOD, p.CIR_TMP, E_T=E_CIR)
     jCIR0 = Interpol(jCIR, logWaveMOD, ALog(wave0.CIR))
     IF (jCIR0 EQ 0.D) THEN jCIR0 = 1.D
     const = p.CIR_FLX * flux0.CIR / jCIR0[0]
     fCIR = (const * jCIR) > 0.D
     fCIR0 = fCIR
     fCIR_Tot = TSum(logWaveMOD, fCIR / waveMOD)
  ENDIF $
  ELSE BEGIN
     jCIR0 = -1.D
     fCIR = DblArr(N_WAVEMOD)
     fCIR0 = fCIR
     fCIR_Tot = 0.D
  ENDELSE
  fDST_Tot = fCIR_Tot

  ;; CLD component flux.
  IF (p.CLD_FLX GT 0.D) THEN BEGIN
     jCLD = Jam_GrainTotEmissivity(waveMOD, p.CLD_TMP, E_T=E_CLD)
     jCLD0 = Interpol(jCLD, logWaveMOD, ALog(wave0.CLD))
     IF (jCLD0 EQ 0.D) THEN jCLD0 = 1.D
     const = p.CLD_FLX * flux0.CLD / jCLD0[0]
     fCLD = (const * jCLD) > 0.D
     fCLD0 = fCLD
     fCLD_Tot = TSum(logWaveMOD, fCLD / waveMOD)
     fDST_Tot += fCLD_Tot
  ENDIF $
  ELSE BEGIN
     jCLD0 = -1.D
     fCLD = DblArr(N_WAVEMOD)
     fCLD0 = fCLD
     fCLD_Tot = 0.D
  ENDELSE

  ;; COO component flux.
  IF (p.COO_FLX GT 0.D) THEN BEGIN
     IF (p.COO_TAU GT 0.D) THEN BEGIN
        tau0COO = p.COO_TAU
        tauScrCOO = (1.D - p.COO_MIX) * (tau0COO * K_Tot + tauICE)
        tauMixCOO = p.COO_MIX * (tau0COO * K_Tot + tauICE)
        extScrCOO = Exp(-tauScrCOO)
        extMixCOO = Replicate(1.D, N_WAVEMOD)
        idx = Where(tauMixCOO GT 0.D, cnt)
        IF (cnt GT 0) THEN $
           extMixCOO[idx] = (1.D - Exp(-tauMixCOO[idx])) / tauMixCOO[idx]
        extCOO = (1.D - p.COO_COV) + p.COO_COV * extScrCOO * extMixCOO
     ENDIF $
     ELSE BEGIN
        tau0COO = 0.D
        extCOO = Replicate(1.D, N_WAVEMOD)
     ENDELSE
     jCOO_0 = Jam_GrainTotEmissivity(waveMOD, p.COO_TMP, E_T=E_COO)
     jCOO = extCOO * jCOO_0
     jCOO0 = Interpol(jCOO, logWaveMOD, ALog(wave0.COO))
     IF (jCOO0 EQ 0.D) THEN jCOO0 = 1.D
     const = p.COO_FLX * flux0.COO / jCOO0[0]
     fCOO = (const * jCOO) > 0.D
     fCOO0 = (const * jCOO_0) > 0.D
     fCOO_Tot = TSum(logWaveMOD, fCOO / waveMOD)
     fDST_Tot += fCOO_Tot
  ENDIF $
  ELSE BEGIN
     extCOO = Replicate(1.D, N_WAVEMOD)
     jCOO0 = -1.D
     fCOO = DblArr(N_WAVEMOD)
     fCOO0 = fCOO
     fCOO_Tot = 0.D
     tau0COO = -1.D
  ENDELSE

  ;; WRM component flux.
  IF (p.WRM_FLX GT 0.D) THEN BEGIN
     IF (p.WRM_TAU GT 0.D) THEN BEGIN
        tau0WRM = p.WRM_TAU
        tauScrWRM = (1.D - p.WRM_MIX) * (tau0WRM * K_Tot + tauICE)
        tauMixWRM = p.WRM_MIX * (tau0WRM * K_Tot + tauICE)
        extScrWRM = Exp(-tauScrWRM)
        extMixWRM = Replicate(1.D, N_WAVEMOD)
        idx = Where(tauMixWRM GT 0.D, cnt)
        IF (cnt GT 0) THEN $
           extMixWRM[idx] = (1.D - Exp(-tauMixWRM[idx])) / tauMixWRM[idx]
        extWRM = (1.D - p.WRM_COV) + p.WRM_COV * extScrWRM * extMixWRM
     ENDIF $
     ELSE BEGIN
        tau0WRM = 0.D
        extWRM = Replicate(1.D, N_WAVEMOD)
     ENDELSE
     jWRM_0 = Jam_GrainTotEmissivity(waveMOD, p.WRM_TMP, E_T=E_WRM)
     jWRM = extWRM * jWRM_0
     jWRM0 = Interpol(jWRM, logWaveMOD, ALog(wave0.WRM))
     IF (jWRM0 EQ 0.D) THEN jWRM0 = 1.D
     const = p.WRM_FLX * flux0.WRM / jWRM0[0]
     fWRM = (const * jWRM) > 0.D
     fWRM0 = (const * jWRM_0) > 0.D
     fWRM_Tot = TSum(logWaveMOD, fWRM / waveMOD)
     fDST_Tot += fWRM_Tot
  ENDIF $
  ELSE BEGIN
     extWRM = Replicate(1.D, N_WAVEMOD)
     jWRM0 = -1.D
     fWRM = DblArr(N_WAVEMOD)
     fWRM0 = fWRM
     fWRM_Tot = 0.D
     tau0WRM = -1.D
  ENDELSE

  ;; HOT component flux.
  IF (p.HOT_FLX GT 0.D) THEN BEGIN
     IF (p.HOT_TAU GT 0.D) THEN BEGIN
        tau0HOT = p.HOT_TAU
        tauScrHOT = (1.D - p.HOT_MIX) * (tau0HOT * K_Tot + tauICE)
        tauMixHOT = p.HOT_MIX * (tau0HOT * K_Tot + tauICE)
        extScrHOT = Exp(-tauScrHOT)
        extMixHOT = Replicate(1.D, N_WAVEMOD)
        idx = Where(tauMixHOT GT 0.D, cnt)
        IF (cnt GT 0) THEN $
           extMixHOT[idx] = (1.D - Exp(-tauMixHOT[idx])) / tauMixHOT[idx]
        extHOT = (1.D - p.HOT_COV) + p.HOT_COV * extScrHOT * extMixHOT
     ENDIF $
     ELSE BEGIN
        tau0HOT = 0.D
        extHOT = Replicate(1.D, N_WAVEMOD)
     ENDELSE
     jHOT_0 = Jam_GrainTotEmissivity(waveMOD, p.HOT_TMP, E_T=E_HOT)
     jHOT = extHOT * jHOT_0
     jHOT0 = Interpol(jHOT, logWaveMOD, ALog(wave0.HOT))
     IF (jHOT0 EQ 0.D) THEN jHOT0 = 1.D
     const = p.HOT_FLX * flux0.HOT / jHOT0[0]
     fHOT = (const * jHOT) > 0.D
     fHOT0 = (const * jHOT_0) > 0.D
     fHOT0_Tot = TSum(logWaveMOD, fHOT0 / waveMOD)
  ENDIF $
  ELSE BEGIN
     extHOT = Replicate(1.D, N_WAVEMOD)
     jHOT0 = -1.D
     fHOT = DblArr(N_WAVEMOD)
     fHOT0 = fHOT
     fHOT0_Tot = 0.D
     tau0HOT = -1.D
  ENDELSE

  ;; PAH component flux.
  IF (p.PAH_FLX GT 0.D) THEN BEGIN
     IF (p.PAH_TAU GT 0.D) THEN BEGIN
        tau0PAH = ((Jam_Check(input, Tag_Name='Tie_PAH_TAU')) && $
                   (input.Tie_PAH_TAU EQ 'WRM_TAU')) ? tau0WRM : p.PAH_TAU
        tau0PAH = ((Jam_Check(input, Tag_Name='Tie_PAH_TAU')) && $
                   (input.Tie_PAH_TAU EQ 'COO_TAU')) ? tau0COO : p.PAH_TAU
        tauScrPAH = (1.D - p.PAH_MIX) * (tau0PAH * K_Tot + tauICE)
        tauMixPAH = p.PAH_MIX * (tau0PAH * K_Tot + tauICE)
        extScrPAH = Exp(-tauScrPAH)
        extMixPAH = Replicate(1.D, N_WAVEMOD)
        idx = Where(tauMixPAH GT 0.D, cnt)
        IF (cnt GT 0) THEN $
           extMixPAH[idx] = (1.D - Exp(-tauMixPAH[idx])) / tauMixPAH[idx]
        extPAH = (1.D - p.PAH_COV) + p.PAH_COV * extScrPAH * extMixPAH
     ENDIF ELSE $
        extPAH = Replicate(1.D, N_WAVEMOD)
     jPAH_0 = Jam_DrudeFlux(waveMOD, drude)
     jPAH = extPAH * jPAH_0
     jPAH0 = Interpol(jPAH, logWaveMOD, ALog(wave0.PAH))
     IF (jPAH0 EQ 0.D) THEN jPAH0 = 1.D
     const = p.PAH_FLX * flux0.PAH / jPAH0[0]
     fPAH = (const * jPAH) > 0.D
     fPAH0 = (const * jPAH_0) > 0.D
     IF (N_Elements(fPAH0_Tot) EQ 0) THEN $
        fPAH0_Tot = TSum(logWaveMOD, fPAH0 / waveMOD)
  ENDIF $
  ELSE BEGIN
     extPAH = Replicate(1.D, N_WAVEMOD)
     fPAH = DblArr(N_WAVEMOD)
     fPAH0 = fPAH
     IF (N_Elements(fPAH0_Tot) EQ 0) THEN $
        fPAH0_Tot = TSum(logWaveMOD, Jam_DrudeFlux(waveMOD, drude) / waveMOD)
  ENDELSE

  ;; STR component flux.
  IF (p.STR_FLX GT 0.D) THEN BEGIN
     IF (p.STR_TAU GT 0.D) THEN BEGIN
        tauScrSTR = (1.D - p.STR_MIX) * (p.STR_TAU * K_Tot + tauICE)
        tauMixSTR = p.STR_MIX * (p.STR_TAU * K_Tot + tauICE)
        extScrSTR = Exp(-tauScrSTR)
        extMixSTR = Replicate(1.D, N_WAVEMOD)
        idx = Where(tauMixSTR GT 0.D, cnt)
        IF (cnt GT 0) THEN $
           extMixSTR[idx] = (1.D - Exp(-tauMixSTR[idx])) / tauMixSTR[idx]
        extSTR = (1.D - p.STR_COV) + p.STR_COV * extScrSTR * extMixSTR
     ENDIF ELSE $
        extSTR = Replicate(1.D, N_WAVEMOD)
     jSTR_0 = sourceSTR
     jSTR = extSTR * jSTR_0
     jSTR0 = Interpol(jSTR, logWaveMod, ALog(wave0.STR))
     IF (jSTR0 EQ 0.D) THEN jSTR0 = 1.D
     const = p.STR_FLX * flux0.STR / jSTR0[0]
     fSTR = (const * jSTR) > 0.D
     fSTR0 = (const * jSTR_0) > 0.D
  ENDIF $
  ELSE BEGIN
     extSTR = Replicate(1.D, N_WAVEMOD)
     fSTR = DblArr(N_WAVEMOD)
     fSTR0 = fSTR
  ENDELSE

  ;; STB component flux.
  IF (p.PAH_FLX GT 0.D) THEN BEGIN
     IF (p.STB_TAU GT 0.D) THEN BEGIN
        tauScrSTB = (1.D - p.STB_MIX) * (p.STB_TAU * K_Tot + tauICE)
        tauMixSTB = p.STB_MIX * (p.STB_TAU * K_Tot + tauICE)
        extScrSTB = Exp(-tauScrSTB)
        extMixSTB = Replicate(1.D, N_WAVEMOD)
        idx = Where(tauMixSTB GT 0.D, cnt)
        IF (cnt GT 0) THEN $
           extMixSTB[idx] = (1.D - Exp(-tauMixSTB[idx])) / tauMixSTB[idx]
        extSTB = (1.D - p.STB_COV) + p.STB_COV * extScrSTB * extMixSTB
     ENDIF ELSE $
        extSTB = Replicate(1.D, N_WAVEMOD)
     jSTB0_100 = (p.STB_100 * source100Myr)
     jSTB0_010 = ((1.D - p.STB_100) * p.STB_010 * source10Myr)
     jSTB0_002 = ((1.D - p.STB_100) * (1.D - p.STB_010) * source2Myr)
     jSTB0 = jSTB0_100 + jSTB0_010 + jSTB0_002
     STB_PAH_RATIO = 15.D ;; NGC7714 ~ 15.
     fSTB0_Tot = p.STB_FLX * STB_PAH_RATIO * fPAH0_Tot
     const = fSTB0_Tot / TSum(logWaveMOD, jSTB0 / waveMOD)
     fSTB_100 = (const * jSTB0_100 * extSTB) > 0.D
     fSTB_010 = (const * jSTB0_010 * extSTB) > 0.D
     fSTB_002 = (const * jSTB0_002 * extSTB) > 0.D
     fSTB = fSTB_100 + fSTB_010 + fSTB_002
     fSTB0_100 = (const * jSTB0_100) > 0.D
     fSTB0_010 = (const * jSTB0_010) > 0.D
     fSTB0_002 = (const * jSTB0_002) > 0.D
     fSTB0 = fSTB0_100 + fSTB0_010 + fSTB0_002
  ENDIF $
  ELSE BEGIN
     extSTB = Replicate(1.D, N_WAVEMOD)
     fSTB = DblArr(N_WAVEMOD)
     fSTB_100 = fSTB
     fSTB_010 = fSTB
     fSTB_002 = fSTB
     fSTB0_100 = fSTB
     fSTB0_010 = fSTB
     fSTB0_002 = fSTB
     fSTB0 = fSTB
  ENDELSE

  ;; DSK component flux.
  IF (p.HOT_FLX GT 0.D) THEN BEGIN
     IF (Jam_Check(input, Tag_Name='TIE_DSK_TAU')) THEN BEGIN
        DSK_TAU = (input.Tie_DSK_TAU EQ 'HOT_TAU') ? tau0HOT : p.DSK_TAU
     ENDIF ELSE $
        DSK_TAU = p.DSK_TAU
     extDSK = (DSK_TAU EQ 0.D) ? $
              Replicate(1.D, N_WAVEMOD) : $
              (1.D - p.DSK_COV) + p.DSK_COV * Exp(-DSK_TAU * K_Tot_DSK) * $
              Exp(-tauICE)
     jDSK_0 = sourceDSK
     fDSK0_Tot = p.DSK_FLX * fHOT0_Tot
     const = fDSK0_Tot / TSum(logWaveMOD, jDSK_0 / waveMOD)
     fDSK = (const * jDSK_0 * extDSK) > 0.D
     fDSK0 = (const * jDSK_0) > 0.D
  ENDIF $
  ELSE BEGIN
     extDSK = Replicate(1.D, N_WAVEMOD)
     fDSK = DblArr(N_WAVEMOD)
     fDSK0 = fDSK
  ENDELSE

  ;; LIN component flux.
  IF ((gauss.Wave0)[0] NE -1) THEN BEGIN
     fLIN = Jam_GaussianFlux(waveMOD, gauss) > 0.D
     fLIN0 = fLIN
  ENDIF $
  ELSE BEGIN
     fLIN = Replicate(0.D, N_WAVEMOD)
     fLIN0 = fLIN
  ENDELSE

  ;; Calculate the total flux.
  fluxMOD = fCIR + fCLD + fCOO + fWRM + fHOT + $
            fLIN + fPAH + fSTR + fSTB + fDSK

  ;; Calculate the total flux at the input wavelengths.
  flux = (fixWave) ? Spline(logWaveMOD, fluxMOD, logWave) : fluxMOD

  ;; Integrate over filters.
  IF (input.DoFilter NE 0) THEN BEGIN
     synPhot = Jam_SynPhot(waveMOD * (1.D + z), fluxMOD, Filters=filters)
     tags = Tag_Names(synPhot.Flux)
     N_Tags = N_Elements(tags)
     FOR i=0,N_Tags-1 DO BEGIN
        idx = Where(filterDat EQ tags[i], cnt)
        IF (cnt GT 0) THEN flux[idx] = synPhot.Flux.(i)
     ENDFOR
  ENDIF

  ;; Create components structure.
  IF (Arg_Present(com)) THEN BEGIN
     IF (fixWave) THEN BEGIN
        fCIR = Spline(logWaveMOD, fCIR, logWave)
        fCLD = Spline(logWaveMOD, fCLD, logWave)
        fCOO = Spline(logWaveMOD, fCOO, logWave)
        fWRM = Spline(logWaveMOD, fWRM, logWave)
        fHOT = Spline(logWaveMOD, fHOT, logWave)
        fSTR = Spline(logWaveMOD, fSTR, logWave)
        fSTB = Spline(logWaveMOD, fSTB, logWave)
        fSTB_100 = Spline(logWaveMOD, fSTB_100, logWave)
        fSTB_010 = Spline(logWaveMOD, fSTB_010, logWave)
        fSTB_002 = Spline(logWaveMOD, fSTB_002, logWave)
        fDSK = Spline(logWaveMOD, fDSK, logWave)
        fLIN = Spline(logWaveMOD, fLIN, logWave)
        fPAH = Spline(logWaveMOD, fPAH, logWave)
     ENDIF
     fSRC = fSTR + fSTB + fDSK
     fDST = fCIR + fCLD + fCOO + fWRM + fHOT
     fCON = fSRC + fDST
     com = { Wave:wave, $
             SRC:fSRC, $
             DST:fDST, $
             CON:fCON, $
             CIR:fCIR, $
             CLD:fCLD, $
             COO:fCOO, $
             WRM:fWRM, $
             HOT:fHOT, $
             STR:fSTR, $
             STB:fSTB, $
             STB_100:fSTB_100, $
             STB_010:fSTB_010, $
             STB_002:fSTB_002, $
             DSK:fDSK, $
             LIN:fLIN, $
             PAH:fPAH }
  ENDIF

  ;; Create model components structure.
  IF (Arg_Present(modCom)) THEN BEGIN
     IF (fixWave) THEN BEGIN
        fCIR0 = Spline(logWaveMOD, fCIR0, logWave)
        fCLD0 = Spline(logWaveMOD, fCLD0, logWave)
        fCOO0 = Spline(logWaveMOD, fCOO0, logWave)
        fWRM0 = Spline(logWaveMOD, fWRM0, logWave)
        fHOT0 = Spline(logWaveMOD, fHOT0, logWave)
        fSTR0 = Spline(logWaveMOD, fSTR0, logWave)
        fSTB0 = Spline(logWaveMOD, fSTB0, logWave)
        fSTB0_100 = Spline(logWaveMOD, fSTB0_100, logWave)
        fSTB0_010 = Spline(logWaveMOD, fSTB0_010, logWave)
        fSTB0_002 = Spline(logWaveMOD, fSTB0_002, logWave)
        fDSK0 = Spline(logWaveMOD, fDSK0, logWave)
        fLIN0 = Spline(logWaveMOD, fLIN0, logWave)
        fPAH0 = Spline(logWaveMOD, fPAH0, logWave)
     ENDIF
     fSRC0 = fSTR0 + fSTB0 + fDSK0
     modCom = { Wave:wave, $
                SRC:fSRC0, $
                CIR:fCIR0, $
                CLD:fCLD0, $
                COO:fCOO0, $
                WRM:fWRM0, $
                HOT:fHOT0, $
                STR:fSTR0, $
                STB:fSTB0, $
                STB_100:fSTB0_100, $
                STB_010:fSTB0_010, $
                STB_002:fSTB0_002, $
                DSK:fDSK0, $
                LIN:fLIN0, $
                PAH:fPAH0 }
  ENDIF

  ;; Create ext structure.
  IF (Arg_Present(ext)) THEN BEGIN
     IF (fixWave) THEN BEGIN
        extCOO = Spline(logWaveMOD, extCOO, logWave)
        extWRM = Spline(logWaveMOD, extWRM, logWave)
        extHOT = Spline(logWaveMOD, extHOT, logWave)
        extSTR = Spline(logWaveMOD, extSTR, logWave)
        extSTB = Spline(logWaveMOD, extSTB, logWave)
        extDSK = Spline(logWaveMOD, extDSK, logWave)
        extPAH = Spline(logWaveMOD, extPAH, logWave)
     ENDIF
     ext = { Wave:wave, $
             COO: extCOO, $
             WRM: extWRM, $
             HOT: extHOT, $
             STR: extSTR, $
             STB: extSTB, $
             DSK: extDSK, $
             PAH: extPAH }
  ENDIF

  ;; Create E0 structure.
  IF (Arg_Present(E0)) THEN E0 = { CIR:jCIR0, CLD:jCLD0, COO:jCOO0, $
                                   WRM:jWRM0, HOT:jHOT0 }

  ;; Create Tau0 structure.
  IF (Arg_Present(tau0)) THEN tau0 = { COO:tau0COO, WRM:tau0WRM, HOT:tau0HOT }

  ;; Return the total observed flux.
  RETURN, flux

END

;;+ ===================================================================
; NAME:
; CAFE_LUMINOSITY
;
; PURPOSE:
; This routine calculates the luminosities of the fit components.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

FUNCTION Cafe_Luminosity, SED, FIT, input, p, pE, wave0

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Calculate the luminosity distance in Mpc and the constant required
  ;; to convert flux densities into luminosities in units of L_Sun. The
  ;; constant has units of [um^3 s^-1 (erg s^-1)^-1 L_Sun].
  z = FIT.z
  d_L = LumDist(z, H0=100.D * input.h0, Omega_m=input.Omega_m, $
                Lambda0=input.Lambda0, /Silent)
  const = 7.421D6 * 4.D * !DPi * d_L^2

  ;; Get total dust flux vectors.
  CIR_FLX = FIT.Com.CIR
  CLD_FLX = FIT.Com.CLD
  COO_FLX = FIT.Com.COO
  WRM_FLX = FIT.Com.WRM
  HOT_FLX = FIT.Com.HOT
  COO_FLX_COR = FIT.Com0.COO
  WRM_FLX_COR = FIT.Com0.WRM
  HOT_FLX_COR = FIT.Com0.HOT

  ;; Get total dust flux error vectors.
  CIR_FLX_ERR = FIT.ErrCom.CIR
  CLD_FLX_ERR = FIT.ErrCom.CLD
  COO_FLX_ERR = FIT.ErrCom.COO
  WRM_FLX_ERR = FIT.ErrCom.WRM
  HOT_FLX_ERR = FIT.ErrCom.HOT
  COO_FLX_COR_ERR = FIT.ErrCom0.COO
  WRM_FLX_COR_ERR = FIT.ErrCom0.WRM
  HOT_FLX_COR_ERR = FIT.ErrCom0.HOT

  ;; Calculate the luminosity of each dust component in L_Sun.
  wave = FIT.Com.Wave
  idx = Where((wave GE 0.1D) AND (wave LE 1.3D3))
  logWave = ALog(wave[idx])
  convWave = 1.D / wave[idx] ;; i.e. = wave / wave^2
  L_CIR = const * TSum(logWave, convWave * CIR_FLX[idx])
  L_CLD = const * TSum(logWave, convWave * CLD_FLX[idx])
  L_COO = const * TSum(logWave, convWave * COO_FLX[idx])
  L_WRM = const * TSum(logWave, convWave * WRM_FLX[idx])
  L_HOT = const * TSum(logWave, convWave * HOT_FLX[idx])
  L_CIR_ERR = const * TSum(logWave, convWave * CIR_FLX_ERR[idx])
  L_CLD_ERR = const * TSum(logWave, convWave * CLD_FLX_ERR[idx])
  L_COO_ERR = const * TSum(logWave, convWave * COO_FLX_ERR[idx])
  L_WRM_ERR = const * TSum(logWave, convWave * WRM_FLX_ERR[idx])
  L_HOT_ERR = const * TSum(logWave, convWave * HOT_FLX_ERR[idx])

  ;; Calculate the extinction corrected luminosities.
  L_COO_COR = const * TSum(logWave, convWave * COO_FLX_COR[idx])
  L_WRM_COR = const * TSum(logWave, convWave * WRM_FLX_COR[idx])
  L_HOT_COR = const * TSum(logWave, convWave * HOT_FLX_COR[idx])
  L_COO_COR_ERR = const * TSum(logWave, convWave * COO_FLX_COR_ERR[idx])
  L_WRM_COR_ERR = const * TSum(logWave, convWave * WRM_FLX_COR_ERR[idx])
  L_HOT_COR_ERR = const * TSum(logWave, convWave * HOT_FLX_COR_ERR[idx])

  ;; Calculate PAH luminosity and error.
  PAH_FLX = FIT.Com.PAH
  PAH_FLX_COR = FIT.Com0.PAH
  PAH_FLX_ERR = FIT.ErrCom.PAH
  PAH_FLX_COR_ERR = FIT.ErrCom0.PAH
  L_PAH = const * TSum(logWave, convWave * PAH_FLX[idx])
  L_PAH_COR = const * TSum(logWave, convWave * PAH_FLX_COR[idx])
  L_PAH_ERR = const * TSum(logWave, convWave * PAH_FLX_ERR[idx])
  L_PAH_COR_ERR = const * TSum(logWave, convWave * PAH_FLX_COR_ERR[idx])

  ;; Calculate dust L_TOT = L(1-1000um).
  DST_FLX = FIT.Com.DST
  DST_FLX_ERR = FIT.ErrCom.DST
  L_DST_TOT = const * TSum(logWave, convWave * DST_FLX[idx])
  L_DST_TOT_ERR = const * TSum(logWave, convWave * DST_FLX_ERR[idx])

  ;; Calculate dust L_IR = L(8-1000um).
  idx = Where((wave GE 8.D) AND (wave LE 1D3), cnt)
  IF (cnt GT 0) THEN BEGIN
     logWave = ALog(wave[idx])
     convWave = 1.D / wave[idx] ;; i.e. = wave / wave^2
     L_DST_IR = const * TSum(logWave, convWave * DST_FLX[idx])
     L_DST_IR_ERR = const * $
                    TSum(logWave, convWave * DST_FLX_ERR[idx])
  ENDIF $
  ELSE BEGIN
     L_DST_IR = 0.D
     L_DST_IR_ERR = 0.D
  ENDELSE

  ;; Calculate dust L_NIR = L(1-8um).
  idx = Where((wave GE 1.D) AND (wave LE 8.D), cnt)
  IF (cnt GT 0) THEN BEGIN
     logWave = ALog(wave[idx])
     convWave = 1.D / wave[idx] ;; i.e. = wave / wave^2
     L_DST_NIR = const * TSum(logWave, convWave * DST_FLX[idx])
     L_DST_NIR_ERR = const * $
                     TSum(logWave, convWave * DST_FLX_ERR[idx])
  ENDIF $
  ELSE BEGIN
     L_DST_NIR = 0.D
     L_DST_NIR_ERR = 0.D
  ENDELSE

  ;; Calculate dust L_MIR = L(8-40um).
  idx = Where((wave GE 8.D) AND (wave LE 40.D), cnt)
  IF (cnt GT 0) THEN BEGIN
     logWave = ALog(wave[idx])
     convWave = 1.D / wave[idx] ;; i.e. = wave / wave^2
     L_DST_MIR = const * TSum(logWave, convWave * DST_FLX[idx])
     L_DST_MIR_ERR = const * $
                     TSum(logWave, convWave * DST_FLX_ERR[idx])
  ENDIF $
  ELSE BEGIN
     L_DST_MIR = 0.D
     L_DST_MIR_ERR = 0.D
  ENDELSE

  ;; Calculate dust L_FIR = L(40-1000um).
  idx = Where((wave GE 40.D) AND (wave LE 1D3), cnt)
  IF (cnt GT 0) THEN BEGIN
     logWave = ALog(wave[idx])
     convWave = 1.D / wave[idx] ;; i.e. = wave / wave^2
     L_DST_FIR = const * TSum(logWave, convWave * DST_FLX[idx])
     L_DST_FIR_ERR = const * $
                     TSum(logWave, convWave * DST_FLX_ERR[idx])
  ENDIF $
  ELSE BEGIN
     L_DST_FIR = 0.D
     L_DST_FIR_ERR = 0.D
  ENDELSE

  ;; Calculate total STR, STB, and DSK luminosities.
  ext = FIT.Ext
  errExt = FIT.ErrExt
  wave = FIT.Com.Wave
  logWave = ALog(wave)
  convWave = 1.D / wave ;; i.e. = wave / wave^2
  SRC_FLX = FIT.Com.SRC
  STR_FLX = FIT.Com.STR
  STB_FLX = FIT.Com.STB
  STB_100_FLX = FIT.Com.STB_100
  STB_010_FLX = FIT.Com.STB_010
  STB_002_FLX = FIT.Com.STB_002
  DSK_FLX = FIT.Com.DSK
  SRC_FLX_ERR = FIT.ErrCom.SRC
  STR_FLX_ERR = FIT.ErrCom.STR
  STB_FLX_ERR = FIT.ErrCom.STB
  STB_100_FLX_ERR = FIT.ErrCom.STB_100
  STB_010_FLX_ERR = FIT.ErrCom.STB_010
  STB_002_FLX_ERR = FIT.ErrCom.STB_002
  DSK_FLX_ERR = FIT.ErrCom.DSK
  STR_FLX_COR = FIT.Com0.STR
  STB_FLX_COR = FIT.Com0.STB
  STB_100_FLX_COR = FIT.Com0.STB_100
  STB_010_FLX_COR = FIT.Com0.STB_010
  STB_002_FLX_COR = FIT.Com0.STB_002
  DSK_FLX_COR = FIT.Com0.DSK
  STR_FLX_COR_ERR = Sqrt((STR_FLX_ERR / ext.STR)^2 + $
                         (STR_FLX / (ext.STR)^2 * errExt.STR)^2)
  STB_FLX_COR_ERR = Sqrt((STB_FLX_ERR / ext.STB)^2 + $
                         (STB_FLX / (ext.STB)^2 * errExt.STB)^2)
  STB_100_FLX_COR_ERR = Sqrt((STB_100_FLX_ERR / ext.STB)^2 + $
                             (STB_100_FLX / (ext.STB)^2 * errExt.STB)^2)
  STB_010_FLX_COR_ERR = Sqrt((STB_010_FLX_ERR / ext.STB)^2 + $
                             (STB_010_FLX / (ext.STB)^2 * errExt.STB)^2)
  STB_002_FLX_COR_ERR = Sqrt((STB_002_FLX_ERR / ext.STB)^2 + $
                             (STB_002_FLX / (ext.STB)^2 * errExt.STB)^2)
  DSK_FLX_COR_ERR = Sqrt((DSK_FLX_ERR / ext.DSK)^2 + $
                         (DSK_FLX / (ext.DSK)^2 * errExt.DSK)^2)
  STR_FLX_COR_ERR = FIT.ErrCom0.STR
  STB_FLX_COR_ERR = FIT.ErrCom0.STB
  STB_100_FLX_COR_ERR = FIT.ErrCom0.STB_100
  STB_010_FLX_COR_ERR = FIT.ErrCom0.STB_010
  STB_002_FLX_COR_ERR = FIT.ErrCom0.STB_002
  DSK_FLX_COR_ERR = FIT.ErrCom0.DSK
  L_STR = const * TSum(logWave, convWave * STR_FLX)
  L_STB = const * TSum(logWave, convWave * STB_FLX)
  L_STB_100 = const * TSum(logWave, convWave * STB_100_FLX)
  L_STB_010 = const * TSum(logWave, convWave * STB_010_FLX)
  L_STB_002 = const * TSum(logWave, convWave * STB_002_FLX)
  L_DSK = const * TSum(logWave, convWave * DSK_FLX)
  L_SRC = L_STR + L_STB + L_DSK
  L_STR_ERR = const * TSum(logWave, convWave * STR_FLX_ERR)
  L_STB_ERR = const * TSum(logWave, convWave * STB_FLX_ERR)
  L_STB_100_ERR = const * TSum(logWave, convWave * STB_100_FLX_ERR)
  L_STB_010_ERR = const * TSum(logWave, convWave * STB_010_FLX_ERR)
  L_STB_002_ERR = const * TSum(logWave, convWave * STB_002_FLX_ERR)
  L_DSK_ERR = const * TSum(logWave, convWave * DSK_FLX_ERR)
  L_SRC_ERR = const * TSum(logWave, convWave * SRC_FLX_ERR)
  L_STR_COR = const * TSum(logWave, convWave * STR_FLX_COR)
  L_STB_COR = const * TSum(logWave, convWave * STB_FLX_COR)
  L_STB_100_COR = const * TSum(logWave, convWave * STB_100_FLX_COR)
  L_STB_010_COR = const * TSum(logWave, convWave * STB_010_FLX_COR)
  L_STB_002_COR = const * TSum(logWave, convWave * STB_002_FLX_COR)
  L_DSK_COR = const * TSum(logWave, convWave * DSK_FLX_COR)
  L_SRC_COR = L_STR_COR + L_STB_COR + L_DSK_COR
  L_STR_COR_ERR = const * TSum(logWave, convWave * STR_FLX_COR_ERR)
  L_STB_COR_ERR = const * TSum(logWave, convWave * STB_FLX_COR_ERR)
  L_STB_100_COR_ERR = const * TSum(logWave, convWave * STB_100_FLX_COR_ERR)
  L_STB_010_COR_ERR = const * TSum(logWave, convWave * STB_010_FLX_COR_ERR)
  L_STB_002_COR_ERR = const * TSum(logWave, convWave * STB_002_FLX_COR_ERR)
  L_DSK_COR_ERR = const * TSum(logWave, convWave * DSK_FLX_COR_ERR)
  L_SRC_COR_ERR = Sqrt(L_STR_COR_ERR^2 + L_STB_COR_ERR^2 + L_DSK_COR_ERR^2)

  RETURN, { DST_NIR: L_DST_NIR, $
            DST_MIR: L_DST_MIR, $
            DST_FIR: L_DST_FIR, $
            DST_IR:  L_DST_IR,  $
            DST_TOT: L_DST_TOT, $
            CIR: L_CIR, $
            CLD: L_CLD, $
            COO: L_COO, $
            WRM: L_WRM, $
            HOT: L_HOT, $
            COO_COR: L_COO_COR, $
            WRM_COR: L_WRM_COR, $
            HOT_COR: L_HOT_COR, $
            PAH: L_PAH, $
            PAH_COR: L_PAH_COR, $
            SRC: L_SRC, $
            STR: L_STR, $
            STB: L_STB, $
            STB_100: L_STB_100, $
            STB_010: L_STB_010, $
            STB_002: L_STB_002, $
            DSK: L_DSK, $
            SRC_COR: L_SRC_COR, $
            STR_COR: L_STR_COR, $
            STB_COR: L_STB_COR, $
            STB_100_COR: L_STB_100_COR, $
            STB_010_COR: L_STB_010_COR, $
            STB_002_COR: L_STB_002_COR, $
            DSK_COR: L_DSK_COR, $

            DST_NIR_ERR: L_DST_NIR_ERR, $
            DST_MIR_ERR: L_DST_MIR_ERR, $
            DST_FIR_ERR: L_DST_FIR_ERR, $
            DST_IR_ERR:  L_DST_IR_ERR,  $
            DST_TOT_ERR: L_DST_TOT_ERR, $
            CIR_ERR: L_CIR_ERR, $
            CLD_ERR: L_CLD_ERR, $
            COO_ERR: L_COO_ERR, $
            WRM_ERR: L_WRM_ERR, $
            HOT_ERR: L_HOT_ERR, $
            COO_COR_ERR: L_COO_COR_ERR, $
            WRM_COR_ERR: L_WRM_COR_ERR, $
            HOT_COR_ERR: L_HOT_COR_ERR, $
            PAH_ERR: L_PAH_ERR, $
            PAH_COR_ERR: L_PAH_COR_ERR, $
            SRC_ERR: L_SRC_ERR, $
            STR_ERR: L_STR_ERR, $
            STB_ERR: L_STB_ERR, $
            STB_100_ERR: L_STB_100_ERR, $
            STB_010_ERR: L_STB_010_ERR, $
            STB_002_ERR: L_STB_002_ERR, $
            DSK_ERR: L_DSK_ERR, $
            SRC_COR_ERR: L_SRC_COR_ERR, $
            STR_COR_ERR: L_STR_COR_ERR, $
            STB_COR_ERR: L_STB_COR_ERR, $
            STB_100_COR_ERR: L_STB_100_COR_ERR, $
            STB_010_COR_ERR: L_STB_010_COR_ERR, $
            STB_002_COR_ERR: L_STB_002_COR_ERR, $
            DSK_COR_ERR: L_DSK_COR_ERR }

END

;;+ ===================================================================
; NAME:
; CAFE_MASS
;
; PURPOSE:
; This routine calculates the masses of the fit components.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

FUNCTION Cafe_Mass, SED, FIT, input, p, pE, wave0, flux0, luminosity

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Calculate the luminosity distance in cm.
  z = FIT.z
  cm_Per_Mpc = 3.0857D24
  d_L = cm_Per_Mpc * $
        LumDist(z, H0=100.D * input.h0, Omega_m=input.Omega_m, $
                Lambda0=input.Lambda0, /Silent)

  ;; f_nu(l) = f_nu(l_0) * [E_nu(l) * Ext(l)] / [E_nu(l_0) * Ext(l_0)]
  ;;         = omega * N_H * [E_nu(l) * Ext(l)]
  ;; --> f_nu(l_0) / [E_nu(l_0) * Ext(l_0)] = omega * N_H
  ;; ---> H = D^2 * f_nu(l_0) / [E_nu(l_0) * Ext(l_0)]

  ;; Calculate "total" H-atoms in each dust component.
  wave = FIT.Com.Wave
  logWave = ALog(wave)
  E0 = FIT.E0 ;; [Jy sr-1 H-1 cm+2]
  errE0 = FIT.ErrE0
  fCIR0 = p.CIR_FLX * flux0.CIR ;; [Jy]
  fCLD0 = p.CLD_FLX * flux0.CLD ;; [Jy]
  fCOO0 = p.COO_FLX * flux0.COO
  fWRM0 = p.WRM_FLX * flux0.WRM
  fHOT0 = p.HOT_FLX * flux0.HOT
  err_fCIR0 = pE.CIR_FLX * flux0.CIR
  err_fCLD0 = pE.CLD_FLX * flux0.CLD
  err_fCOO0 = pE.COO_FLX * flux0.COO
  err_fWRM0 = pE.WRM_FLX * flux0.WRM
  err_fHOT0 = pE.HOT_FLX * flux0.HOT
  H_CIR = (E0.CIR EQ -1.D) ? 0.D : d_L^2 * fCIR0 / E0.CIR
  H_CLD = (E0.CLD EQ -1.D) ? 0.D : d_L^2 * fCLD0 / E0.CLD
  H_COO = (E0.COO EQ -1.D) ? 0.D : d_L^2 * fCOO0 / E0.COO
  H_WRM = (E0.WRM EQ -1.D) ? 0.D : d_L^2 * fWRM0 / E0.WRM
  H_HOT = (E0.HOT EQ -1.D) ? 0.D : d_L^2 * fHOT0 / E0.HOT
  err_H_CIR = (E0.CIR LE 0.D) ? 0.D : $
              d_L^2 * Sqrt((err_fCIR0 / E0.CIR)^2 +  $
                           (fCIR0 / (E0.CIR)^2 * errE0.CIR)^2)
  err_H_CLD = (E0.CLD LE 0.D) ? 0.D : $
              d_L^2 * Sqrt((err_fCLD0 / E0.CLD)^2 +  $
                           (fCLD0 / (E0.CLD)^2 * errE0.CLD)^2)
  err_H_COO = (E0.COO LE 0.D) ? 0.D : $
              d_L^2 * Sqrt((err_fCOO0 / E0.COO)^2 +  $
                           (fCOO0 / (E0.COO)^2 * errE0.COO)^2)
  err_H_WRM = (E0.WRM LE 0.D) ? 0.D : $
              d_L^2 * Sqrt((err_fWRM0 / E0.WRM)^2 + $
                           (fWRM0 / (E0.WRM)^2 * errE0.WRM)^2)
  err_H_HOT = (E0.HOT LE 0.D) ? 0.D : $
              d_L^2 * Sqrt((err_fHOT0 / E0.HOT)^2 + $
                           (fHOT0 / (E0.HOT)^2 * errE0.HOT)^2)

  ;; Define grain material densities.
  density_Gra = 2.24D / 1.989D33 / 1D12 ;; M_Sun um^-3 = [2.24 g cm^-3]
  density_Sil = 3.50D / 1.989D33 / 1D12 ;; M_Sun um^-3 = [3.50 g cm^-3]

  ;; Calculate masses.
  T_bb = [p.CIR_TMP, p.CLD_TMP, p.COO_TMP, p.WRM_TMP, p.HOT_TMP]
  dnda = Jam_GrainSizeDF(a, T_bb)
  loga = ALog(a[1] / a[0])
  const = 4.D / 3.D * !DPi * a^4
  const_Gra = const * density_Gra
  const_Sil = const * density_Sil
  massPerH_CIR = Jam_IntTab(const_Gra * (dnda.Carb)[*,0] + $
                            const_Sil * (dnda.Sil)[*,0], loga)
  massPerH_CLD = Jam_IntTab(const_Gra * (dnda.Carb)[*,0] + $
                            const_Sil * (dnda.Sil)[*,0], loga)
  massPerH_COO = Jam_IntTab(const_Gra * (dnda.Carb)[*,1] + $
                            const_Sil * (dnda.Sil)[*,1], loga)
  massPerH_WRM = Jam_IntTab(const_Gra * (dnda.Carb)[*,2] + $
                            const_Sil * (dnda.Sil)[*,2], loga)
  massPerH_HOT = Jam_IntTab(const_Gra * (dnda.Carb)[*,3] + $
                            const_Sil * (dnda.Sil)[*,3], loga)
  massCIR = H_CIR * massPerH_CIR
  massCLD = H_CLD * massPerH_CLD
  massCOO = H_COO * massPerH_COO
  massWRM = H_WRM * massPerH_WRM
  massHOT = H_HOT * massPerH_HOT
  errMassCIR = err_H_CIR * massPerH_CIR
  errMassCLD = err_H_CLD * massPerH_CLD
  errMassCOO = err_H_COO * massPerH_COO
  errMassWRM = err_H_WRM * massPerH_WRM
  errMassHOT = err_H_HOT * massPerH_HOT

  ;; Calculate hydrogen mass.
  m_H = 1.6737249D-24 / 1.98892D+33 ;; Mass of a single H nucleon in [MSun].
  mass_H = 1.4D * m_H * (H_CIR + H_CLD + H_COO + H_WRM + H_HOT)
  err_Mass_H = 1.4D * m_H * Sqrt(err_H_CIR^2 + err_H_CLD^2 + err_H_COO^2 + $
                                 err_H_WRM^2 + err_H_HOT^2)

  ;; Calculate stellar masses.
  IF (p.STB_FLX GT 0.D) THEN BEGIN

     ;; Define M/L ratios for SB99 models.
     M_to_L_100 = 8.50D-2
     M_to_L_010 = 5.35D-3
     M_to_L_002 = 9.02D-4

     ;; Calculate SB component stellar masses.
     mass100 = luminosity.STB_100_COR * M_to_L_100
     mass010 = luminosity.STB_010_COR * M_to_L_010
     mass002 = luminosity.STB_002_COR * M_to_L_002
     errMass100 = luminosity.STB_100_COR_ERR * M_to_L_100
     errMass010 = luminosity.STB_010_COR_ERR * M_to_L_010
     errMass002 = luminosity.STB_002_COR_ERR * M_to_L_002

  ENDIF $
  ELSE BEGIN
     mass002 = 0.D
     mass010 = 0.D
     mass100 = 0.D
     errMass002 = 0.D
     errMass010 = 0.D
     errMass100 = 0.D
  ENDELSE

  RETURN, { CIR:massCIR, $
            CLD:massCLD, $
            COO:massCOO, $
            WRM:massWRM, $
            HOT:massHOT, $

            HTOT:mass_H, $

            STB:mass100 + mass010 + mass002, $
            STB_100:mass100, $
            STB_010:mass010, $
            STB_002:mass002, $

            CIR_ERR:errMassCIR, $
            CLD_ERR:errMassCLD, $
            COO_ERR:errMassCOO, $
            WRM_ERR:errMassWRM, $
            HOT_ERR:errMassHOT, $

            HTOT_ERR:err_Mass_H, $

            STB_ERR:Sqrt(errMass100^2 + errMass010^2 + errMass002^2), $
            STB_100_ERR:errMass100, $
            STB_010_ERR:errMass010, $
            STB_002_ERR:errMass002 }

END

;;+ ===================================================================
; NAME:
; CAFE_MEANTEMP
;
; PURPOSE:
; This routine calculates the mean grain temperature of each component.
; In short, the temperature of a component is equal to the temperature
; of the grain-size which dominates the luminosity of the component
; (since different sized grains are heated to different temperatures in
; the same radiation field). The temperatures are also averaged across
; grain species. See "JAM_GrainMeanEQTemp" and Marshall et al. (2007).
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

FUNCTION Cafe_MeanTemp, p, pE, com, input

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; *** CIR ***
  IF (p.CIR_FLX GT 0.D) THEN BEGIN
     T_CIR = Jam_GrainMeanEQTemp(p.CIR_TMP, $
                                 SourceType=input.Source_CIR, $
                                 Err_T_bb=pE.CIR_TMP, $
                                 Err_MeanEQTemp=T_CIR_Err, /Big)
  ENDIF $
  ELSE BEGIN
     T_CIR = -1.D
     T_CIR_ERR = -1.D
  ENDELSE

  ;; *** CLD ***
  IF (p.CLD_FLX GT 0.D) THEN BEGIN
     T_CLD = Jam_GrainMeanEQTemp(p.CLD_TMP, $
                                 SourceType=input.Source_CLD, $
                                 Err_T_bb=pE.CLD_TMP, $
                                 Err_MeanEQTemp=T_CLD_Err, /Big)
  ENDIF $
  ELSE BEGIN
     T_CLD = -1.D
     T_CLD_ERR = -1.D
  ENDELSE

  ;; *** COO ***
  IF (p.COO_FLX GT 0.D) THEN BEGIN
     T_COO = Jam_GrainMeanEQTemp(p.COO_TMP, $
                                 SourceType=input.Source_COO, $
                                 Err_T_bb=pE.COO_TMP, $
                                 Err_MeanEQTemp=T_COO_Err, /Big)
  ENDIF $
  ELSE BEGIN
     T_COO = -1.D
     T_COO_ERR = -1.D
  ENDELSE

  ;; *** WRM ***
  IF (p.WRM_FLX GT 0.D) THEN BEGIN
     T_WRM = Jam_GrainMeanEQTemp(p.WRM_TMP, $
                                 SourceType=input.Source_WRM, $
                                 Err_T_bb=pE.WRM_TMP, $
                                 Err_MeanEQTemp=T_WRM_Err, /Big)
  ENDIF $
  ELSE BEGIN
     T_WRM = -1.D
     T_WRM_ERR = -1.D
  ENDELSE

  ;; *** HOT ***
  IF (p.HOT_FLX GT 0.D) THEN BEGIN
     T_HOT = Jam_GrainMeanEQTemp(p.HOT_TMP, $
                                 SourceType=input.Source_HOT, $
                                 Err_T_bb=pE.HOT_TMP, $
                                 Err_MeanEQTemp=T_HOT_Err, /Big)
  ENDIF $
  ELSE BEGIN
     T_HOT = -1.D
     T_HOT_ERR = -1.D
  ENDELSE

  RETURN, { CIR:T_CIR, $
            CLD:T_CLD, $
            COO:T_COO, $
            WRM:T_WRM, $
            HOT:T_HOT, $
            CIR_ERR:T_CIR_ERR, $
            CLD_ERR:T_CLD_ERR, $
            COO_ERR:T_COO_ERR, $
            WRM_ERR:T_WRM_ERR, $
            HOT_ERR:T_HOT_ERR }

END

;;+ ===================================================================
; NAME:
; CAFE_YRANGE
;
; PURPOSE:
; This routine calculates the flux density range to use in plots.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

FUNCTION Cafe_YRange, _flux, eFlux

  Compile_Opt IDL2, Hidden
  On_Error, 2

  flux = _flux
  IF (N_Elements(eFlux) GT 0) THEN BEGIN
     idx = Where(flux EQ 0.D, cnt)
     IF (cnt GT 0) THEN flux[idx] = 3.D * eFlux[idx]
  ENDIF
  yRange = MinMax(flux)
  yRange = [0.7D * yRange[0], 1.3D * yRange[1]]

  RETURN, yRange

END

;;+ ===================================================================
; NAME:
; CAFE_OUTPUT
;
; PURPOSE:
; This routine compiles model output into a structure.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

FUNCTION Cafe_Output, SED, input, YRange=yRange, RangeIRS=yRangeIRS, IRS=IRS

  Compile_Opt IDL2, Hidden
  On_Error, 2

  IF (N_Elements(yRangeIRS) EQ 0) THEN yRangeIRS = yRange

  ;; Get output info structures.
  p = Cafe_Params(SED.Cafe.Params.Values)
  pE = Cafe_Params(SED.Cafe.Params.Errors)

  ;; Get data.
  IF (N_Elements(input) EQ 0) THEN input = SED.Cafe.Extra.Input
  luminosity = SED.Cafe.Luminosity
  tags = Tag_Names(luminosity)
  mass = SED.Cafe.Mass
  T_Mean = SED.CAFE.T_Mean
  tau0 = SED.Cafe.Tau0

  ;; Create structures of text output.
  N_FREE = StrCompress(String(SED.Cafe.N_Free, Format='(I2)'), /Remove_All)
  DOF_TOT = StrCompress(String(SED.Cafe.DOF_SED, Format='(I4)'), /Remove_All)
  DOF_OPT_NIR = StrCompress(String(SED.Cafe.DOF_OPT + SED.Cafe.DOF_NIR, $
                                   Format='(I4)'), /Remove_All)
  DOF_IRS_MIR = StrCompress(String(SED.Cafe.DOF_IRS + SED.Cafe.DOF_MIR, $
                                   Format='(I4)'), /Remove_All)
  DOF_FIR_RAD = StrCompress(String(SED.Cafe.DOF_FIR + SED.Cafe.DOF_RAD, $
                                   Format='(I4)'), /Remove_All)
  DOF_FTR = StrCompress(String(SED.Cafe.DOF_FTR, Format='(I4)'), /Remove_All)
  noIRS = (DOF_IRS_MIR LT 50)

  ;; *** REDSHIFT ***
  z = SED.cz / 3D5
  output = { Name:'z', $
             Value:z, $
             Err:0.D, $
             Units:'', $
             ;; Format -> [Scientific?, NoTrimZeros?, SigFigs?, SigFigsErr?]
             Format:[0, 0, 3, 2] }

  ;; *** CHI-SQR ***
  output = [output, { Name:TeXtoIDL('\chi^2/' + DOF_TOT + $
                                    '(' + N_FREE + ')'), $
                      Value:(SED.CAFE.ChiSqrSED / SED.CAFE.DOF_SED), $
                      Err:-1.D, $
                      Units:'', $
                      Format:[0, 1, 3, 0] } ]
  IF (~(Keyword_Set(IRS)) OR (noIRS EQ 1)) THEN BEGIN
     IF (SED.CAFE.ChiSqrOPT NE 0.D) OR (SED.CAFE.ChiSqrNIR NE 0.D) THEN BEGIN
        output = [output, { Name:TeXtoIDL("`\chi^2_{OPT+NIR}'/" + $
                                          DOF_OPT_NIR), $
                            Value:((SED.CAFE.ChiSqrOPT + $
                                    SED.CAFE.ChiSqrNIR) / $
                                   (SED.CAFE.DOF_OPT + SED.CAFE.DOF_NIR)), $
                            Err:-1.D, $
                            Units:'', $
                            Format:[0, 1, 3, 0] } ]
     ENDIF
     IF ((SED.CAFE.ChiSqrIRS NE 0.D) OR (SED.CAFE.ChiSqrMIR NE 0.D)) THEN BEGIN
        output = [output, { Name:TeXtoIDL("`\chi^2_{IRS+MIR}'/" + $
                                          DOF_IRS_MIR), $
                            Value:((SED.CAFE.ChiSqrIRS + $
                                    SED.CAFE.ChiSqrMIR) / $
                                   (SED.CAFE.DOF_IRS + SED.CAFE.DOF_MIR)), $
                            Err:-1.D, $
                            Units:'', $
                            Format:[0, 1, 3, 0] } ]
     ENDIF
     IF ((SED.CAFE.ChiSqrFIR NE 0.D) OR (SED.CAFE.ChiSqrRAD NE 0.D)) THEN BEGIN
        output = [output, { Name:TeXtoIDL("`\chi^2_{FIR+RAD}'/" + $
                                          DOF_FIR_RAD), $
                            Value:((SED.CAFE.ChiSqrFIR + $
                                    SED.CAFE.ChiSqrRAD) / $
                                   (SED.CAFE.DOF_FIR + SED.CAFE.DOF_RAD)), $
                            Err:-1.D, $
                            Units:'', $
                            Format:[0, 1, 3, 0] } ]
     ENDIF
     IF (SED.CAFE.ChiSqrFTR NE 0.D) THEN BEGIN
        output = [output, { Name:TeXtoIDL("`\chi^2_{LIN+PAH}'/" + DOF_FTR), $
                            Value:SED.CAFE.ChiSqrFTR / SED.CAFE.DOF_FTR, $
                            Err:-1.D, $
                            Units:'', $
                            Format:[0, 1, 3, 0] } ]
     ENDIF
  ENDIF

  name_CIR = StrLowCase(input.Label_CIR)
  name_CLD = StrLowCase(input.Label_CLD)
  name_COO = StrLowCase(input.Label_COO)
  name_WRM = StrLowCase(input.Label_WRM)
  name_HOT = StrLowCase(input.Label_HOT)
  name_STR = StrLowCase(input.Label_STR)
  name_STB = StrLowCase(input.Label_STB)
  name_DSK = StrLowCase(input.Label_DSK)
  name_PAH = StrLowCase(input.Label_PAH)
  name_LIN = StrLowCase(input.Label_LIN)

  L_STR_SRC = luminosity.STR_COR / luminosity.SRC_COR
  L_STB_SRC = luminosity.STB_COR / luminosity.SRC_COR
  L_DSK_SRC = luminosity.DSK_COR / luminosity.SRC_COR
  L_SRC_RAT = 0.01D

  EPS = 1D-3

  ;; *** TEMPERATURES ***
  IF ((Keyword_Set(IRS)) OR (noIRS EQ 1)) THEN BEGIN
     IF (Max(SED.Cafe.Fit.Com.CIR) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('T_{' + name_CIR + '}'), $
                            Value:T_Mean.CIR, $
                            Err:T_Mean.CIR_ERR, $
                            Units:'K', $
                            Format:[0, 0, 2, 1] } ]
     ENDIF
     IF (Max(SED.Cafe.Fit.Com.CLD) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('T_{' + name_CLD + '}'), $
                            Value:T_Mean.CLD, $
                            Err:T_Mean.CLD_ERR, $
                            Units:'K', $
                            Format:[0, 0, 2, 1] } ]
     ENDIF
     IF (Max(SED.Cafe.Fit.Com.COO) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('T_{' + name_COO + '}'), $
                            Value:T_Mean.COO, $
                            Err:T_Mean.COO_ERR, $
                            Units:'K', $
                            Format:[0, 0, 2, 1] } ]
     ENDIF
     IF (Max(SED.Cafe.Fit.Com.WRM) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('T_{' + name_WRM + '}'), $
                            Value:T_Mean.WRM, $
                            Err:T_Mean.WRM_ERR, $
                            Units:'K', $
                            Format:[0, 0, 3, 2] } ]
     ENDIF
     IF (Max(SED.Cafe.Fit.Com.HOT) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('T_{' + name_HOT + '}'), $
                            Value:T_Mean.HOT, $
                            Err:T_Mean.HOT_ERR, $
                            Units:'K', $
                            Format:[0, 0, 3, 2] } ]
     ENDIF
  ENDIF

  ;; *** TAU ***
  IF ((Keyword_Set(IRS)) OR (noIRS EQ 1)) THEN BEGIN
     IF ((Max(SED.Cafe.Fit.Com.COO) GT yRange[0]) AND $
         (p.COO_COV GT 1D-2) AND (tau0.COO GT 1D-2)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\tau_{9.7}^{' + name_COO + '}'), $
                            Value:tau0.COO, $
                            Err:tau0.COO_ERR, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((Max(SED.Cafe.Fit.Com.WRM) GT yRange[0]) AND $
         (p.WRM_COV GT 1D-2) AND (tau0.WRM GT 1D-2)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\tau_{9.7}^{' + name_WRM + '}'), $
                            Value:tau0.WRM, $
                            Err:tau0.WRM_ERR, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((Max(SED.Cafe.Fit.Com.HOT) GT yRange[0]) AND $
         (p.HOT_COV GT 1D-2) AND (tau0.HOT GT 1D-2)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\tau_{9.7}^{' + name_HOT + '}'), $
                            Value:tau0.HOT, $
                            Err:tau0.HOT_ERR, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((Max(SED.Cafe.Fit.Com.DSK) GT yRange[0]) AND $
         (p.DSK_COV GT 1D-2) AND (tau0.DSK GT 1D-2)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\tau_{V}^{' + name_DSK + '}'), $
                            Value:tau0.DSK, $
                            Err:tau0.DSK_ERR, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((L_STR_SRC GT L_SRC_RAT) AND $
;;      IF ((Max(SED.Cafe.Fit.Com.STR) GT yRange[0]) AND $
         (p.STR_COV GT 1D-2) AND (p.STR_TAU GT 1D-2)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\tau_{V}^{' + name_STR + '}'), $
                            Value:p.STR_TAU, $
                            Err:pE.STR_TAU, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((L_STB_SRC GT L_SRC_RAT) AND $
;;      IF ((Max(SED.Cafe.Fit.Com.STB) GT yRange[0]) AND $
         (p.STB_COV GT 1D-2) AND (tau0.STB GT 1D-2)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\tau_{V}^{' + name_STB + '}'), $
                            Value:tau0.STB, $
                            Err:tau0.STB_ERR, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((Jam_Check(input, Tag_Name='Tie_PAH_TAU')) && $
         (input.Tie_PAH_TAU EQ 'WRM_TAU')) THEN BEGIN
        tau0PAH = tau0.WRM
        tau0PAH_ERR = tau0.WRM_ERR
     ENDIF $
     ELSE IF ((Jam_Check(input, Tag_Name='Tie_PAH_TAU')) && $
         (input.Tie_PAH_TAU EQ 'COO_TAU')) THEN BEGIN
        tau0PAH = tau0.COO
        tau0PAH_ERR = tau0.COO_ERR
     ENDIF $
     ELSE BEGIN
        tau0PAH = p.PAH_TAU
        tau0PAH_ERR = pE.PAH_TAU
     ENDELSE
     IF ((Max(SED.Cafe.Fit.Com.PAH) GT yRangeIRS[0]) AND $
         (p.PAH_COV GT 1D-2) AND (tau0PAH GT 1D-2)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\tau_{9.7}^{' + name_PAH + '}'), $
                            Value:tau0PAH, $
                            Err:tau0PAH_ERR, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF (p.TAU_ICE GT 1D-2) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\tau_{6.1}^{ice}'), $
                            Value:p.TAU_ICE, $
                            Err:pE.TAU_ICE, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF (p.TAU_HAC GT 1D-2) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\tau_{6.85}^{hac}'), $
                            Value:p.TAU_HAC, $
                            Err:pE.TAU_HAC, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
  ENDIF

  ;; *** MIX ***
  IF ((Keyword_Set(IRS)) OR (noIRS EQ 1)) THEN BEGIN
     IF ((Max(SED.Cafe.Fit.Com.COO) GT yRange[0]) AND $
         (p.COO_MIX * tau0.COO GT 1D-2)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\alpha_{' + $
                                          name_COO + '}^{mixed}'), $
                            Value:p.COO_MIX, $
                            Err:pE.COO_MIX, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((Max(SED.Cafe.Fit.Com.WRM) GT yRange[0]) AND $
         (p.WRM_MIX * tau0.WRM GT 1D-2)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\alpha_{' + $
                                          name_WRM + '}^{mixed}'), $
                            Value:p.WRM_MIX, $
                            Err:pE.WRM_MIX, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((Max(SED.Cafe.Fit.Com.HOT) GT yRange[0]) AND $
         (p.HOT_MIX * tau0.HOT GT 1D-2)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\alpha_{' + $
                                          name_HOT + '}^{mixed}'), $
                            Value:p.HOT_MIX, $
                            Err:pE.HOT_MIX, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((Max(SED.Cafe.Fit.Com.PAH) GT yRangeIRS[0]) AND $
         (p.PAH_MIX * tau0PAH GT 1D-2)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\alpha_{' + $
                                          name_PAH + '}^{mixed}'), $
                            Value:p.PAH_MIX, $
                            Err:pE.PAH_MIX, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((L_STB_SRC GT L_SRC_RAT) AND $
;;      IF ((Max(SED.Cafe.Fit.Com.STB) GT yRange[0]) AND $
         (p.STB_MIX * tau0.STB GT 1D-2)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\alpha_{' + $
                                          name_STB + '}^{mixed}'), $
                            Value:p.STB_MIX, $
                            Err:pE.STB_MIX, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
  ENDIF

  ;; *** COVER ***
  IF ((Keyword_Set(IRS)) OR (noIRS EQ 1)) THEN BEGIN
     IF ((Max(SED.Cafe.Fit.Com.COO) GT yRange[0]) AND $
         (tau0.COO GT 1D-2) AND (p.COO_COV LT 1.D)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\epsilon_{' + $
                                          name_COO + '}^{cover}'), $
                            Value:(p.COO_COV GT 1D-2) ? p.COO_COV : 0.D, $
                            Err:(p.COO_COV GT 1D-2) ? pE.COO_COV : 0.D, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((Max(SED.Cafe.Fit.Com.WRM) GT yRange[0]) AND $
         (tau0.WRM GT 1D-2) AND (p.WRM_COV LT 1.D)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\epsilon_{' + $
                                          name_WRM + '}^{cover}'), $
                            Value:(p.WRM_COV GT 1D-2) ? p.WRM_COV : 0.D, $
                            Err:(p.WRM_COV GT 1D-2) ? pE.WRM_COV : 0.D, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((Max(SED.Cafe.Fit.Com.HOT) GT yRange[0]) AND $
         (tau0.HOT GT 1D-2) AND (p.HOT_COV LT 1.D)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\epsilon_{' + $
                                          name_HOT + '}^{cover}'), $
                            Value:(p.HOT_COV GT 1D-2) ? p.HOT_COV : 0.D, $
                            Err:(p.HOT_COV GT 1D-2) ? pE.HOT_COV : 0.D, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((Max(SED.Cafe.Fit.Com.PAH) GT yRangeIRS[0]) AND $
         (tau0PAH GT 1D-2) AND (p.PAH_COV LT 1.D)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\epsilon_{' + $
                                          name_PAH + '}^{cover}'), $
                            Value:(p.PAH_COV GT 1D-2) ? p.PAH_COV : 0.D, $
                            Err:(p.PAH_COV GT 1D-2) ? pE.PAH_COV : 0.D, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((L_STB_SRC GT L_SRC_RAT) AND $
;;      IF ((Max(SED.Cafe.Fit.Com.STB) GT yRange[0]) AND $
         (tau0.STB GT 1D-2) AND (p.STB_COV LT 1.D)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\epsilon_{' + $
                                          name_STB + '}^{cover}'), $
                            Value:(p.STB_COV GT 1D-2) ? p.STB_COV : 0.D, $
                            Err:(p.STB_COV GT 1D-2) ? pE.STB_COV : 0.D, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
     IF ((L_DSK_SRC GT L_SRC_RAT) AND $
;;      IF ((Max(SED.Cafe.Fit.Com.DSK) GT yRange[0]) AND $
         (tau0.DSK GT 1D-2) AND (p.DSK_COV LT 1.D)) THEN BEGIN
        output = [output, { Name:TeXtoIDL('\epsilon_{' + $
                                          name_DSK + '}^{cover}'), $
                            Value:(p.DSK_COV GT 1D-2) ? p.DSK_COV : 0.D, $
                            Err:(p.DSK_COV GT 1D-2) ? pE.DSK_COV : 0.D, $
                            Units:'', $
                            Format:[0, 0, 3, 3] } ]
     ENDIF
  ENDIF

  ;; *** L_SRC ***
  IF ((~Keyword_Set(IRS)) OR (input.Make_SED_Plot EQ 0)) THEN BEGIN
     IF (luminosity.SRC GT 0.D) THEN BEGIN
        output = [output, { Name:TeXtoIDL('L_{src}'), $
                            Value:luminosity.SRC, $
                            Err:luminosity.SRC_ERR, $
                            Units:'L' + SunSymbol(), $
                            Format:[1, 1, 1, 1] } ]
        IF (luminosity.SRC_COR GT luminosity.SRC) THEN BEGIN
           output = [output, { Name:TeXtoIDL('<L_{src}>'), $
                               Value:luminosity.SRC_COR, $
                               Err:luminosity.SRC_COR_ERR, $
                               Units:'L' + SunSymbol(), $
                               Format:[1, 1, 1, 1] } ]
        ENDIF
     ENDIF
  ENDIF

  ;; *** L_STR ***
  IF ((~Keyword_Set(IRS)) OR (input.Make_SED_Plot EQ 0)) THEN BEGIN
     IF (L_STR_SRC GT L_SRC_RAT) THEN BEGIN
;;      IF (Max(SED.Cafe.Fit.Com.STR) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('L_{' + name_STR + '}'), $
                            Value:luminosity.STR, $
                            Err:luminosity.STR_ERR, $
                            Units:'L' + SunSymbol(), $
                            Format:[1, 1, 1, 1] } ]
        IF ((p.STR_TAU GT 1D-2) AND (p.STR_COV GT 1D-2)) THEN BEGIN
           output = [output, { Name:TeXtoIDL('<L_{' + name_STR + '}>'), $
                               Value:luminosity.STR_COR, $
                               Err:luminosity.STR_COR_ERR, $
                               Units:'L' + SunSymbol(), $
                               Format:[1, 1, 1, 1] } ]
        ENDIF
     ENDIF
  ENDIF

  ;; *** L_STB ***
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (L_STB_SRC GT L_SRC_RAT) THEN BEGIN
;;      IF (Max(SED.Cafe.Fit.Com.STB) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('L_{' + name_STB + '}'), $
                            Value:luminosity.STB, $
                            Err:luminosity.STB_ERR, $
                            Units:'L' + SunSymbol(), $
                            Format:[1, 1, 1, 1] } ]
        IF ((luminosity.STB_100 / luminosity.STB) GT 1D-2) THEN BEGIN
           output = [output, { Name:TeXtoIDL('L_{' + name_STB + '}^{100}'), $
                               Value:luminosity.STB_100, $
                               Err:luminosity.STB_100_ERR, $
                               Units:'L' + SunSymbol(), $
                               Format:[1, 1, 1, 1] } ]
        ENDIF
        IF ((luminosity.STB_010 / luminosity.STB) GT 1D-2) THEN BEGIN
           output = [output, { Name:TeXtoIDL('L_{' + name_STB + '}^{10}'), $
                               Value:luminosity.STB_010, $
                               Err:luminosity.STB_010_ERR, $
                               Units:'L' + SunSymbol(), $
                               Format:[1, 1, 1, 1] } ]
        ENDIF
        IF ((luminosity.STB_002 / luminosity.STB) GT 1D-2) THEN BEGIN
           output = [output, { Name:TeXtoIDL('L_{' + name_STB + '}^{2}'), $
                               Value:luminosity.STB_002, $
                               Err:luminosity.STB_002_ERR, $
                               Units:'L' + SunSymbol(), $
                               Format:[1, 1, 1, 1] } ]
        ENDIF
        IF ((tau0.STB GT 1D-2) AND (p.STB_COV GT 1D-2)) THEN BEGIN
           output = [output, { Name:TeXtoIDL('<L_{' + name_STB + '}>'), $
                               Value:luminosity.STB_COR, $
                               Err:luminosity.STB_COR_ERR, $
                               Units:'L' + SunSymbol(), $
                               Format:[1, 1, 1, 1] } ]
           IF ((luminosity.STB_100 / luminosity.STB) GT 1D-2) THEN BEGIN
              output = [output, { Name:TeXtoIDL('<L_{'+name_STB+'}^{100}>'), $
                                  Value:luminosity.STB_100_COR, $
                                  Err:luminosity.STB_100_COR_ERR, $
                                  Units:'L' + SunSymbol(), $
                                  Format:[1, 1, 1, 1] } ]
           ENDIF
           IF ((luminosity.STB_010 / luminosity.STB) GT 1D-2) THEN BEGIN
              output = [output, { Name:TeXtoIDL('<L_{'+name_STB+'}^{10}>'), $
                                  Value:luminosity.STB_010_COR, $
                                  Err:luminosity.STB_010_COR_ERR, $
                                  Units:'L' + SunSymbol(), $
                                  Format:[1, 1, 1, 1] } ]
           ENDIF
           IF ((luminosity.STB_002 / luminosity.STB) GT 1D-2) THEN BEGIN
              output = [output, { Name:TeXtoIDL('<L_{'+name_STB+'}^{2}>'), $
                                  Value:luminosity.STB_002_COR, $
                                  Err:luminosity.STB_002_COR_ERR, $
                                  Units:'L' + SunSymbol(), $
                                  Format:[1, 1, 1, 1] } ]
           ENDIF
        ENDIF
     ENDIF
  ENDIF

  ;; *** L_DSK ***
  IF ((~Keyword_Set(IRS)) OR (input.Make_SED_Plot EQ 0)) THEN BEGIN
     IF (L_DSK_SRC GT L_SRC_RAT) THEN BEGIN
;;      IF (Max(SED.Cafe.Fit.Com.DSK) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('L_{' + name_DSK + '}'), $
                            Value:luminosity.DSK, $
                            Err:luminosity.DSK_ERR, $
                            Units:'L' + SunSymbol(), $
                            Format:[1, 1, 1, 1] } ]
        IF ((tau0.DSK GT 1D-2) AND (p.DSK_COV GT 1D-2)) THEN BEGIN
           output = [output, { Name:TeXtoIDL('<L_{' + name_DSK + '}>'), $
                               Value:luminosity.DSK_COR, $
                               Err:luminosity.DSK_COR_ERR, $
                               Units:'L' + SunSymbol(), $
                               Format:[1, 1, 1, 1] } ]
        ENDIF
     ENDIF
  ENDIF

  ;; *** L_DST ***
  IF ((~Keyword_Set(IRS)) OR (input.Make_SED_Plot EQ 0)) THEN BEGIN
     IF (luminosity.DST_TOT GT 0.D) THEN BEGIN
        output = [output, { Name:TeXtoIDL('L_{tot}^{dust}'), $
                            Value:luminosity.DST_TOT, $
                            Err:luminosity.DST_TOT_ERR, $
                            Units:'L' + SunSymbol(), $
                            Format:[1, 1, 1, 1] } ]
        output = [output, { Name:TeXtoIDL('L_{ir}^{dust}'), $
                            Value:luminosity.DST_IR, $
                            Err:luminosity.DST_IR_ERR, $
                            Units:'L' + SunSymbol(), $
                            Format:[1, 1, 1, 1] } ]
     ENDIF
  ENDIF

  ;; *** L_CIR ***
  IF ((~Keyword_Set(IRS)) OR (input.Make_SED_Plot EQ 0)) THEN BEGIN
     IF (Max(SED.Cafe.Fit.Com.CIR) GT yRange[0]) THEN BEGIN
        err = Sqrt((luminosity.CIR_ERR / luminosity.DST_TOT)^2 + $
                   (luminosity.CIR / (luminosity.DST_TOT)^2 * $
                    luminosity.DST_TOT_ERR)^2)
        name = 'L_{' + name_CIR + '}/L_{tot}^{dust}'
        output = [output, { Name:TeXtoIDL(name), $
                            Value:luminosity.CIR / luminosity.DST_TOT, $
                            Err:err, $
                            Units:'', $
                            Format:[0, 1, 2, 2] } ]
     ENDIF
  ENDIF

  ;; *** L_CLD ***
  IF ((~Keyword_Set(IRS)) OR (input.Make_SED_Plot EQ 0)) THEN BEGIN
     IF (Max(SED.Cafe.Fit.Com.CLD) GT yRange[0]) THEN BEGIN
        err = Sqrt((luminosity.CLD_ERR / luminosity.DST_TOT)^2 + $
                   (luminosity.CLD / (luminosity.DST_TOT)^2 * $
                    luminosity.DST_TOT_ERR)^2)
        name = 'L_{' + name_CLD + '}/L_{tot}^{dust}'
        output = [output, { Name:TeXtoIDL(name), $
                            Value:luminosity.CLD / luminosity.DST_TOT, $
                            Err:err, $
                            Units:'', $
                            Format:[0, 1, 2, 2] } ]
     ENDIF
  ENDIF

  ;; *** L_COO ***
  IF ((~Keyword_Set(IRS)) OR (input.Make_SED_Plot EQ 0)) THEN BEGIN
     IF (Max(SED.Cafe.Fit.Com.COO) GT yRange[0]) THEN BEGIN
        err = Sqrt((luminosity.COO_ERR / luminosity.DST_TOT)^2 + $
                   (luminosity.COO / (luminosity.DST_TOT)^2 * $
                    luminosity.DST_TOT_ERR)^2)
        name = 'L_{' + name_COO + '}/L_{tot}^{dust}'
        output = [output, { Name:TeXtoIDL(name), $
                            Value:luminosity.COO / luminosity.DST_TOT, $
                            Err:err, $
                            Units:'', $
                            Format:[0, 1, 2, 2] } ]
        IF ((tau0.COO GT 1D-2) AND (p.COO_COV GT 1D-2)) THEN BEGIN
           err = Sqrt((luminosity.COO_COR_ERR / luminosity.DST_TOT)^2 + $
                      (luminosity.COO_COR / (luminosity.DST_TOT)^2 * $
                       luminosity.DST_TOT_ERR)^2)
           name = '<L_{' + name_COO + '}>/L_{tot}^{dust}'
           output = [output, { Name:TeXtoIDL(name), $
                               Value:luminosity.COO_COR / luminosity.DST_TOT, $
                               Err:err, $
                               Units:'', $
                               Format:[0, 1, 2, 2] } ]
        ENDIF
     ENDIF
  ENDIF

  ;; *** L_WRM ***
  IF ((~Keyword_Set(IRS)) OR (input.Make_SED_Plot EQ 0)) THEN BEGIN
     IF (Max(SED.Cafe.Fit.Com.WRM) GT yRange[0]) THEN BEGIN
        err = Sqrt((luminosity.WRM_ERR / luminosity.DST_TOT)^2 + $
                   (luminosity.WRM / (luminosity.DST_TOT)^2 * $
                    luminosity.DST_TOT_ERR)^2)
        name = 'L_{' + name_WRM + '}/L_{tot}^{dust}'
        output = [output, { Name:TeXtoIDL(name), $
                            Value:luminosity.WRM / luminosity.DST_TOT, $
                            Err:err, $
                            Units:'', $
                            Format:[0, 1, 2, 2] } ]
        IF ((tau0.WRM GT 1D-2) AND (p.WRM_COV GT 1D-2)) THEN BEGIN
           err = Sqrt((luminosity.WRM_COR_ERR / luminosity.DST_TOT)^2 + $
                      (luminosity.WRM_COR / (luminosity.DST_TOT)^2 * $
                       luminosity.DST_TOT_ERR)^2)
           name = '<L_{' + name_WRM + '}>/L_{tot}^{dust}'
           output = [output, { Name:TeXtoIDL(name), $
                               Value:luminosity.WRM_COR / luminosity.DST_TOT, $
                               Err:err, $
                               Units:'', $
                               Format:[0, 1, 2, 2] } ]
        ENDIF
     ENDIF
  ENDIF

  ;; *** L_HOT ***
  IF ((~Keyword_Set(IRS)) OR (input.Make_SED_Plot EQ 0)) THEN BEGIN
     IF (Max(SED.Cafe.Fit.Com.HOT) GT yRange[0]) THEN BEGIN
        err = Sqrt((luminosity.HOT_ERR / luminosity.DST_TOT)^2 + $
                   (luminosity.HOT / (luminosity.DST_TOT)^2 * $
                    luminosity.DST_TOT_ERR)^2)
        name = 'L_{' + name_HOT + '}/L_{tot}^{dust}'
        output = [output, { Name:TeXtoIDL(name), $
                            Value:luminosity.HOT / luminosity.DST_TOT, $
                            Err:err, $
                            Units:'', $
                            Format:[0, 1, 2, 2] } ]
        IF ((tau0.HOT GT 1D-2) AND (p.HOT_COV GT 1D-2)) THEN BEGIN
           err = Sqrt((luminosity.HOT_COR_ERR / luminosity.DST_TOT)^2 + $
                      (luminosity.HOT_COR / (luminosity.DST_TOT)^2 * $
                       luminosity.DST_TOT_ERR)^2)
           name = '<L_{' + name_HOT + '}>/L_{tot}^{dust}'
           output = [output, { Name:TeXtoIDL(name), $
                               Value:luminosity.HOT_COR / luminosity.DST_TOT, $
                               Err:err, $
                               Units:'', $
                               Format:[0, 1, 2, 2] } ]
        ENDIF
     ENDIF
  ENDIF

  ;; *** L_PAH ***
  IF ((Keyword_Set(IRS)) AND $
      (Max(SED.Cafe.Fit.Com.PAH) GT yRangeIRS[0])) THEN BEGIN
     err = Sqrt((luminosity.PAH_ERR / luminosity.DST_TOT)^2 + $
                (luminosity.PAH / (luminosity.DST_TOT)^2 * $
                 luminosity.DST_TOT_ERR)^2)
     output = [output, { Name:TeXtoIDL('L_{pah}/L_{tot}^{dust}'), $
                         Value:luminosity.PAH / luminosity.DST_TOT, $
                         Err:err, $
                         Units:'', $
                         Format:[0, 1, 2, 2] } ]
     IF ((tau0PAH GT 1D-2) AND (p.PAH_COV GT 1D-2)) THEN BEGIN
        err = Sqrt((luminosity.PAH_COR_ERR / luminosity.DST_TOT)^2 + $
                   (luminosity.PAH_COR / (luminosity.DST_TOT)^2 * $
                    luminosity.DST_TOT_ERR)^2)
        output = [output, { Name:TeXtoIDL('<L_{pah}>/L_{tot}^{dust}'), $
                            Value:luminosity.PAH_COR / luminosity.DST_TOT, $
                            Err:err, $
                            Units:'', $
                            Format:[0, 1, 2, 2] } ]
     ENDIF
  ENDIF

  ;; *** MASS ***
  IF ((~Keyword_Set(IRS)) OR (input.Make_SED_Plot EQ 0)) THEN BEGIN
     output = [output, { Name:TeXtoIDL('M_H'), $
                         Value:mass.HTOT, $
                         Err:mass.HTOT_ERR, $
                         Units:'M' + SunSymbol(), $
                         Format:[1, 0, 1, 1] } ]
     IF (Max(SED.Cafe.Fit.Com.CIR) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('M_{' + name_CIR + '}'), $
                            Value:mass.CIR, $
                            Err:mass.CIR_ERR, $
                            Units:'M' + SunSymbol(), $
                            Format:[1, 0, 1, 1] } ]
     ENDIF
     IF (Max(SED.Cafe.Fit.Com.CLD) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('M_{' + name_CLD + '}'), $
                            Value:mass.CLD, $
                            Err:mass.CLD_ERR, $
                            Units:'M' + SunSymbol(), $
                            Format:[1, 0, 1, 1] } ]
     ENDIF
     IF (Max(SED.Cafe.Fit.Com.COO) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('M_{' + name_COO + '}'), $
                            Value:mass.COO, $
                            Err:mass.COO_ERR, $
                            Units:'M' + SunSymbol(), $
                            Format:[1, 1, 1, 1] } ]
     ENDIF
     IF (Max(SED.Cafe.Fit.Com.WRM) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('M_{' + name_WRM + '}'), $
                            Value:mass.WRM, $
                            Err:mass.WRM_ERR, $
                            Units:'M' + SunSymbol(), $
                            Format:[1, 1, 1, 1] } ]
     ENDIF
     IF (Max(SED.Cafe.Fit.Com.HOT) GT yRange[0]) THEN BEGIN
        output = [output, { Name:TeXtoIDL('M_{' + name_HOT + '}'), $
                            Value:mass.HOT, $
                            Err:mass.HOT_ERR, $
                            Units:'M' + SunSymbol(), $
                            Format:[1, 1, 1, 1] } ]
     ENDIF
  ENDIF

  ;; PAH ratios.
  IF ((Keyword_Set(IRS)) AND (SED.Cafe.Extra.FitPAHs EQ 1)) THEN BEGIN
     IF (SED.Cafe.PAH.Data.EPower6 GT 0.D) THEN BEGIN
        output = [output, { Name:TeXtoIDL('P(6.2)'), $
                            Value:SED.CAFE.PAH.Data.Power6, $
                            Err:SED.CAFE.PAH.Data.EPower6, $
                            Units:TeXtoIDL('erg s^{-1} cm^{-2}'), $
                            Format:[0, 1, 2, 2] } ]
     ENDIF
     IF (SED.Cafe.PAH.Data.EPower7 GT 0.D) THEN BEGIN
        output = [output, { Name:TeXtoIDL('P(7.7)'), $
                            Value:SED.CAFE.PAH.Data.Power7, $
                            Err:SED.CAFE.PAH.Data.EPower7, $
                            Units:TeXtoIDL('erg s^{-1} cm^{-2}'), $
                            Format:[0, 1, 2, 2] } ]
     ENDIF
     IF (SED.Cafe.PAH.Data.EPower11 GT 0.D) THEN BEGIN
        output = [output, { Name:TeXtoIDL('P(11.3)'), $
                            Value:SED.CAFE.PAH.Data.Power11, $
                            Err:SED.CAFE.PAH.Data.EPower11, $
                            Units:TeXtoIDL('erg s^{-1} cm^{-2}'), $
                            Format:[0, 1, 2, 2] } ]
     ENDIF
     IF ((Finite(SED.CAFE.PAH.Data.RatioExt6)) AND $
         (SED.Cafe.PAH.Data.EPowerExt6 GT 0) AND $
         (SED.Cafe.PAH.Data.EPowerExt7 GT 0)) THEN BEGIN;; AND $
;;         ((SED.CAFE.PAH.Data.RatioExt6 GT SED.CAFE.PAH.Data.ERatioExt6))) THEN BEGIN
        output = [output, { Name:TeXtoIDL('P(6.2)/P(7.7)'), $
                            Value:SED.CAFE.PAH.Data.RatioExt6, $
                            Err:SED.CAFE.PAH.Data.ERatioExt6, $
                            Units:'', $
                            Format:[0, 1, 2, 2] } ]
     ENDIF
     IF ((Finite(SED.CAFE.PAH.Data.Ratio6)) AND $
         (tau0PAH GT 1D-2) AND (p.PAH_COV GT 1D-2) AND $
         (SED.Cafe.PAH.Data.EPower6 GT 0) AND $
         (SED.Cafe.PAH.Data.EPower7 GT 0)) THEN BEGIN;; AND $
;;         ((SED.CAFE.PAH.Data.Ratio6 GT SED.CAFE.PAH.Data.ERatio6))) THEN BEGIN
        output = [output, { Name:TeXtoIDL('<P(6.2)/P(7.7)>'), $
                            Value:SED.CAFE.PAH.Data.Ratio6, $
                            Err:SED.CAFE.PAH.Data.ERatio6, $
                            Units:'', $
                            Format:[0, 1, 2, 2] } ]
     ENDIF
     IF ((Finite(SED.CAFE.PAH.Data.RatioExt11)) AND $
         (SED.Cafe.PAH.Data.EPowerExt11 GT 0) AND $
         (SED.Cafe.PAH.Data.EPowerExt7 GT 0)) THEN BEGIN;; AND $
;;         ((SED.CAFE.PAH.Data.RatioExt11 GT $
;;           SED.CAFE.PAH.Data.ERatioExt11))) THEN BEGIN
        output = [output, { Name:TeXtoIDL('P(11.3)/P(7.7)'), $
                            Value:SED.CAFE.PAH.Data.RatioExt11, $
                            Err:SED.CAFE.PAH.Data.ERatioExt11, $
                            Units:'', $
                            Format:[0, 1, 2, 2] } ]
     ENDIF
     IF ((Finite(SED.CAFE.PAH.Data.Ratio11)) AND $
         (tau0PAH GT 1D-2) AND (p.PAH_COV GT 1D-2) AND $
         (SED.Cafe.PAH.Data.EPower11 GT 0) AND $
         (SED.Cafe.PAH.Data.EPower7 GT 0)) THEN BEGIN ;; AND $
;;         ((SED.CAFE.PAH.Data.Ratio11 GT SED.CAFE.PAH.Data.ERatio11))) THEN BEGIN
        output = [output, { Name:TeXtoIDL('<P(11.3)/P(7.7)>'), $
                            Value:SED.CAFE.PAH.Data.Ratio11, $
                            Err:SED.CAFE.PAH.Data.ERatio11, $
                            Units:'', $
                            Format:[0, 1, 2, 2] } ]
     ENDIF
     IF ((Finite(SED.CAFE.PAH.Data.Ratio12)) AND $
         (SED.Cafe.PAH.Data.EPower12 GT 0) AND $
         (SED.Cafe.PAH.Data.EPower7 GT 0)) THEN BEGIN ;; AND $
;;         ((SED.CAFE.PAH.Data.Ratio12 GT SED.CAFE.PAH.Data.ERatio12))) THEN BEGIN
        output = [output, { Name:TeXtoIDL('<P(12.7)/P(7.7)>'), $
                            Value:SED.CAFE.PAH.Data.Ratio12, $
                            Err:SED.CAFE.PAH.Data.ERatio12, $
                            Units:'', $
                            Format:[0, 1, 2, 2] } ]
     ENDIF
     IF ((Finite(SED.CAFE.PAH.Data.Ratio17)) AND $
         (SED.Cafe.PAH.Data.EPower17 GT 0) AND $
         (SED.Cafe.PAH.Data.EPower7 GT 0)) THEN BEGIN ;; AND $
;;         ((SED.CAFE.PAH.Data.Ratio17 GT SED.CAFE.PAH.Data.ERatio17))) THEN BEGIN
        output = [output, { Name:TeXtoIDL('<P(17)/P(7.7)>'), $
                            Value:SED.CAFE.PAH.Data.Ratio17, $
                            Err:SED.CAFE.PAH.Data.ERatio17, $
                            Units:'', $
                            Format:[0, 1, 3, 3] } ]
     ENDIF
  ENDIF

  RETURN, output

END

;;+ ===================================================================
; NAME:
; CAFE_YTICKFORMAT
;
; PURPOSE:
; This routine formats the y-axis tick labels for SED plots.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

FUNCTION Cafe_YTickFormat, axis, index, value

  Compile_Opt IDL2, Hidden
  On_Error, 2

  RETURN, Jam_Print(value, 1, /Scientific)

END

;;+ ===================================================================
; NAME:
; CAFE_PLOT_DATA
;
; PURPOSE:
; This routine plots the SED and fit results.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

FUNCTION Cafe_Plot_Data, SED, colors, style, input, CRC=CRC, IRS=IRS, $
								 Thick=thick, PS=PS, P_Multi=P_Multi, $
								 XRange=xRange, YRange=yRangeFlux, $
								 OPlotFit=oPlotFit, Lines=lines, _Extra=extra

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Calculate redshift.
  z = SED.Cafe.Fit.z

  ;; Get data vectors.
  IF (Keyword_Set(IRS)) THEN BEGIN
     maskDat = Replicate(0L, N_Elements(SED.Cafe.DAT.Plot))
     maskDat[SED.Cafe.DAT.Index.IRS] = 1L
  ENDIF $
  ELSE BEGIN
     maskDat = Replicate(1L, N_Elements(SED.Cafe.DAT.Plot))
     IF ((input.PlotOPT EQ 0) AND ((SED.Cafe.DAT.Index.OPT)[0] NE -1L)) THEN $
        maskDat[SED.Cafe.DAT.Index.OPT] = 0L
     IF ((input.PlotNIR EQ 0) AND ((SED.Cafe.DAT.Index.NIR)[0] NE -1L)) THEN $
        maskDat[SED.Cafe.DAT.Index.NIR] = 0L
     IF ((input.PlotIRS EQ 0) AND ((SED.Cafe.DAT.Index.IRS)[0] NE -1L)) THEN $
        maskDat[SED.Cafe.DAT.Index.IRS] = 0L
     IF ((input.PlotFIR EQ 0) AND ((SED.Cafe.DAT.Index.FIR)[0] NE -1L)) THEN $
        maskDat[SED.Cafe.DAT.Index.FIR] = 0L
     IF ((input.PlotRAD EQ 0) AND ((SED.Cafe.DAT.Index.RAD)[0] NE -1L)) THEN $
        maskDat[SED.Cafe.DAT.Index.RAD] = 0L
  ENDELSE
  indexDat = Where((SED.Cafe.DAT.Plot * maskDat) EQ 1, countDat)
  IF (countDat EQ 0) THEN indexDat = SED.Cafe.DAT.Index.IRS
  nameDat = (SED.Cafe.DAT.Name)[indexDat]
  filterDat = (SED.Cafe.DAT.Filter)[indexDat]
  waveDat = (SED.Cafe.DAT.Wave)[indexDat]
  fluxDat = (SED.Cafe.DAT.Flux)[indexDat]
  eFluxDat = (SED.Cafe.DAT.Sigma)[indexDat]
  statDat = (SED.Cafe.DAT.Stat)[indexDat]
  fitDat = (SED.Cafe.DAT.Fit)[indexDat]
  plotDat = (SED.Cafe.DAT.Plot)[indexDat]

  ;; Return if IRS plot but no IRS data.
  IF ((Keyword_Set(IRS)) AND (Total(fluxDat) EQ 0.D)) THEN RETURN, 0L

  ;; Create indices to data vectors.
  idxDatIRS = Where(nameDat EQ 'IRS', cntDatIRS)
  IF (cntDatIRS GT 0) THEN BEGIN
     minWaveIRS = Min(waveDat[idxDatIRS], Max=maxWaveIRS)
  ENDIF $
  ELSE BEGIN
     minWaveIRS = 5.D / (1.D + z)
     maxWaveIRS = 40.D / (1.D + z)
  ENDELSE
  idxDatOPT = Where(waveDat LT 1.D)
  idxDatNIR = Where((waveDat GE 1.D) AND (waveDat LT minWaveIRS))
  idxDatMIR = Where((nameDat NE 'IRS') AND $
                    (waveDat GE minWaveIRS) AND $
                    (waveDat LE maxWaveIRS))
  idxDatFIR = Where((waveDat GT maxWaveIRS) AND (waveDat LE 1.3D3))
  idxDatRAD = Where(waveDat GT 1.3D3)

  ;; Get fit vectors.
  indexFit = (Keyword_Set(IRS)) ? SED.Cafe.FIT.Index.IRS : $
             LIndGen(N_Elements(SED.Cafe.FIT.Wave))
  waveFit = (SED.Cafe.FIT.Wave)[indexFit]
  fluxFit = (SED.Cafe.FIT.Flux)[indexFit]
  fluxFitLIN = (SED.Cafe.FIT.Com.LIN)[indexFit]
  fluxFitPAH = (SED.Cafe.FIT.Com.PAH)[indexFit]
  fluxFitCIR = (SED.Cafe.FIT.Com.CIR)[indexFit]
  fluxFitCLD = (SED.Cafe.FIT.Com.CLD)[indexFit]
  fluxFitCOO = (SED.Cafe.FIT.Com.COO)[indexFit]
  fluxFitWRM = (SED.Cafe.FIT.Com.WRM)[indexFit]
  fluxFitHOT = (SED.Cafe.FIT.Com.HOT)[indexFit]
  fluxFitSTR = (SED.Cafe.FIT.Com.STR)[indexFit]
  fluxFitSTB = (SED.Cafe.FIT.Com.STB)[indexFit]
  fluxFitSTB_100 = (SED.Cafe.FIT.Com.STB_100)[indexFit]
  fluxFitSTB_010 = (SED.Cafe.FIT.Com.STB_010)[indexFit]
  fluxFitSTB_002 = (SED.Cafe.FIT.Com.STB_002)[indexFit]
  idxSTB = Where(waveFit GT 10.D, cntSTB)
  IF (cntSTB GT 0) THEN BEGIN
     cutSTB = Exp(-(waveFit[idxSTB] - 10.D) / 10.D)
     fluxFitSTB[idxSTB] *= cutSTB
     fluxFitSTB_100[idxSTB] *= cutSTB
     fluxFitSTB_010[idxSTB] *= cutSTB
     fluxFitSTB_002[idxSTB] *= cutSTB
  ENDIF
  fluxFitDSK = (SED.Cafe.FIT.Com.DSK)[indexFit]
  fluxFitCON = (SED.Cafe.FIT.Com.CON)[indexFit]
  eFluxFit = (SED.Cafe.Fit.Sigma)[indexFit]
  eFluxFitLIN = (SED.Cafe.FIT.ErrCom.LIN)[indexFit]
  eFluxFitPAH = (SED.Cafe.FIT.ErrCom.PAH)[indexFit]
  eFluxFitCIR = (SED.Cafe.FIT.ErrCom.CIR)[indexFit]
  eFluxFitCLD = (SED.Cafe.FIT.ErrCom.CLD)[indexFit]
  eFluxFitCOO = (SED.Cafe.FIT.ErrCom.COO)[indexFit]
  eFluxFitWRM = (SED.Cafe.FIT.ErrCom.WRM)[indexFit]
  eFluxFitHOT = (SED.Cafe.FIT.ErrCom.HOT)[indexFit]
  eFluxFitSTR = (SED.Cafe.FIT.ErrCom.STR)[indexFit]
  eFluxFitSTB = (SED.Cafe.FIT.ErrCom.STB)[indexFit]
  eFluxFitSTB_100 = (SED.Cafe.FIT.ErrCom.STB_100)[indexFit]
  eFluxFitSTB_010 = (SED.Cafe.FIT.ErrCom.STB_010)[indexFit]
  eFluxFitSTB_002 = (SED.Cafe.FIT.ErrCom.STB_002)[indexFit]
  eFluxFitDSK = (SED.Cafe.FIT.ErrCom.DSK)[indexFit]
  eFluxFitCON = (SED.Cafe.FIT.ErrCom.CON)[indexFit]

  ;; Get 'Extra' fit vector to plot.
  doOPlot = (N_Elements(oPlotFit) GT 0)
  IF (doOPlot) THEN $
     fluxFitOPlot = Interpol(oPlotFit.Flux, oPlotFit.Wave, waveFit)

  ;; Get axis types.
  xAxisType = (Keyword_Set(IRS)) ? input.Plot_IRS_XAXS : input.Plot_SED_XAXS
  yAxisType = (Keyword_Set(IRS)) ? input.Plot_IRS_YAXS : input.Plot_SED_YAXS

  ;; Set multiplicative wave/freq factor.
  CASE (xAxisType) OF
     0: wave0Dat = Replicate(1.D, N_Elements(waveDat))
     1: wave0Dat = 3D14 / waveDat^2 ;; [um->Hz]
  ENDCASE
  CASE (xAxisType) OF
     0: wave0Fit = Replicate(1.D, N_Elements(waveFit))
     1: wave0Fit = 3D14 / waveFit^2 ;; [um->Hz]
  ENDCASE

  ;; Set multiplicative flux factor.
  CASE (yAxisType) OF
     0: flux0Dat = (1.D + z) * Replicate(1.D, N_Elements(fluxDat))
     1: flux0Dat = (1.D + z) * Replicate(1D3, N_Elements(fluxDat))
     2: flux0Dat = (1.D + z) * 1D-23 * 3D14 / waveDat^2
     3: flux0Dat = (1.D + z) * 1D-23 * 3D14 / waveDat
  ENDCASE
  CASE (yAxisType) OF
     0: flux0Fit = (1.D + z) * Replicate(1.D, N_Elements(fluxFit))
     1: flux0Fit = (1.D + z) * Replicate(1D3, N_Elements(fluxFit))
     2: flux0Fit = (1.D + z) * 1D-23 * 3D14 / waveFit^2
     3: flux0Fit = (1.D + z) * 1D-23 * 3D14 / waveFit
  ENDCASE
  IF ((yAxisType EQ 2) OR (yAxisType EQ 3)) THEN BEGIN
     logYScale = Floor(ALog10(Min(flux0Fit * fluxFit)))
     yScale = 10^Double(logYScale)
     flux0Dat /= yScale
     flux0Fit /= yScale
  ENDIF

  ;; Sort fit fluxes.
  idxFit = Sort(wave0Fit * waveFit)
  waveFit = (wave0Fit * waveFit)[idxFit]
  fluxFit = (flux0Fit * fluxFit)[idxFit]
  fluxFitLIN = (flux0Fit * fluxFitLIN)[idxFit]
  fluxFitPAH = (flux0Fit * fluxFitPAH)[idxFit]
  fluxFitCIR = (flux0Fit * fluxFitCIR)[idxFit]
  fluxFitCLD = (flux0Fit * fluxFitCLD)[idxFit]
  fluxFitCOO = (flux0Fit * fluxFitCOO)[idxFit]
  fluxFitWRM = (flux0Fit * fluxFitWRM)[idxFit]
  fluxFitHOT = (flux0Fit * fluxFitHOT)[idxFit]
  fluxFitSTR = (flux0Fit * fluxFitSTR)[idxFit]
  fluxFitSTB = (flux0Fit * fluxFitSTB)[idxFit]
  fluxFitSTB_100 = (flux0Fit * fluxFitSTB_100)[idxFit]
  fluxFitSTB_010 = (flux0Fit * fluxFitSTB_010)[idxFit]
  fluxFitSTB_002 = (flux0Fit * fluxFitSTB_002)[idxFit]
  fluxFitDSK = (flux0Fit * fluxFitDSK)[idxFit]
  fluxFitCON = (flux0Fit * fluxFitCON)[idxFit]
  eFluxFit = (flux0Fit * eFluxFit)[idxFit]
  eFluxFitLIN = (flux0Fit * eFluxFitLIN)[idxFit]
  eFluxFitPAH = (flux0Fit * eFluxFitPAH)[idxFit]
  eFluxFitCIR = (flux0Fit * eFluxFitCIR)[idxFit]
  eFluxFitCLD = (flux0Fit * eFluxFitCLD)[idxFit]
  eFluxFitCOO = (flux0Fit * eFluxFitCOO)[idxFit]
  eFluxFitWRM = (flux0Fit * eFluxFitWRM)[idxFit]
  eFluxFitHOT = (flux0Fit * eFluxFitHOT)[idxFit]
  eFluxFitSTR = (flux0Fit * eFluxFitSTR)[idxFit]
  eFluxFitSTB = (flux0Fit * eFluxFitSTB)[idxFit]
  eFluxFitSTB_100 = (flux0Fit * eFluxFitSTB_100)[idxFit]
  eFluxFitSTB_010 = (flux0Fit * eFluxFitSTB_010)[idxFit]
  eFluxFitSTB_002 = (flux0Fit * eFluxFitSTB_002)[idxFit]
  eFluxFitDSK = (flux0Fit * eFluxFitDSK)[idxFit]
  eFluxFitCON = (flux0Fit * eFluxFitCON)[idxFit]
  IF (doOPlot) THEN fluxFitOPlot = (flux0Fit * fluxFitOPlot)[idxFit]

  ;; Calculate min/max wavelengths to plot.
  idxFitDat = Where(plotDat EQ 1L)
  IF (Keyword_Set(IRS)) THEN BEGIN
     minWave0 = Min(waveDat[idxFitDat], Max=maxWave0)
  ENDIF $
  ELSE BEGIN
     minWave0 = ((input.PlotOPT + input.PlotNIR) GT 0) ? 1.D : Min(waveDat)
     maxWave0 = ((input.PlotFIR + input.PlotRAD) GT 0) ? 1D3 : Max(waveDat)
  ENDELSE
  IF (xAxisType EQ 1) THEN BEGIN
     _minWave0 = 3D14 / maxWave0
     _maxWave0 = 3D14 / minWave0
     minWave0 = _minWave0
     maxWave0 = _maxWave0
  ENDIF

  ;; Set wavelength range.
  xLog = (Keyword_Set(IRS)) ? input.Plot_IRS_XLOG : input.Plot_SED_XLOG
  dxLog = (Keyword_Set(IRS)) ? input.Plot_IRS_dx : input.Plot_SED_dx
  dxLin = 0.04D
  xRange0 = (Keyword_Set(IRS)) ? [minWave0, maxWave0] : $
            [Min((wave0Dat * waveDat)[idxFitDat]) < minWave0, $
            Max((wave0Dat * waveDat)[idxFitDat]) > maxWave0]
  dxRange = xRange0[1] - xRange0[0]
  forceXRange = 1
  IF ((N_Elements(xRange) EQ 0) || (xRange[0] NE -1)) THEN BEGIN
     forceXRange = 0
     xRange = (xLog EQ 1) ? $
              [(1.D - dxLog) * xRange0[0], (1.D + dxLog) * xRange0[1]] : $
              [xRange0[0] - (dxLin * dxRange), xRange0[1] + (dxLin * dxRange)]
  ENDIF

  ;; Set flux range.
  yLog = (Keyword_Set(IRS)) ? input.Plot_IRS_YLOG : input.Plot_SED_YLOG
  dyLog = (Keyword_Set(IRS)) ? input.Plot_IRS_dy : input.Plot_SED_dy
  dyLin = 0.04D
  IF (countDat GT 0) THEN BEGIN
     idxPlot = Where(plotDat EQ 1L)
     yRangeFlux = Cafe_YRange(fluxDat[idxPlot], eFluxDat[idxPlot])
     yRange0 = Cafe_YRange((flux0Dat * fluxDat)[idxPlot], $
                                 (flux0Dat * eFluxDat)[idxPlot])

     yRangeFlux = yRangeFlux > $
                  Min(Cafe_YRange(fluxFit / flux0Fit))
     yRange0 = yRange0 > Min(Cafe_YRange(fluxFit))
  ENDIF $
  ELSE BEGIN
     yRangeFlux = Cafe_YRange(fluxFit / flux0Fit)
     yRange0 = Cafe_YRange(fluxFit)
  ENDELSE

  ;; If logarithmic y-axis, make sure there are sufficient major tick marks.
  IF (yLog EQ 1) THEN BEGIN
     ;; Loop twice to handle cases when zero -or- one major tick mark exist.
     FOR i=0,1 DO BEGIN
        logMax = ALog10(yRange0[1])
        logMin = ALog10(2.D * yRange0[0])
        logFluxMax = ALog10(yRangeFlux[1])
        logFluxMin = ALog10(2.D * yRangeFlux[0])
        ;; This checks if there is at most one major tick mark.
        IF ((Floor(logMax) - Ceil(logMin)) LE 0) THEN BEGIN
           ;;; This checks if we should add a major tick mark above...
           testMin = (10^logMin - 10.D^Floor(logMin)) / 10.D^Ceil(logMin)
           testMax = 1.D - $
                     ((10^logMax - 10.D^Floor(logMax)) / 10.D^Ceil(logMax))
           IF (testMax LE testMin) THEN BEGIN
              ;; This checks if the data point is more than halfway to the
              ;; next major tick mark--if not, then don't include the tick.
              IF (testMax LT 0.5D) THEN BEGIN
                 yRange0[1] = 10^Double(Ceil(logMax)) > yRange0[1]
                 yRangeFlux[1] = 10^Double(Ceil(logFluxMax)) > yRangeFlux[1]
              ENDIF
           ENDIF $
           ;;; ...or below the current tick mark.
           ELSE BEGIN
              ;; This checks if the data point is more than halfway to the
              ;; next major tick mark--if not, then don't include the tick.
              IF (testMin LT 0.5D) THEN BEGIN
                 yRange0[0] = 10^Double(Floor(logMin)) < yRange0[0]
                 yRangeFlux[0] = 10^Double(Floor(logFluxMin)) < yRangeFlux[0]
              ENDIF
           ENDELSE
        ENDIF
     ENDFOR
  ENDIF

  ;; Apply y-axis padding
  dyRange = yRange0[1] - yRange0[0]
  yRangeMin = (yLog EQ 1) ? $
              (1.D - dyLog) * yRange0[0] : $
              yRange0[0] - (dyLin * dyRange)
  yRangeFluxMin = (yLog EQ 1) ? $
                  (1.D - dyLog) * yRangeFlux[0] : $
                  yRangeFlux[0] - (dyLin * dyRange)
  yRangeMax = (yLog EQ 1) ? $
              (1.D + dyLog) * yRange0[1] : $
              yRange0[1] + (dyLin * dyRange)
  yRangeFluxMax = (yLog EQ 1) ? $
                  (1.D + dyLog) * yRangeFlux[1] : $
                  yRangeFlux[1] + (dyLin * dyRange)
  yRange = [yRangeMin, yRangeMax]
  yRangeFlux = [yRangeFluxMin, yRangeFluxMax]

  ;; Set plot position.
  xALT = (Keyword_Set(IRS)) ? input.Plot_IRS_XALT : input.Plot_SED_XALT
  IF (Keyword_Set(CRC)) THEN BEGIN
     IF (xALT EQ 1) THEN $
        position = [0.13, 0.13, 0.93, 0.87]
  ENDIF $
  ELSE BEGIN
     IF (xAlt EQ 1) THEN BEGIN
        position = (Keyword_Set(PS)) ? $
                   [0.04, 0.30, 0.70, 0.92] : $
                   [0.09, 0.30, 0.70, 0.92]
     ENDIF $
     ELSE BEGIN
        position = (Keyword_Set(PS)) ? $
                   [0.08, 0.30, 0.70, 0.95] : $ ;; 0.04
                   [0.09, 0.30, 0.70, 0.95]
     ENDELSE
  ENDELSE

  ;; Set x-title.
  IF (Keyword_Set(CRC)) THEN BEGIN
     CASE (xAxisType) OF
        0: xTitle = TeXtoIDL('\lambda_{rest} (\mum)')
        1: xTitle = TeXtoIDL('\nu_{rest} (Hz)')
     ENDCASE
  ENDIF
  CASE (xAxisType) OF
     0: xAltTitle = TeXtoIDL('\nu_{rest} (Hz)')
     1: xAltTitle = TeXtoIDL('\lambda_{rest} (\mum)')
  ENDCASE

  ;; Set y-title.
  CASE (yAxisType) OF
     0: yTitle = TeXtoIDL('f_{\nu} (Jy)')
     1: yTitle = TeXtoIDL('f_{\nu} (mJy)')
     2: yTitle = TeXtoIDL('f_{\lambda} (' + $
                          TeXtoIDL(StrCompress('10^{' + $
                                               String(logYScale) + '}', $
                                               /Remove_All)) + $
                          ' erg s^{-1} cm^{-2} \mum^{-1})')
     3: yTitle = TeXtoIDL('\nu f_{\nu} (' + $
                          TeXtoIDL(StrCompress('10^{' + $
                                               String(logYScale) + '}', $
                                               /Remove_All)) + $
                          ' erg s^{-1} cm^{-2})')
  ENDCASE

  ;; Get plot options.
  plotData = (Keyword_Set(IRS)) ? input.Plot_IRS_Dat : input.Plot_SED_Dat
  plotFit = (Keyword_Set(IRS)) ? input.Plot_IRS_Fit : input.Plot_SED_Fit
  plotLIN = (Keyword_Set(IRS)) ? input.Plot_IRS_LIN : input.Plot_SED_LIN
  plotPAH = (Keyword_Set(IRS)) ? input.Plot_IRS_PAH : input.Plot_SED_PAH
  plotCIR = (Keyword_Set(IRS)) ? input.Plot_IRS_CIR : input.Plot_SED_CIR
  plotCLD = (Keyword_Set(IRS)) ? input.Plot_IRS_CLD : input.Plot_SED_CLD
  plotCOO = (Keyword_Set(IRS)) ? input.Plot_IRS_COO : input.Plot_SED_COO
  plotWRM = (Keyword_Set(IRS)) ? input.Plot_IRS_WRM : input.Plot_SED_WRM
  plotHOT = (Keyword_Set(IRS)) ? input.Plot_IRS_HOT : input.Plot_SED_HOT
  plotSTR = (Keyword_Set(IRS)) ? input.Plot_IRS_STR : input.Plot_SED_STR
  plotSTB = (Keyword_Set(IRS)) ? input.Plot_IRS_STB : input.Plot_SED_STB
  plotSTB_100 = (Keyword_Set(IRS)) ? $
                input.Plot_IRS_STB_100 : input.Plot_SED_STB_100
  plotSTB_010 = (Keyword_Set(IRS)) ? $
                input.Plot_IRS_STB_010 : input.Plot_SED_STB_010
  plotSTB_002 = (Keyword_Set(IRS)) ? $
                input.Plot_IRS_STB_002 : input.Plot_SED_STB_002
  plotDSK = (Keyword_Set(IRS)) ? input.Plot_IRS_DSK : input.Plot_SED_DSK
  plotCON = (Keyword_Set(IRS)) ? input.Plot_IRS_CON : input.Plot_SED_CON
  plotEDat = (Keyword_Set(IRS)) ? input.Plot_IRS_EDat : input.Plot_SED_EDat
  plotEFit = (Keyword_Set(IRS)) ? input.Plot_IRS_EFit : input.Plot_SED_EFit

  ;; Set x-character-size.
  xCharSize = (Keyword_Set(CRC)) ? 1.D : 1D-30

  ;; Plot axes.
  charSize = (Keyword_Set(PS)) ? 1.D : 1.2D
  xStyle = (xALT EQ 0) ? 1 : 9
  IF ((Keyword_Set(IRS)) AND (xLog EQ 1) AND (xAxisType EQ 0)) THEN BEGIN
     xTickV = [1.D + DIndGen(10), 15.D, 20.D, 25.D, 30.D, 35.D]
     IF (xAxisType EQ 1) THEN xTickV = 3D14 / xTickV
     idx = Where((xTickV GE xRange[0]) AND (xTickV LE xRange[1]))
     xTickV = xTickV[idx]
     xTicks = N_Elements(xTickV) - 1
     Plot, [1], [1], /NoData, $
           XLog=xLog, XStyle=xStyle, XRange=xRange, XTitle=xTitle, $
           YLog=yLog, YStyle=1, YRange=yRange, YTitle=yTitle, $
           Position=position, Background=FSC_Color(colors.BCK), $
           Color=FSC_Color(colors.AXS), $
           XCharSize=xCharSize, Thick=thick, XThick=thick, YThick=thick, $
           XTicks=xTicks, XTickV=xTickV, CharSize=charSize, _Extra=extra
  ENDIF $
  ELSE BEGIN
     IF (~Keyword_Set(IRS)) THEN yTickFormat1 = 'Cafe_YTickFormat'
     Plot, [1], [1], /NoData, $
           XLog=xLog, XStyle=xStyle, XRange=xRange, XTitle=xTitle, $
           YLog=yLog, YStyle=1, YRange=yRange, YTitle=yTitle, $
           Position=position, Background=FSC_Color(colors.BCK), $
           Color=FSC_Color(colors.AXS), $
           XCharSize=xCharSize, Thick=thick, XThick=thick, YThick=thick, $
           CharSize=charSize, YTickFormat=yTickFormat1, _Extra=extra
  ENDELSE
  IF (xALT EQ 1) THEN BEGIN
     xRangeAlt = 3D14 / xRange
     Axis, XAxis=1, XTitle=xAltTitle + '!C', XLog=xLog, XStyle=1, $
           XRange=xRangeAlt, Color=FSC_Color(colors.AXS), $
           XThick=thick, YThick=thick, CharSize=charSize
  ENDIF

  ;; Overplot fit errors.
  IF (plotEFit EQ 1) THEN BEGIN
     xTest = (forceXRange EQ 1) ? xRange : xRange0
     idx = Where((waveFit GE xTest[0]) AND (waveFit LE xTest[1]))
     waveFillFit = [waveFit[idx], Reverse(waveFit[idx])]
     eFluxFillMax = fluxFit[idx] + eFluxFitCON[idx]
     eFluxFillMin = fluxFit[idx] - eFluxFitCON[idx]
     fluxFillFit = [eFluxFillMax, Reverse(eFluxFillMin)]
     PolyFill, waveFillFit, fluxFillFit > yRange[0] < yRange[1], $
               Color=FSC_Color(colors.FER)
  ENDIF

  ;; Set-up IRS plotting.
  IF (cntDatIRS GT 0) THEN BEGIN

     ;; Gather IRS data.
     wavePlotIRS = waveDat[idxDatIRS]
     waveIRS = (wave0Dat * waveDat)[idxDatIRS]
     waveMaxIRS = (wave0Dat * waveDat)[Max(idxDatIRS)]
     fluxIRS = (flux0Dat * fluxDat)[idxDatIRS]
     eFluxIRS = (flux0Dat * eFluxDat)[idxDatIRS]
     fitIRS = fitDat[idxDatIRS]
     idxSortIRS = Sort(waveIRS)
     wavePlotIRS = wavePlotIRS[idxSortIRS]
     waveIRS = waveIRS[idxSortIRS]
     fluxIRS = fluxIRS[idxSortIRS]
     eFluxIRS = eFluxIRS[idxSortIRS]
     fitIRS = fitIRS[idxSortIRS]
     plotMinIRS = Min(wavePlotIRS, Max=plotMaxIRS)
     idxPlotIRS = Where((wavePlotIRS GE plotMinIRS) AND $
                        (wavePlotIRS LE plotMaxIRS))
     wavePlotIRS = wavePlotIRS[idxPlotIRS]
     waveIRS = waveIRS[idxPlotIRS]
     fluxIRS = fluxIRS[idxPlotIRS]
     eFluxIRS = eFluxIRS[idxPlotIRS]
     fitIRS = fitIRS[idxPlotIRS]

     ;; Overplot IRS spectral errors.
     IF (plotEDat EQ 1) THEN BEGIN
        idx = Where((waveIRS GE xRange0[0]) AND (waveIRS LE xRange0[1]))
        waveFillIRS = [waveIRS[idx], Reverse(waveIRS[idx])]
        eFluxFillMax = Interpol(fluxFit, waveFit, waveIRS[idx]) + eFluxIRS[idx]
        eFluxFillMin = Interpol(fluxFit, waveFit, waveIRS[idx]) - eFluxIRS[idx]
        fluxFillIRS = [eFluxFillMax, Reverse(eFluxFillMin)]
        PolyFill, waveFillIRS, fluxFillIRS > yRange[0] < yRange[1], $
                  Color=FSC_Color(colors.ERR)
     ENDIF

  ENDIF

  ;; Overplot data.
  IF (plotData EQ 1) THEN BEGIN

     ;; Plot IRS spectra.
     IF (cntDatIRS GT 0) THEN BEGIN
        OPlot, waveIRS, fluxIRS, Color=FSC_Color(colors.DAT), Thick=thick
     ENDIF

     ;; Plot photometry.
     pSize = 1.0
     IF (~Keyword_Set(IRS)) THEN BEGIN

        IF (input.DoFilter NE 0) THEN BEGIN
           synFlux = SED.Cafe.SynPhot.Flux
           synEFlux = SED.Cafe.SynPhot.EFlux
           synWidth = SED.Cafe.SynPhot.Width
           synName = Tag_Names(synFlux)
        ENDIF ELSE $
           synName = ['']

        ;; OPT
        IF (idxDatOPT[0] NE -1L) THEN BEGIN
           nameOPT = nameDat[idxDatOPT]
           filterOPT = filterDat[idxDatOPT]
           waveOPT = (wave0Dat * waveDat)[idxDatOPT]
           fluxOPT = (flux0Dat * fluxDat)[idxDatOPT]
           eFluxOPT = (flux0Dat * eFluxDat)[idxDatOPT]
           statOPT = statDat[idxDatOPT]
           fitOPT = fitDat[idxDatOPT]
           plotOPT = plotDat[idxDatOPT]
           ;; Detections...
           idx = Where(statOPT EQ 0L, cnt)
           IF (cnt GT 0) THEN BEGIN
              filterOPTDet = filterOPT[idx]
              waveOPTDet = waveOPT[idx]
              fluxOPTDet = fluxOPT[idx]
              eFluxOPTDet = eFluxOPT[idx]
              fitOPTDet = fitOPT[idx]
              idx = Where(fitOPTDet EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlotError, waveOPTDet[idx], fluxOPTDet[idx], $
                             eFluxOPTDet[idx], /NoHat, $
                             ErrColor=FSC_Color(colors.DAT), PSym=8
              ENDIF
              idx = Where(fitOPTDet EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 IF (Total(eFluxOPTDet[idx]) GT 0.D) THEN BEGIN
                    OPlotError, waveOPTDet[idx], fluxOPTDet[idx], $
                                eFluxOPTDet[idx], /NoHat, $
                                ErrColor=FSC_Color(colors.DAT), PSym=8
                 ENDIF ELSE $
                    OPlot, waveOPTDet[idx], fluxOPTDet[idx], PSym=8
              ENDIF
           ENDIF
           ;; Upper-limits...
           idx = Where(statOPT EQ 1L, cnt)
           IF (cnt GT 0) THEN BEGIN
              filterOPTLim = filterOPT[idx]
              waveOPTLim = waveOPT[idx]
              fluxOPTLim = fluxOPT[idx]
              eFluxOPTLim = eFluxOPT[idx]
              fitOPTLim = fitOPT[idx]
              idx = Where(fitOPTLim EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveOPTLim[idx], $
                        fluxOPTLim[idx] + 3.D * eFluxOPTLim[idx], PSym=8
                 PlotSym, 1, 1.5, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveOPTLim[idx], $
                        fluxOPTLim[idx] + 3.D * eFluxOPTLim[idx], PSym=8
              ENDIF
              idx = Where(fitOPTLim EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveOPTLim[idx], $
                        fluxOPTLim[idx] + 3.D * eFluxOPTLim[idx], PSym=8
                 PlotSym, 1, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveOPTLim[idx], $
                        fluxOPTLim[idx] + 3.D * eFluxOPTLim[idx], PSym=8
              ENDIF
           ENDIF
           ;; Lower-limits...
           idx = Where(statOPT EQ -1L, cnt)
           IF (cnt GT 0) THEN BEGIN
              waveOPTLim = waveOPT[idx]
              fluxOPTLim = fluxOPT[idx]
              eFluxOPTLim = eFluxOPT[idx]
              fitOPTLim = fitOPT[idx]
              idx = Where(fitOPTLim EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveOPTLim[idx], $
                        fluxOPTLim[idx] - 3.D * eFluxOPTLim[idx], PSym=8
                 PlotSym, 2, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveOPTLim[idx], $
                        fluxOPTLim[idx] - 3.D * eFluxOPTLim[idx], PSym=8
              ENDIF
              idx = Where(fitOPTLim EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveOPTLim[idx], $
                        fluxOPTLim[idx] - 3.D * eFluxOPTLim[idx], PSym=8
                 PlotSym, 2, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveOPTLim[idx], $
                        fluxOPTLim[idx] - 3.D * eFluxOPTLim[idx], PSym=8
              ENDIF
           ENDIF
        ENDIF

        ;; NIR
        IF (idxDatNIR[0] NE -1L) THEN BEGIN
           nameNIR = nameDat[idxDatNIR]
           filterNIR = filterDat[idxDatNIR]
           waveNIR = (wave0Dat * waveDat)[idxDatNIR]
           fluxNIR = (flux0Dat * fluxDat)[idxDatNIR]
           eFluxNIR = (flux0Dat * eFluxDat)[idxDatNIR]
           statNIR = statDat[idxDatNIR]
           fitNIR = fitDat[idxDatNIR]
           plotNIR = plotDat[idxDatNIR]
           ;; Detections...
           idx = Where(statNIR EQ 0L, cnt)
           IF (cnt GT 0) THEN BEGIN
              filterNIRDet = filterNIR[idx]
              waveNIRDet = waveNIR[idx]
              fluxNIRDet = fluxNIR[idx]
              eFluxNIRDet = eFluxNIR[idx]
              fitNIRDet = fitNIR[idx]
              idx = Where(fitNIRDet EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlotError, waveNIRDet[idx], fluxNIRDet[idx], $
                             eFluxNIRDet[idx], /NoHat, $
                             ErrColor=FSC_Color(colors.DAT), PSym=8
              ENDIF
              idx = Where(fitNIRDet EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 IF (Total(eFluxNIRDet[idx]) GT 0.D) THEN BEGIN
                    OPlotError, waveNIRDet[idx], fluxNIRDet[idx], $
                                eFluxNIRDet[idx], /NoHat, $
                                ErrColor=FSC_Color(colors.DAT), PSym=8
                 ENDIF ELSE $
                    OPlot, waveNIRDet[idx], fluxNIRDet[idx], PSym=8
              ENDIF
           ENDIF
           ;; Upper-limits...
           idx = Where(statNIR EQ 1L, cnt)
           IF (cnt GT 0) THEN BEGIN
              filterNIRLim = filterNIR[idx]
              waveNIRLim = waveNIR[idx]
              fluxNIRLim = fluxNIR[idx]
              eFluxNIRLim = eFluxNIR[idx]
              fitNIRLim = fitNIR[idx]
              idx = Where(fitNIRLim EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveNIRLim[idx], $
                        fluxNIRLim[idx] + 3.D * eFluxNIRLim[idx], PSym=8
                 PlotSym, 1, 1.5, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveNIRLim[idx], $
                        fluxNIRLim[idx] + 3.D * eFluxNIRLim[idx], PSym=8
              ENDIF
              idx = Where(fitNIRLim EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveNIRLim[idx], $
                        fluxNIRLim[idx] + 3.D * eFluxNIRLim[idx], PSym=8
                 PlotSym, 1, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveNIRLim[idx], $
                        fluxNIRLim[idx] + 3.D * eFluxNIRLim[idx], PSym=8
              ENDIF
           ENDIF
           ;; Lower-limits...
           idx = Where(statNIR EQ -1L, cnt)
           IF (cnt GT 0) THEN BEGIN
              waveNIRLim = waveNIR[idx]
              fluxNIRLim = fluxNIR[idx]
              eFluxNIRLim = eFluxNIR[idx]
              fitNIRLim = fitNIR[idx]
              idx = Where(fitNIRLim EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveNIRLim[idx], $
                        fluxNIRLim[idx] - 3.D * eFluxNIRLim[idx], PSym=8
                 PlotSym, 2, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveNIRLim[idx], $
                        fluxNIRLim[idx] - 3.D * eFluxNIRLim[idx], PSym=8
              ENDIF
              idx = Where(fitNIRLim EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveNIRLim[idx], $
                        fluxNIRLim[idx] - 3.D * eFluxNIRLim[idx], PSym=8
                 PlotSym, 2, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveNIRLim[idx], $
                        fluxNIRLim[idx] - 3.D * eFluxNIRLim[idx], PSym=8
              ENDIF
           ENDIF
        ENDIF

        ;; MIR
        IF (idxDatMIR[0] NE -1L) THEN BEGIN
           nameMIR = nameDat[idxDatMIR]
           filterMIR = filterDat[idxDatMIR]
           waveMIR = (wave0Dat * waveDat)[idxDatMIR]
           fluxMIR = (flux0Dat * fluxDat)[idxDatMIR]
           eFluxMIR = (flux0Dat * eFluxDat)[idxDatMIR]
           statMIR = statDat[idxDatMIR]
           fitMIR = fitDat[idxDatMIR]
           plotMIR = plotDat[idxDatMIR]
           ;; Detections...
           idx = Where(statMIR EQ 0L, cnt)
           IF (cnt GT 0) THEN BEGIN
              filterMIRDet = filterMIR[idx]
              waveMIRDet = waveMIR[idx]
              fluxMIRDet = fluxMIR[idx]
              eFluxMIRDet = eFluxMIR[idx]
              fitMIRDet = fitMIR[idx]
              idx = Where(fitMIRDet EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0,/Fill, Color=FSC_Color(colors.Dat), Thick=thick
                 OPlotError, waveMIRDet[idx], fluxMIRDet[idx], $
                             eFluxMIRDet[idx], /NoHat, $
                             ErrColor=FSC_Color(colors.DAT), $
                             PSym=8
              ENDIF
              idx = Where(fitMIRDet EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 IF (Total(eFluxMIRDet[idx]) GT 0.D) THEN BEGIN
                    OPlotError, waveMIRDet[idx], fluxMIRDet[idx], $
                                eFluxMIRDet[idx], /NoHat, $
                                ErrColor=FSC_Color(colors.DAT), PSym=8
                 ENDIF ELSE $
                    OPlot, waveMIRDet[idx], fluxMIRDet[idx], PSym=8
              ENDIF
           ENDIF
           ;; Upper-limits...
           idx = Where(statMIR EQ 1L, cnt)
           IF (cnt GT 0) THEN BEGIN
              filterMIRLim = filterMIR[idx]
              waveMIRLim = waveMIR[idx]
              fluxMIRLim = fluxMIR[idx]
              eFluxMIRLim = eFluxMIR[idx]
              fitMIRLim = fitMIR[idx]
              idx = Where(fitMIRLim EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveMIRLim[idx], $
                        fluxMIRLim[idx] + 3.D * eFluxMIRLim[idx], PSym=8
                 PlotSym, 1, 1.5, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveMIRLim[idx], $
                        fluxMIRLim[idx] + 3.D * eFluxMIRLim[idx], PSym=8
              ENDIF
              idx = Where(fitMIRLim EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveMIRLim[idx], $
                        fluxMIRLim[idx] + 3.D * eFluxMIRLim[idx], PSym=8
                 PlotSym, 1, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveMIRLim[idx], $
                        fluxMIRLim[idx] + 3.D * eFluxMIRLim[idx], PSym=8
              ENDIF
           ENDIF
           ;; Lower-limits...
           idx = Where(statMIR EQ -1L, cnt)
           IF (cnt GT 0) THEN BEGIN
              waveMIRLim = waveMIR[idx]
              fluxMIRLim = fluxMIR[idx]
              eFluxMIRLim = eFluxMIR[idx]
              fitMIRLim = fitMIR[idx]
              idx = Where(fitMIRLim EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveMIRLim[idx], $
                        fluxMIRLim[idx] - 3.D * eFluxMIRLim[idx], PSym=8
                 PlotSym, 2, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveMIRLim[idx], $
                        fluxMIRLim[idx] - 3.D * eFluxMIRLim[idx], PSym=8
              ENDIF
              idx = Where(fitMIRLim EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveMIRLim[idx], $
                        fluxMIRLim[idx] - 3.D * eFluxMIRLim[idx], PSym=8
                 PlotSym, 2, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveMIRLim[idx], $
                        fluxMIRLim[idx] - 3.D * eFluxMIRLim[idx], PSym=8
              ENDIF
           ENDIF
        ENDIF

        ;; FIR
        IF (idxDatFIR[0] NE -1L) THEN BEGIN
           nameFIR = nameDat[idxDatFIR]
           filterFIR = filterDat[idxDatFIR]
           waveFIR = (wave0Dat * waveDat)[idxDatFIR]
           fluxFIR = (flux0Dat * fluxDat)[idxDatFIR]
           eFluxFIR = (flux0Dat * eFluxDat)[idxDatFIR]
           statFIR = statDat[idxDatFIR]
           fitFIR = fitDat[idxDatFIR]
           plotFIR = plotDat[idxDatFIR]
           ;; Detections...
           idx = Where(statFIR EQ 0L, cnt)
           IF (cnt GT 0) THEN BEGIN
              filterFIRDet = filterFIR[idx]
              waveFIRDet = waveFIR[idx]
              fluxFIRDet = fluxFIR[idx]
              eFluxFIRDet = eFluxFIR[idx]
              fitFIRDet = fitFIR[idx]
              idx = Where(fitFIRDet EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlotError, waveFIRDet[idx], fluxFIRDet[idx], $
                             eFluxFIRDet[idx], /NoHat, $
                             ErrColor=FSC_Color(colors.DAT), $
                             PSym=8
              ENDIF
              idx = Where(fitFIRDet EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 IF (Total(eFluxFIRDet[idx]) GT 0.D) THEN BEGIN
                    OPlotError, waveFIRDet[idx], fluxFIRDet[idx], $
                                eFluxFIRDet[idx], /NoHat, $
                                ErrColor=FSC_Color(colors.DAT), PSym=8
                 ENDIF ELSE $
                    OPlot, waveFIRDet[idx], fluxFIRDet[idx], PSym=8
              ENDIF
           ENDIF
           ;; Upper-limits...
           idx = Where(statFIR EQ 1L, cnt)
           IF (cnt GT 0) THEN BEGIN
              filterFIRLim = filterFIR[idx]
              waveFIRLim = waveFIR[idx]
              fluxFIRLim = fluxFIR[idx]
              eFluxFIRLim = eFluxFIR[idx]
              fitFIRLim = fitFIR[idx]
              idx = Where(fitFIRLim EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveFIRLim[idx], $
                        fluxFIRLim[idx] + 3.D * eFluxFIRLim[idx], PSym=8
                 PlotSym, 1, 1.5, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveFIRLim[idx], $
                        fluxFIRLim[idx] + 3.D * eFluxFIRLim[idx], PSym=8
              ENDIF
              idx = Where(fitFIRLim EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveFIRLim[idx], $
                        fluxFIRLim[idx] + 3.D * eFluxFIRLim[idx], PSym=8
                 PlotSym, 1, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveFIRLim[idx], $
                        fluxFIRLim[idx] + 3.D * eFluxFIRLim[idx], PSym=8
              ENDIF
           ENDIF
           ;; Lower-limits...
           idx = Where(statFIR EQ -1L, cnt)
           IF (cnt GT 0) THEN BEGIN
              waveFIRLim = waveFIR[idx]
              fluxFIRLim = fluxFIR[idx]
              eFluxFIRLim = eFluxFIR[idx]
              fitFIRLim = fitFIR[idx]
              idx = Where(fitFIRLim EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveFIRLim[idx], $
                        fluxFIRLim[idx] - 3.D * eFluxFIRLim[idx], PSym=8
                 PlotSym, 2, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveFIRLim[idx], $
                        fluxFIRLim[idx] - 3.D * eFluxFIRLim[idx], PSym=8
              ENDIF
              idx = Where(fitFIRLim EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveFIRLim[idx], $
                        fluxFIRLim[idx] - 3.D * eFluxFIRLim[idx], PSym=8
                 PlotSym, 2, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveFIRLim[idx], $
                        fluxFIRLim[idx] - 3.D * eFluxFIRLim[idx], PSym=8
              ENDIF
           ENDIF
        ENDIF

        ;; RAD
        IF (idxDatRAD[0] NE -1L) THEN BEGIN
           nameRAD = nameDat[idxDatRAD]
           filterRAD = filterDat[idxDatRAD]
           waveRAD = (wave0Dat * waveDat)[idxDatRAD]
           fluxRAD = (flux0Dat * fluxDat)[idxDatRAD]
           eFluxRAD = (flux0Dat * eFluxDat)[idxDatRAD]
           statRAD = statDat[idxDatRAD]
           fitRAD = fitDat[idxDatRAD]
           plotRAD = plotDat[idxDatRAD]
           ;; Detections...
           idx = Where(statRAD EQ 0L, cnt)
           IF (cnt GT 0) THEN BEGIN
              filterRADDet = filterRAD[idx]
              waveRADDet = waveRAD[idx]
              fluxRADDet = fluxRAD[idx]
              eFluxRADDet = eFluxRAD[idx]
              fitRADDet = fitRAD[idx]
              idx = Where(fitRADDet EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0,/Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlotError, waveRADDet[idx], fluxRADDet[idx], $
                             eFluxRADDet[idx], /NoHat, $
                             ErrColor=FSC_Color(colors.DAT), $
                             PSym=8
              ENDIF
              idx = Where(fitRADDet EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 IF (Total(eFluxRADDet[idx]) GT 0.D) THEN BEGIN
                    OPlotError, waveRADDet[idx], fluxRADDet[idx], $
                                eFluxRADDet[idx], /NoHat, $
                                ErrColor=FSC_Color(colors.DAT), PSym=8
                 ENDIF ELSE $
                    OPlot, waveRADDet[idx], fluxRADDet[idx], PSym=8
              ENDIF
           ENDIF
           ;; Upper-limits...
           idx = Where(statRAD EQ 1L, cnt)
           IF (cnt GT 0) THEN BEGIN
              filterRADLim = filterRAD[idx]
              waveRADLim = waveRAD[idx]
              fluxRADLim = fluxRAD[idx]
              eFluxRADLim = eFluxRAD[idx]
              fitRADLim = fitRAD[idx]
              idx = Where(fitRADLim EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveRADLim[idx], $
                        fluxRADLim[idx] + 3.D * eFluxRADLim[idx], PSym=8
                 PlotSym, 1, 1.5, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveRADLim[idx], $
                        fluxRADLim[idx] + 3.D * eFluxRADLim[idx], PSym=8
              ENDIF
              idx = Where(fitRADLim EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveRADLim[idx], $
                        fluxRADLim[idx] + 3.D * eFluxRADLim[idx], PSym=8
                 PlotSym, 1, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveRADLim[idx], $
                        fluxRADLim[idx] + 3.D * eFluxRADLim[idx], PSym=8
              ENDIF
           ENDIF
           ;; Lower-limits...
           idx = Where(statRAD EQ -1L, cnt)
           IF (cnt GT 0) THEN BEGIN
              waveRADLim = waveRAD[idx]
              fluxRADLim = fluxRAD[idx]
              eFluxRADLim = eFluxRAD[idx]
              fitRADLim = fitRAD[idx]
              idx = Where(fitRADLim EQ 1L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveRADLim[idx], $
                        fluxRADLim[idx] - 3.D * eFluxRADLim[idx], PSym=8
                 PlotSym, 2, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveRADLim[idx], $
                        fluxRADLim[idx] - 3.D * eFluxRADLim[idx], PSym=8
              ENDIF
              idx = Where(fitRADLim EQ 0L, cnt)
              IF (cnt GT 0) THEN BEGIN
                 PlotSym, 0, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveRADLim[idx], $
                        fluxRADLim[idx] - 3.D * eFluxRADLim[idx], PSym=8
                 PlotSym, 2, 1.5, Color=FSC_Color(colors.DAT), Thick=thick
                 OPlot, waveRADLim[idx], $
                        fluxRADLim[idx] - 3.D * eFluxRADLim[idx], PSym=8
              ENDIF
           ENDIF
        ENDIF
     ENDIF
  ENDIF

  ;; Mark IRAC and MIPS data.
  IF (input.CircleIRAC NE 0) THEN BEGIN
     nameNIR5 = StrMid(nameDat, 0, 5)
     idxIRAC36 = Where((nameNIR5 EQ 'IRAC1') AND (plotDat EQ 1), cntIRAC36)
     idxIRAC45 = Where((nameNIR5 EQ 'IRAC2') AND (plotDat EQ 1), cntIRAC45)
     idxIRAC58 = Where((nameNIR5 EQ 'IRAC3') AND (plotDat EQ 1), cntIRAC58)
     idxIRAC80 = Where((nameNIR5 EQ 'IRAC4') AND (plotDat EQ 1), cntIRAC80)
     PlotSym, 0, 1.5, Thick=thick, Color=FSC_Color(colors.DAT)
     IF (cntIRAC36 GT 0) THEN BEGIN
        waveIRAC36 = (wave0Dat * waveDat)[idxIRAC36]
        fluxIRAC36 = (flux0Dat * fluxDat)[idxIRAC36]
        eFluxIRAC36 = (flux0Dat * eFluxDat)[idxIRAC36]
        ulIRAC36 = fluxIRAC36 + 3.D * eFluxIRAC36
        llIRAC36 = fluxIRAC36 - 3.D * eFluxIRAC36
        idx = Where(statDat[idxIRAC36] EQ 0L, cnt)
        IF (cnt GT 0) THEN OPlot, waveIRAC36[idx], fluxIRAC36[idx], PSym=8
        idx = Where(statDat[idxIRAC36] EQ 1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveIRAC36[idx], ulIRAC36[idx], PSym=8
        idx = Where(statDat[idxIRAC36] EQ -1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveIRAC36[idx], llIRAC36[idx], PSym=8
     ENDIF
     IF (cntIRAC45 GT 0) THEN BEGIN
        waveIRAC45 = (wave0Dat * waveDat)[idxIRAC45]
        fluxIRAC45 = (flux0Dat * fluxDat)[idxIRAC45]
        eFluxIRAC45 = (flux0Dat * eFluxDat)[idxIRAC45]
        ulIRAC45 = fluxIRAC45 + 3.D * eFluxIRAC45
        llIRAC45 = fluxIRAC45 - 3.D * eFluxIRAC45
        idx = Where(statDat[idxIRAC45] EQ 0L, cnt)
        IF (cnt GT 0) THEN OPlot, waveIRAC45[idx], fluxIRAC45[idx], PSym=8
        idx = Where(statDat[idxIRAC45] EQ 1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveIRAC45[idx], ulIRAC45[idx], PSym=8
        idx = Where(statDat[idxIRAC45] EQ -1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveIRAC45[idx], llIRAC45[idx], PSym=8
     ENDIF
     IF (cntIRAC58 GT 0) THEN BEGIN
        waveIRAC58 = (wave0Dat * waveDat)[idxIRAC58]
        fluxIRAC58 = (flux0Dat * fluxDat)[idxIRAC58]
        eFluxIRAC58 = (flux0Dat * eFluxDat)[idxIRAC58]
        ulIRAC58 = fluxIRAC58 + 3.D * eFluxIRAC58
        llIRAC58 = fluxIRAC58 - 3.D * eFluxIRAC58
        idx = Where(statDat[idxIRAC58] EQ 0L, cnt)
        IF (cnt GT 0) THEN OPlot, waveIRAC58[idx], fluxIRAC58[idx], PSym=8
        idx = Where(statDat[idxIRAC58] EQ 1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveIRAC58[idx], ulIRAC58[idx], PSym=8
        idx = Where(statDat[idxIRAC58] EQ -1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveIRAC58[idx], llIRAC58[idx], PSym=8
     ENDIF
     IF (cntIRAC80 GT 0) THEN BEGIN
        waveIRAC80 = (wave0Dat * waveDat)[idxIRAC80]
        fluxIRAC80 = (flux0Dat * fluxDat)[idxIRAC80]
        eFluxIRAC80 = (flux0Dat * eFluxDat)[idxIRAC80]
        ulIRAC80 = fluxIRAC80 + 3.D * eFluxIRAC80
        llIRAC80 = fluxIRAC80 - 3.D * eFluxIRAC80
        idx = Where(statDat[idxIRAC80] EQ 0L, cnt)
        IF (cnt GT 0) THEN OPlot, waveIRAC80[idx], fluxIRAC80[idx], PSym=8
        idx = Where(statDat[idxIRAC80] EQ 1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveIRAC80[idx], ulIRAC80[idx], PSym=8
        idx = Where(statDat[idxIRAC80] EQ -1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveIRAC80[idx], llIRAC80[idx], PSym=8
     ENDIF
  ENDIF
  IF (input.CircleMIPS NE 0) THEN BEGIN
     nameFIR6 = StrMid(nameDat, 0, 6)
     idxMIPS24 = Where((nameFIR6 EQ 'MIPS24') AND $
                       (plotDat EQ 1), cntMIPS24)
     idxMIPS70 = Where((nameFIR6 EQ 'MIPS70') AND $
                       (plotDat EQ 1), cntMIPS70)
     idxMIPS160 = Where((nameFIR6 EQ 'MIPS16') AND $
                        (plotDat EQ 1), cntMIPS160)
     PlotSym, 0, 1.5, Thick=thick, Color=FSC_Color(colors.DAT)
     IF (cntMIPS24 GT 0) THEN BEGIN
        waveMIPS24 = (wave0Dat * waveDat)[idxMIPS24]
        fluxMIPS24 = (flux0Dat * fluxDat)[idxMIPS24]
        eFluxMIPS24 = (flux0Dat * eFluxDat)[idxMIPS24]
        ulMIPS24 = fluxMIPS24 + 3.D * eFluxMIPS24
        llMIPS24 = fluxMIPS24 - 3.D * eFluxMIPS24
        idx = Where(statDat[idxMIPS24] EQ 0L, cnt)
        IF (cnt GT 0) THEN OPlot, waveMIPS24[idx], fluxMIPS24[idx], PSym=8
        idx = Where(statDat[idxMIPS24] EQ 1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveMIPS24[idx], ulMIPS24[idx], PSym=8
        idx = Where(statDat[idxMIPS24] EQ -1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveMIPS24[idx], llMIPS24[idx], PSym=8
     ENDIF
     IF (cntMIPS70 GT 0) THEN BEGIN
        waveMIPS70 = (wave0Dat * waveDat)[idxMIPS70]
        fluxMIPS70 = (flux0Dat * fluxDat)[idxMIPS70]
        eFluxMIPS70 = (flux0Dat * eFluxDat)[idxMIPS70]
        ulMIPS70 = fluxMIPS70 + 3.D * eFluxMIPS70
        llMIPS70 = fluxMIPS70 - 3.D * eFluxMIPS70
        idx = Where(statDat[idxMIPS70] EQ 0L, cnt)
        IF (cnt GT 0) THEN OPlot, waveMIPS70[idx], fluxMIPS70[idx], PSym=8
        idx = Where(statDat[idxMIPS70] EQ 1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveMIPS70[idx], ulMIPS70[idx], PSym=8
        idx = Where(statDat[idxMIPS70] EQ -1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveMIPS70[idx], llMIPS70[idx], PSym=8
     ENDIF
     IF (cntMIPS160 GT 0) THEN BEGIN
        waveMIPS160 = (wave0Dat * waveDat)[idxMIPS160]
        fluxMIPS160 = (flux0Dat * fluxDat)[idxMIPS160]
        eFluxMIPS160 = (flux0Dat * eFluxDat)[idxMIPS160]
        ulMIPS160 = fluxMIPS160 + 3.D * eFluxMIPS160
        llMIPS160 = fluxMIPS160 - 3.D * eFluxMIPS160
        idx = Where(statDat[idxMIPS160] EQ 0L, cnt)
        IF (cnt GT 0) THEN OPlot, waveMIPS160[idx], fluxMIPS160[idx], PSym=8
        idx = Where(statDat[idxMIPS160] EQ 1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveMIPS160[idx], ulMIPS160[idx], PSym=8
        idx = Where(statDat[idxMIPS160] EQ -1L, cnt)
        IF (cnt GT 0) THEN OPlot, waveMIPS160[idx], llMIPS160[idx], PSym=8
     ENDIF
  ENDIF

  ;; Overplot components.
  thickSub = 0.1D
  styleSub = 3
  idx = Where((waveFit GE xRange0[0]) AND (waveFit LE xRange0[1]))
  IF (plotLIN) THEN BEGIN
     thisFlux = (input.Plot_IRS_LINONCON EQ 1) ? $
                fluxFitCON[idx] + fluxFitLIN[idx] : fluxFitLIN[idx]
     OPlot, waveFit[idx], thisFlux, $
            Color=FSC_Color(colors.LIN), LineStyle=style.LIN, Thick=thick
     IF (input.Plot_IRS_LINONCON EQ 1) THEN BEGIN
        OPlot, waveFit[idx], fluxFitCON[idx], $
               Color=FSC_Color(colors.BCK), LineStyle=0, Thick=thick
     ENDIF
  ENDIF
  IF (plotPAH) THEN BEGIN
     IF (~input.Plot_IRS_SUBPAH) THEN BEGIN
        thisFlux = (input.Plot_IRS_PAHONCON EQ 1) ? $
                   fluxFitCON[idx] + fluxFitPAH[idx] : fluxFitPAH[idx]
        OPlot, waveFit[idx], thisFlux, $
               Color=FSC_Color(colors.PAH), LineStyle=style.PAH, Thick=thick
     ENDIF
     IF (input.Plot_IRS_SUBPAH) THEN BEGIN
        extPAH = ((SED.Cafe.Fit.Ext.PAH)[indexFit])[idx]
        drude = SED.CAFE.PAH.Drude
        nComplex = N_Elements(Uniq(drude.Complex, Sort(drude.Complex)))
        FOR i=0,nComplex-1 DO BEGIN
           idxPAH = Where(drude.Complex EQ i, cntPAH)
           IF (cntPAH GT 0) THEN BEGIN
              fPAH = extPAH * Jam_DrudeFlux(waveFit[idx], drude, Lines=idxPAH)
              thisPAH = (input.Plot_IRS_PAHONCON EQ 1) ? $
                        fluxFitCON[idx] + flux0Fit[idx]*fPAH : flux0Fit[idx]*fPAH
              OPlot, waveFit[idx], thisPAH, $
                     Color=FSC_Color(colors.PAH), LineStyle=0, Thick=thick
           ENDIF
        ENDFOR
     ENDIF
     IF (input.Plot_IRS_PAHONCON EQ 1) THEN BEGIN
        OPlot, waveFit[idx], fluxFitCON[idx], $
               Color=FSC_Color(colors.BCK), LineStyle=0, Thick=thick
     ENDIF
  ENDIF
  IF (plotSTR) THEN BEGIN
     OPlot, waveFit[idx], fluxFitSTR[idx], $
            Color=FSC_Color(colors.STR), LineStyle=style.STR, Thick=thick
  ENDIF
  IF (plotSTB) THEN BEGIN
     OPlot, waveFit[idx], fluxFitSTB[idx], $
            Color=FSC_Color(colors.STB), LineStyle=style.STB, Thick=thick
     alignment = (input.Plot_SED_YAXS EQ 3) ? 0 : 1
     CASE (xAxisType) OF
        0: idxICE = Round(Interpol(LIndGen(N_Elements(idx)), $
                                   waveFit[idx], 3.D))
        1: idxICE = Round(Interpol(LIndGen(N_Elements(idx)), $
                                   3D14 / waveFit[idx], 3.D))
     ENDCASE
     IF ((plotSTB_100) AND $
         (Max(fluxFitSTB_100[idx]) GT yRangeFlux[0])) THEN BEGIN
        OPlot, waveFit[idx], fluxFitSTB_100[idx], $
               Color=FSC_Color(colors.STB), LineStyle=2, Thick=thick
        XYOutS, (waveFit[idx])[idxICE], (fluxFitSTB_100[idx])[idxICE], $
                '100 Myr', Alignment=alignment, /Data, Color=FSC_Color(colors.AXS)
     ENDIF
     IF ((plotSTB_010) AND $
         (Max(fluxFitSTB_010[idx]) GT yRangeFlux[0])) THEN BEGIN
        OPlot, waveFit[idx], fluxFitSTB_010[idx], $
               Color=FSC_Color(colors.STB), LineStyle=2, Thick=thick
        XYOutS, (waveFit[idx])[idxICE], (fluxFitSTB_010[idx])[idxICE], $
                '10 Myr', Alignment=alignment, /Data, Color=FSC_Color(colors.AXS)
     ENDIF
     IF ((plotSTB_002) AND $
         (Max(fluxFitSTB_002[idx]) GT yRangeFlux[0])) THEN BEGIN
        OPlot, waveFit[idx], fluxFitSTB_002[idx], $
               Color=FSC_Color(colors.STB), LineStyle=2, Thick=thick
        XYOutS, (waveFit[idx])[idxICE], (fluxFitSTB_002[idx])[idxICE], $
                '2 Myr', Alignment=alignment, /Data, Color=FSC_Color(colors.AXS)
     ENDIF
  ENDIF
  IF (plotDSK) THEN BEGIN
     OPlot, waveFit[idx], fluxFitDSK[idx], $
            Color=FSC_Color(colors.DSK), LineStyle=style.DSK, Thick=thick
  ENDIF
  IF (plotCIR) THEN BEGIN
     OPlot, waveFit[idx], fluxFitCIR[idx], $
            Color=FSC_Color(colors.CIR), LineStyle=style.CIR, Thick=thick
  ENDIF
  IF (plotCLD) THEN BEGIN
     OPlot, waveFit[idx], fluxFitCLD[idx], $
            Color=FSC_Color(colors.CLD), LineStyle=style.CLD, Thick=thick
  ENDIF
  IF (plotCOO) THEN BEGIN
     OPlot, waveFit[idx], fluxFitCOO[idx], $
            Color=FSC_Color(colors.COO), LineStyle=style.COO, Thick=thick
  ENDIF
  IF (plotWRM) THEN BEGIN
     OPlot, waveFit[idx], fluxFitWRM[idx], $
            Color=FSC_Color(colors.WRM), LineStyle=style.WRM, Thick=thick
  ENDIF
  IF (plotHOT) THEN BEGIN
     OPlot, waveFit[idx], fluxFitHOT[idx], $
            Color=FSC_Color(colors.HOT), LineStyle=style.HOT, Thick=thick
  ENDIF

  ;; Overplot continua.
  IF (plotCON) THEN BEGIN
     OPlot, waveFit[idx], fluxFitCON[idx], $
            Color=FSC_Color(colors.CON), LineStyle=style.CON, Thick=2*thick
  ENDIF

  ;; Overplot 'Extra' fit.
  IF (doOPlot) THEN BEGIN
     idxIRS = Where((waveFit GE waveMaxIRS) AND (waveFit LE xRange0[1]))
     OPlot, waveFit[idxIRS], fluxFitOPlot[idxIRS], $
            Color=FSC_Color(colors.FIT), Thick=thick, LineStyle=2
  ENDIF

  ;; Overplot fit.
  IF (plotFit) THEN BEGIN
     OPlot, waveFit[idx], fluxFit[idx], $
            Color=FSC_Color(colors.FIT), Thick=thick, LineStyle=style.Fit
  ENDIF

  ;; Annotate silicates.
  IF ((Keyword_Set(IRS)) AND (input.Label_IRS_SIL)) THEN BEGIN
     c1 = Convert_Coord(7.8D, yRange[0], /Data, /To_Normal)
     c2 = Convert_Coord(13.8D, yRange[0], /Data, /To_Normal)
     c3 = Convert_Coord(14.1D, yRange[0], /Data, /To_Normal)
     c4 = Convert_Coord(23.D, yRange[0], /Data, /To_Normal)
     yScl = 0.03
     dy = 0.01
     ;; 9.7um
     IF ((xRange[0] LE 7.7D) AND (xRange[1] GE 13.9D)) THEN BEGIN
        PlotS, [c1[0], c2[0]], yScl + [c1[1], c2[1]], $
               Thick=thick, Color=FSC_Color(colors.DAT), /Normal
        PlotS, [c1[0], c1[0]], [yScl, yScl+dy] + c1[1], $
               Thick=thick, Color=FSC_Color(colors.DAT), /Normal
        PlotS, [c2[0], c2[0]], [yScl, yScl+dy] + c2[1], $
               Thick=thick, Color=FSC_Color(colors.DAT), /Normal
        XYOutS, 0.5 * [c1[0] + c2[0]], yScl + dy + c1[1], $
                TeXtoIDL('9.7 \mum') + ' Silicates', $
                Color=FSC_Color(colors.DAT), Alignment=0.5, CharSize=cs, /Normal
     ENDIF
     ;; 18um
     IF ((xRange[0] LE 14.D) AND (xRange[1] GE 23.1D)) THEN BEGIN
        PlotS, [c3[0], c4[0]], yScl + [c3[1], c4[1]], $
               Thick=thick, Color=FSC_Color(colors.DAT), /Normal
        PlotS, [c3[0], c3[0]], [yScl, yScl+dy] + c3[1], $
               Thick=thick, Color=FSC_Color(colors.DAT), /Normal
        PlotS, [c4[0], c4[0]], [yScl, yScl+dy] + c4[1], $
               Thick=thick, Color=FSC_Color(colors.DAT), /Normal
        XYOutS, 0.5 * [c3[0] + c4[0]], yScl + dy + c3[1], $
                TeXtoIDL('18 \mum') + ' Silicates', $
                Color=FSC_Color(colors.DAT), Alignment=0.5, CharSize=cs, /Normal
     ENDIF
  ENDIF

  ;; Annotate lines.
  IF ((Keyword_Set(IRS)) AND (input.Label_IRS_LIN EQ 1)) THEN BEGIN
     IF (N_Elements(lines) EQ 0) THEN BEGIN
        linesFile = File_Which('cafe.lines.txt')
        lines = Jam_ReadData(linesFile)
     ENDIF $
     ELSE BEGIN
        IF (Jam_Check(lines, TName='STRING')) THEN $
           lines = Jam_ReadData(lines)
     ENDELSE
     name = lines.Name
     wave0 = lines.Wave
     show = lines.Show
     N_LINE = N_Elements(name)
     FOR i=0,N_LINE-1 DO BEGIN

        ;; Continue if line is in IRS spectrum.
        IF ((wave0[i] GT Min(waveDat)) AND $
            (wave0[i] LT Max(waveDat))) THEN BEGIN

           ;; Find peak wavelength.
           x0 = wave0[i]

           ;; Calculate line SNR.
           lineWave0 = SED.Cafe.LIN.Data.Wave0
           idxLIN = Where((lineWave0 GT 0.995*x0) AND $
                          (lineWave0 LT 1.005*x0), cntLIN)
           SNR_x0 = (cntLIN EQ 1) ? $
                    (SED.Cafe.LIN.Data.Power)[idxLIN] / $
                    (SED.Cafe.LIN.Data.ErrPower)[idxLIN] : $
                    0.D

           ;; Check if fit to line peak was pegged.
           isPegged = ((SED.Cafe.LIN.ErrGauss.Peak)[idxLIN] EQ 0.D)

           ;; Check if POOR_FIT_FLAG is set for line.
           isPoorFit = ((SED.Cafe.LIN.Gauss.POOR_FIT_FLAG)[idxLIN] EQ 2)

           ;; Continue if EQW and SNR are high enough (auto mode) or if 'Show' is
           ;; explicitly set.
           IF (((lines.Auto EQ 1) AND $
                (isPegged EQ 0) AND $
                (isPoorFit EQ 0) AND $
                (SNR_x0 GT lines.Min_SNR) AND $
                (show[i] EQ 1)) OR $
               ((lines.Auto EQ 0) AND $
                (show[i] EQ 1))) THEN BEGIN

              ;; Calculate peak flux.
              y0 = Max([Interpol(fluxDat, waveDat, x0), $
                        Interpol(fluxFit, waveFit, x0)])
              cont0 = Interpol(fluxFitCON, waveFit, x0)

              ;; Calculate y-offsets, etc.
              offset0 = lines.Offset + (lines.Nudge)[i]
              dOffset0 = offset0 / 5.D
              yMin = ((lines.Pos)[i] EQ 1) ? $
                     (1.D + dOffset0) * y0 : $
                     (1.D - offset0 + dOffset0) * y0
              yMax = ((lines.Pos)[i] EQ 1) ? $
                     (1.D + offset0 - dOffset0) * y0 : 0.95 * cont0

              ;; Draw reference line.
              IF (lines.Draw_Line EQ 1) THEN BEGIN
                 OPlot, [x0, x0], [yMin, yMax], Color=FSC_Color(colors.DAT), $
                        Thick=thick, LineStyle=lines.Line_Style
              ENDIF

              ;; Label line.
              yLabel = ((lines.Pos)[i] EQ 1) ? $
                       (1.D + offset0) * y0 : $
                       (1.D - 0.95D * offset0) * y0
              XYOutS, x0, yLabel, TeXtoIDL(name[i]), Alignment=0.5, $
                      Color=FSC_Color(colors.DAT)

           ENDIF

        ENDIF

     ENDFOR
  ENDIF

  ;; Re-plot axes to repair any damage.
  IF (~Keyword_Set(P_Multi)) THEN BEGIN
     IF ((Keyword_Set(IRS)) AND (xLog EQ 1) AND (xAxisType EQ 0)) THEN BEGIN
        xTickV = [1.D + DIndGen(10), 15.D, 20.D, 25.D, 30.D, 35.D]
        IF (xAxisType EQ 1) THEN xTickV = 3D14 / xTickV
        idx = Where((xTickV GE xRange[0]) AND (xTickV LE xRange[1]))
        xTickV = xTickV[idx]
        xTicks = N_Elements(xTickV) - 1
        Plot, [1], [1], /NoData, /NoErase, $
              XLog=xLog, XStyle=xStyle, XRange=xRange, $
              YLog=yLog, YStyle=1, YRange=yRange, $
              Position=position, Background=FSC_Color(colors.BCK), $
              Color=FSC_Color(colors.AXS), $
              XCharSize=xCharSize, Thick=thick, XThick=thick, YThick=thick, $
              XTicks=xTicks, XTickV=xTickV, CharSize=charSize, _Extra=extra
     ENDIF $
     ELSE BEGIN
        IF (~Keyword_Set(IRS)) THEN yTickFormat2 = 'Cafe_YTickFormat'
        Plot, [1], [1], /NoData, /NoErase, $
              XLog=xLog, XStyle=xStyle, XRange=xRange, $
              YLog=yLog, YStyle=1, YRange=yRange, $
              Position=position, Background=FSC_Color(colors.BCK), $
              Color=FSC_Color(colors.AXS), $
              XCharSize=xCharSize, Thick=thick, XThick=thick, $
              YThick=thick, CharSize=charSize, YTickFormat=yTickFormat2, $
              _Extra=extra
     ENDELSE
     IF (xALT EQ 1) THEN BEGIN
        xRangeAlt = 3D14 / xRange
        Axis, XAxis=1, XLog=xLog, XStyle=1, $
              XRange=xRangeAlt, Color=FSC_Color(colors.AXS), XThick=thick, $
              YThick=thick, CharSize=charSize, /NoErase
     ENDIF
  ENDIF

  RETURN, 1L

END

;;+ ===================================================================
; NAME:
; CAFE_PLOT_RES
;
; PURPOSE:
; This routine produces residual plots.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

PRO Cafe_Plot_Res, SED, colors, input, CRC=CRC, IRS=IRS, Thick=thick, $
                   PS=PS, XRange=xRange

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Calculate all residuals.
  idx = Where(SED.Cafe.Fit.Flux GT 0.D)
  res = SED.Cafe.DAT.Flux - $
        Interpol((SED.Cafe.FIT.Flux)[idx], $
                 ALog((SED.Cafe.FIT.Wave)[idx]), $
                 ALog(SED.Cafe.DAT.Wave))

  ;; Calculate synthetic photometry residuals.
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (Jam_Check(SED.Cafe.SynPhot, $
                   TName='STRUCT', Tag_Name='Flux')) THEN BEGIN
        synFlux = SED.Cafe.SynPhot.Flux
        synName = Tag_Names(synFlux)
        FOR i=0,N_Elements(SED.Cafe.DAT.Filter)-1 DO BEGIN
           idxSyn = Where(synName EQ (SED.Cafe.DAT.Filter)[i], cntSyn)
           IF (cntSyn EQ 1) THEN $
              res[i] = (SED.Cafe.DAT.Flux)[i] - synFlux.(idxSyn)
        ENDFOR
     ENDIF
  ENDIF

  ;; Calculate IRS residuals.
  indexIRS = SED.Cafe.DAT.Index.IRS
  doIRS = (N_Elements(indexIRS) GT 10) ? 1L : 0L
  IF (doIRS) THEN BEGIN
     waveIRS = (SED.Cafe.DAT.Wave)[indexIRS]
     sigmaIRS = (SED.Cafe.DAT.Sigma)[indexIRS]
     fitIRS = (SED.Cafe.DAT.Fit)[indexIRS]
     resIRS = res[indexIRS] / (sigmaIRS > 1D-30)
  ENDIF

  ;; Calculate OPT residuals.
  indexOPT = SED.Cafe.DAT.Index.OPT
  cntOPT = 0
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (indexOPT[0] NE -1L) THEN BEGIN
        waveOPT = (SED.Cafe.DAT.Wave)[indexOPT]
        sigmaOPT = (SED.Cafe.DAT.Sigma)[indexOPT]
        resOPT = res[indexOPT] / sigmaOPT
        statOPT = (SED.Cafe.DAT.Stat)[indexOPT]
        fitOPT = (SED.Cafe.DAT.Fit)[indexOPT]
        plotOPT = (SED.Cafe.DAT.Plot)[indexOPT]
        idx = Where((statOPT EQ 0.D) AND $
                    (plotOPT EQ 1L) AND $
                    (fitOPT EQ 1L), cntOPT)
        IF (cntOPT GT 0) THEN BEGIN
           waveOPT = waveOPT[idx]
           resOPT = resOPT[idx]
           plotOPT = plotOPT[idx]
        ENDIF
     ENDIF
  ENDIF

  ;; Calculate NIR residuals.
  indexNIR = SED.Cafe.DAT.Index.NIR
  cntNIR = 0
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (indexNIR[0] NE -1L) THEN BEGIN
        nameNIR = (SED.Cafe.DAT.Name)[indexNIR]
        waveNIR = (SED.Cafe.DAT.Wave)[indexNIR]
        sigmaNIR = (SED.Cafe.DAT.Sigma)[indexNIR]
        resNIR = res[indexNIR] / sigmaNIR
        statNIR = (SED.Cafe.DAT.Stat)[indexNIR]
        fitNIR = (SED.Cafe.DAT.Fit)[indexNIR]
        plotNIR = (SED.Cafe.DAT.Plot)[indexNIR]
        idx = Where((statNIR EQ 0.D) AND $
                    (plotNIR EQ 1L) AND $
                    (fitNIR EQ 1L), cntNIR)
        IF (cntNIR GT 0) THEN BEGIN
           nameNIR = nameNIR[idx]
           waveNIR = waveNIR[idx]
           resNIR = resNIR[idx]
           plotNIR = plotNIR[idx]
        ENDIF
     ENDIF
  ENDIF

  ;; Calculate MIR residuals.
  indexMIR = SED.Cafe.DAT.Index.MIR
  cntMIR = 0
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (indexMIR[0] NE -1L) THEN BEGIN
        waveMIR = (SED.Cafe.DAT.Wave)[indexMIR]
        sigmaMIR = (SED.Cafe.DAT.Sigma)[indexMIR]
        resMIR = res[indexMIR] / sigmaMIR
        statMIR = (SED.Cafe.DAT.Stat)[indexMIR]
        fitMIR = (SED.Cafe.DAT.Fit)[indexMIR]
        plotMIR = (SED.Cafe.DAT.Plot)[indexMIR]
        idx = Where((statMIR EQ 0.D) AND $
                    (plotMIR EQ 1L) AND $
                    (fitMIR EQ 1L), cntMIR)
        IF (cntMIR GT 0) THEN BEGIN
           waveMIR = waveMIR[idx]
           resMIR = resMIR[idx]
           plotMIR = plotMIR[idx]
        ENDIF
     ENDIF
  ENDIF

  ;; Calculate FIR residuals.
  indexFIR = SED.Cafe.DAT.Index.FIR
  cntFIR = 0
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (indexFIR[0] NE -1L) THEN BEGIN
        nameFIR = (SED.Cafe.DAT.Name)[indexFIR]
        waveFIR = (SED.Cafe.DAT.Wave)[indexFIR]
        sigmaFIR = (SED.Cafe.DAT.Sigma)[indexFIR]
        resFIR = res[indexFIR] / sigmaFIR
        statFIR = (SED.Cafe.DAT.Stat)[indexFIR]
        fitFIR = (SED.Cafe.DAT.Fit)[indexFIR]
        plotFIR = (SED.Cafe.DAT.Plot)[indexFIR]
        idx = Where((statFIR EQ 0.D) AND $
                    (plotFIR EQ 1L) AND $
                    (fitFIR EQ 1L), cntFIR)
        IF (cntFIR GT 0) THEN BEGIN
           nameFIR = nameFIR[idx]
           waveFIR = waveFIR[idx]
           resFIR = resFIR[idx]
           plotFIR = plotFIR[idx]
        ENDIF
     ENDIF
  ENDIF

  ;; Calculate RAD residuals.
  indexRAD = SED.Cafe.DAT.Index.RAD
  cntRAD = 0
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (indexRAD[0] NE -1L) THEN BEGIN
        nameRAD = (SED.Cafe.DAT.Name)[indexRAD]
        waveRAD = (SED.Cafe.DAT.Wave)[indexRAD]
        sigmaRAD = (SED.Cafe.DAT.Sigma)[indexRAD]
        resRAD = res[indexRAD] / sigmaRAD
        statRAD = (SED.Cafe.DAT.Stat)[indexRAD]
        fitRAD = (SED.Cafe.DAT.Fit)[indexRAD]
        plotRAD = (SED.Cafe.DAT.Plot)[indexRAD]
        idx = Where((statRAD EQ 0.D) AND $
                    (plotRAD EQ 1L) AND $
                    (fitRAD EQ 1L), cntRAD)
        IF (cntRAD GT 0) THEN BEGIN
           waveRAD = waveRAD[idx]
           resRAD = resRAD[idx]
           plotRAD = plotRAD[idx]
        ENDIF
     ENDIF
  ENDIF

  ;; Get axis types.
  xAxisType = (Keyword_Set(IRS)) ? input.Plot_IRS_XAXS : input.Plot_SED_XAXS

  ;; Set multiplicative wave/freq factor.
  IF (doIRS) THEN BEGIN
     CASE (xAxisType) OF
        0: wave0IRS = Replicate(1.D, N_Elements(waveIRS))
        1: wave0IRS = 3D14 / waveIRS^2 ;; [um->Hz]
     ENDCASE
  ENDIF
  IF (N_Elements(waveOPT) NE 0) THEN BEGIN
     CASE (xAxisType) OF
        0: wave0OPT = Replicate(1.D, N_Elements(waveOPT))
        1: wave0OPT = 3D14 / waveOPT^2 ;; [um->Hz]
     ENDCASE
  ENDIF
  IF (N_Elements(waveNIR) NE 0) THEN BEGIN
     CASE (xAxisType) OF
        0: wave0NIR = Replicate(1.D, N_Elements(waveNIR))
        1: wave0NIR = 3D14 / waveNIR^2 ;; [um->Hz]
     ENDCASE
  ENDIF
  IF (N_Elements(waveMIR) NE 0) THEN BEGIN
     CASE (xAxisType) OF
        0: wave0MIR = Replicate(1.D, N_Elements(waveMIR))
        1: wave0MIR = 3D14 / waveMIR^2 ;; [um->Hz]
     ENDCASE
  ENDIF
  IF (N_Elements(waveFIR) NE 0) THEN BEGIN
     CASE (xAxisType) OF
        0: wave0FIR = Replicate(1.D, N_Elements(waveFIR))
        1: wave0FIR = 3D14 / waveFIR^2 ;; [um->Hz]
     ENDCASE
  ENDIF
  IF (N_Elements(waveRAD) NE 0) THEN BEGIN
     CASE (xAxisType) OF
        0: wave0RAD = Replicate(1.D, N_Elements(waveRAD))
        1: wave0RAD = 3D14 / waveRAD^2 ;; [um->Hz]
     ENDCASE
  ENDIF
  resPhot = [0.D]
  IF (cntOPT GT 0) THEN BEGIN
     waveOPT = wave0OPT * waveOPT
     resPhot = [resPhot, resOPT]
  ENDIF
  IF (cntNIR GT 0) THEN BEGIN
     waveNIR = wave0NIR * waveNIR
     resPhot = [resPhot, resNIR]
  ENDIF
  IF (cntMIR GT 0) THEN BEGIN
     waveMIR = wave0MIR * waveMIR
     resPhot = [resPhot, resMIR]
  ENDIF
  IF (cntFIR GT 0) THEN BEGIN
     waveFIR = wave0FIR * waveFIR
     resPhot = [resPhot, resFIR]
  ENDIF
  IF (cntRAD GT 0) THEN BEGIN
     waveRAD = wave0RAD * waveRAD
     resPhot = [resPhot, resRAD]
  ENDIF

  ;; Sort data.
  IF (doIRS) THEN BEGIN
     idxDatIRS = Sort(wave0IRS * waveIRS)
     waveIRS = (wave0IRS * waveIRS)[idxDatIRS]
     resIRS = resIRS[idxDatIRS]
  ENDIF

  ;; Set residuals range.
  IF ((Keyword_Set(IRS)) AND (doIRS)) THEN BEGIN
     idxFit = Where(fitIRS EQ 1L)
     idx = Sort(resIRS[idxFit])
     cnt = N_Elements(idx)
     CLIP_FRACTION = 0.05D
     N_SIGMA = Ceil((((resIRS[idxFit])[idx])[Floor((1.D - $
                                                    CLIP_FRACTION) * cnt)])[0])
  ENDIF ELSE $
     N_SIGMA = Ceil(Max(Abs(resPhot)))
  yRange0 = [-N_SIGMA, N_SIGMA]
  dy = 0.1D
  yRange = (1.D + dy) * yRange0

  ;; Set plot position.
  IF (~Keyword_Set(CRC)) THEN BEGIN
     position = (Keyword_Set(PS)) ? $
                [0.08, 0.1, 0.7, 0.3] : $ ;; 0.04
                [0.09, 0.1, 0.7, 0.3]
  ENDIF

  ;; Set x-title.
  CASE (xAxisType) OF
     0: xTitle = TeXtoIDL('\lambda_{rest} (\mum)')
     1: xTitle = TeXtoIDL('\nu_{rest} (Hz)')
  ENDCASE

  ;; Plot axes.
  IF (~Keyword_Set(CRC)) THEN noErase = 1
  xStyle = (~Keyword_Set(CRC)) ? 9 : 1
  xLog = (Keyword_Set(IRS)) ? input.Plot_IRS_XLOG : input.Plot_SED_XLOG
  yTitle = TeXtoIDL('f_\nu^{data} - f_\nu^{total} (\sigma)')
  yMargin = (Keyword_Set(CRC)) ? [12, 12] : [4, 2]
  charSize = (Keyword_Set(PS)) ? 1.D : 1.2D
  IF ((Keyword_Set(IRS)) AND (xLog EQ 1) AND (xAxisType EQ 0)) THEN BEGIN
     xTickV = [1.D + DIndGen(10), 15.D, 20.D, 25.D, 30.D, 35.D]
     IF (xAxisType EQ 1) THEN xTickV = 3D14 / xTickV
     idx = Where((xTickV GE xRange[0]) AND (xTickV LE xRange[1]))
     xTickV = xTickV[idx]
     xTicks = N_Elements(xTickV) - 1
     Plot, [1], [1], /NoData, $
           XLog=xLog, XStyle=xStyle, XRange=xRange, XTitle=xTitle, $
           YMargin=yMargin, YStyle=1, YRange=yRange, YTitle=yTitle, $
           Position=position, Color=FSC_Color(colors.AXS), NoErase=noErase, $
           Thick=thick, XThick=thick, YThick=thick, $
           XTicks=xTicks, XTickV=xTickV, CharSize=charSize
  ENDIF $
  ELSE BEGIN
     Plot, [1], [1], /NoData, $
           XLog=xLog, XStyle=xStyle, XRange=xRange, XTitle=xTitle, $
           YMargin=yMargin, YStyle=1, YRange=yRange, YTitle=yTitle, $
           Position=position, Color=FSC_Color(colors.AXS), NoErase=noErase, $
           Thick=thick, XThick=thick, YThick=thick, CharSize=charSize
  ENDELSE

  ;; Re-plot axis to get correct line weight.
  IF ((Keyword_Set(IRS)) AND (xLog EQ 1) AND (xAxisType EQ 0)) THEN BEGIN
     xTickV = [1.D + DIndGen(10), 15.D, 20.D, 25.D, 30.D, 35.D]
     IF (xAxisType EQ 1) THEN xTickV = 3D14 / xTickV
     idx = Where((xTickV GE xRange[0]) AND (xTickV LE xRange[1]))
     xTickV = xTickV[idx]
     xTicks = N_Elements(xTickV) - 1
     Plot, [1], [1], /NoData, /NoErase, $
           XLog=xLog, XStyle=xStyle, XRange=xRange, $
           YMargin=yMargin, YStyle=1, YRange=yRange, $
           Position=position, Color=FSC_Color(colors.AXS), Thick=thick, $
           XThick=thick, YThick=thick, XTicks=xTicks, XTickV=xTickV, $
           CharSize=charSize
  ENDIF $
  ELSE BEGIN
     Plot, [1], [1], /NoData, /NoErase, $
           XLog=xLog, XStyle=xStyle, XRange=xRange, $
           YMargin=yMargin, YStyle=1, YRange=yRange, $
           Position=position, Color=FSC_Color(colors.AXS), Thick=thick, $
           XThick=thick, YThick=thick, CharSize=charSize
  ENDELSE

  ;; Overplot IRS residuals.
  IF (doIRS) THEN OPlot, waveIRS, resIRS, Color=FSC_Color(colors.DAT), Thick=thick

  ;; Overplot OPT residuals.
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (cntOPT GT 0) THEN BEGIN
        PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT)
        index = Where((resOPT GE yRange0[0]) AND $
                      (resOPT LE yRange0[1]), countOPT)
        IF (countOPT GT 0) THEN PlotS, waveOPT[index], resOPT[index], PSym=8
     ENDIF
  ENDIF

  ;; Overplot NIR residuals.
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (cntNIR GT 0) THEN BEGIN
        PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT)
        index = Where((resNIR GE yRange0[0]) AND $
                      (resNIR LE yRange0[1]), countNIR)
        IF (countNIR GT 0) THEN PlotS, waveNIR[index], resNIR[index], PSym=8
     ENDIF
  ENDIF

  ;; Overplot MIR residuals.
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (cntMIR GT 0) THEN BEGIN
        PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT)
        index = Where((resMIR GE yRange0[0]) AND $
                      (resMIR LE yRange0[1]), countMIR)
        IF (countMIR GT 0) THEN PlotS, waveMIR[index], resMIR[index], PSym=8
     ENDIF
  ENDIF

  ;; Overplot FIR residuals.
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (cntFIR GT 0) THEN BEGIN
        PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT)
        index = Where((resFIR GE yRange0[0]) AND $
                      (resFIR LE yRange0[1]), countFIR)
        IF (countFIR GT 0) THEN PlotS, waveFIR[index], resFIR[index], PSym=8
     ENDIF
  ENDIF

  ;; Overplot RAD residuals.
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (cntRAD GT 0) THEN BEGIN
        PlotSym, 0, /Fill, Color=FSC_Color(colors.DAT)
        index = Where((resRAD GE yRange0[0]) AND $
                      (resRAD LE yRange0[1]), countRAD)
        IF (countRAD GT 0) THEN PlotS, waveRAD[index], resRAD[index], PSym=8
     ENDIF
  ENDIF

  ;; Mark IRAC and MIPS data.
  IF (~Keyword_Set(IRS)) THEN BEGIN
     IF (input.CircleIRAC) THEN BEGIN
        nameIRAC = SED.Cafe.Dat.Name
        plotIRAC = SED.Cafe.Dat.Plot
        fitIRAC = SED.Cafe.Dat.Fit
        statIRAC = SED.Cafe.Dat.Stat
        wave = SED.Cafe.Dat.Wave
        sigma = SED.Cafe.Dat.Sigma
        nameIRAC5 = StrMid(nameIRAC, 0, 5)
        idxIRAC36 = Where((nameIRAC5 EQ 'IRAC1') AND (statIRAC EQ 0) AND $
                          (fitIRAC EQ 1) AND (plotIRAC EQ 1), cntIRAC36)
        idxIRAC45 = Where((nameIRAC5 EQ 'IRAC2') AND (statIRAC EQ 0) AND $
                          (fitIRAC EQ 1) AND (plotIRAC EQ 1), cntIRAC45)
        idxIRAC58 = Where((nameIRAC5 EQ 'IRAC3') AND (statIRAC EQ 0) AND $
                          (fitIRAC EQ 1) AND (plotIRAC EQ 1), cntIRAC58)
        idxIRAC80 = Where((nameIRAC5 EQ 'IRAC4') AND (statIRAC EQ 0) AND $
                          (fitIRAC EQ 1) AND (plotIRAC EQ 1), cntIRAC80)
        PlotSym, 0, 1.5, Thick=thick, Color=FSC_Color(colors.DAT)
        IF (cntIRAC36 EQ 1) THEN $
           PlotS, wave[idxIRAC36], res[idxIRAC36] / sigma[idxIRAC36], PSym=8
        IF (cntIRAC45 EQ 1) THEN $
           PlotS, wave[idxIRAC45], res[idxIRAC45] / sigma[idxIRAC45], PSym=8
        IF (cntIRAC58 EQ 1) THEN $
           PlotS, wave[idxIRAC58], res[idxIRAC58] / sigma[idxIRAC58], PSym=8
        IF (cntIRAC80 EQ 1) THEN $
           PlotS, wave[idxIRAC80], res[idxIRAC80] / sigma[idxIRAC80], PSym=8
     ENDIF
     IF (input.CircleMIPS) THEN BEGIN
        nameMIPS = SED.Cafe.Dat.Name
        plotMIPS = SED.Cafe.Dat.Plot
        fitMIPS = SED.Cafe.Dat.Fit
        statMIPS = SED.Cafe.Dat.Fit
        wave = SED.Cafe.Dat.Wave
        sigma = SED.Cafe.Dat.Sigma
        nameMIPS6 = StrMid(nameMIPS, 0, 6)
        idxMIPS24 = Where((nameMIPS6 EQ 'MIPS24') AND (statMIPS EQ 0) AND $
                          (fitMIPS EQ 1) AND (plotMIPS EQ 1), cntMIPS24)
        idxMIPS70 = Where((nameMIPS6 EQ 'MIPS70') AND (statMIPS EQ 0) AND $
                          (fitMIPS EQ 1) AND (plotMIPS EQ 1), cntMIPS70)
        idxMIPS160 = Where((nameMIPS6 EQ 'MIPS16') AND (statMIPS EQ 0) AND $
                           (fitMIPS EQ 1) AND (plotMIPS EQ 1), cntMIPS160)
        PlotSym, 0, 1.5, Thick=thick, Color=FSC_Color(colors.DAT)
        IF (cntMIPS24 EQ 1) THEN $
           PlotS, wave[idxMIPS24], res[idxMIPS24] / sigma[idxMIPS24], PSym=8
        IF (cntMIPS70 EQ 1) THEN $
           PlotS, wave[idxMIPS70], res[idxMIPS70] / sigma[idxMIPS70], PSym=8
        IF (cntMIPS160 EQ 1) THEN $
           PlotS, wave[idxMIPS160], res[idxMIPS160] / sigma[idxMIPS160], PSym=8
     ENDIF
  ENDIF

  ;; Overplot zero residual line.
  PlotS, [xRange[0], xRange[1]], [0.D, 0.D], Color=FSC_Color(colors.AXS), $
         LineStyle=2

  RETURN

END

;;+ ===================================================================
; NAME:
; CAFE_PLOT_LEG
;
; PURPOSE:
; This routine plots the legend.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

PRO Cafe_Plot_Leg, SED, colors, style, input, PS=PS, CRC=CRC, $
                         Thick=thick, IRS=IRS, YRange=yRange, $
                         SubTitle=subTitle

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Model parameters.
  p = Cafe_Params(SED.Cafe.Params.Values)

  ;; Set flux range.
  indexDat = (Keyword_Set(IRS)) ? $
             SED.Cafe.DAT.Index.IRS : $
             LIndGen(N_Elements(SED.Cafe.DAT.Wave))
  fluxDat = (SED.Cafe.DAT.Flux)[indexDat]
  plotDat = (SED.Cafe.DAT.Plot)[indexDat]

  ;; Get plot options.
  plotLIN = (Keyword_Set(IRS)) ? input.Plot_IRS_LIN : input.Plot_SED_LIN
  plotPAH = (Keyword_Set(IRS)) ? input.Plot_IRS_PAH : input.Plot_SED_PAH
  plotSTR = (Keyword_Set(IRS)) ? input.Plot_IRS_STR : input.Plot_SED_STR
  plotSTB = (Keyword_Set(IRS)) ? input.Plot_IRS_STB : input.Plot_SED_STB
  plotDSK = (Keyword_Set(IRS)) ? input.Plot_IRS_DSK : input.Plot_SED_DSK
  plotCIR = (Keyword_Set(IRS)) ? input.Plot_IRS_CIR : input.Plot_SED_CIR
  plotCLD = (Keyword_Set(IRS)) ? input.Plot_IRS_CLD : input.Plot_SED_CLD
  plotCOO = (Keyword_Set(IRS)) ? input.Plot_IRS_COO : input.Plot_SED_COO
  plotWRM = (Keyword_Set(IRS)) ? input.Plot_IRS_WRM : input.Plot_SED_WRM
  plotHOT = (Keyword_Set(IRS)) ? input.Plot_IRS_HOT : input.Plot_SED_HOT
  plotCON = (Keyword_Set(IRS)) ? input.Plot_IRS_CON : input.Plot_SED_CON

  ;; Get axis type.
  xAxisType = (Keyword_Set(IRS)) ? input.Plot_IRS_XAXS : input.Plot_SED_XAXS
  yAxisType = (Keyword_Set(IRS)) ? input.Plot_IRS_YAXS : input.Plot_SED_YAXS

  ;; Figure out legend position.
  altLeg = (Keyword_Set(IRS)) ? input.Alt_IRS_Leg : input.Alt_SED_Leg
  IF (xAxisType EQ 0) THEN BEGIN ;; [um]
     IF (yAxisType EQ 2) THEN BEGIN ;; [erg s-1 cm-2 um-1]
        pos = (altLeg EQ 1) ? 'L' : 'R'
     ENDIF $
     ELSE BEGIN ;; [Jy] -or- [erg s-a cm-s]
        pos = (altLeg EQ 1) ? 'R' : 'L'
     ENDELSE
  ENDIF $
  ELSE BEGIN ;; [Hz]
     IF (yAxisType EQ 2) THEN BEGIN ;; [erg s-1 cm-2 um-1]
        pos = (altLeg EQ 1) ? 'R' : 'L'
     ENDIF $
     ELSE BEGIN ;; [Jy] -or- [erg s-1 cm-1]
        pos = (altLeg EQ 1) ? 'L' : 'R'
     ENDELSE
  ENDELSE

  ;; Set position of legend for all cases.
  xALT = (Keyword_Set(IRS)) ? input.Plot_IRS_XALT : input.Plot_SED_XALT
  IF (pos EQ 'L') THEN BEGIN
     IF (~Keyword_Set(CRC)) THEN BEGIN
        IF (xALT EQ 1) THEN BEGIN
           position = (Keyword_Set(PS)) ? [0.05, 0.87] : [0.10, 0.89]
        ENDIF $
        ELSE BEGIN
           position = (Keyword_Set(PS)) ? [0.10, 0.89] : [0.10, 0.91]
        ENDELSE
     ENDIF $
     ELSE BEGIN
        position = (xALT EQ 1) ? [0.14, 0.81] : [0.13, 0.89]
     ENDELSE
  ENDIF $
  ELSE BEGIN
     IF (~Keyword_Set(CRC)) THEN BEGIN
        IF (xALT EQ 1) THEN BEGIN
           position = (Keyword_Set(PS)) ? [0.53, 0.88] : [0.55, 0.88]
        ENDIF $
        ELSE BEGIN
           position = (Keyword_Set(PS)) ? [0.52, 0.90] : [0.55, 0.91]
        ENDELSE
     ENDIF $
     ELSE BEGIN
        position = (xALT EQ 1) ? [0.70, 0.81] : [0.75, 0.88]
     ENDELSE
  ENDELSE

  components =  ['Total']
  colorList = [FSC_Color(colors.FIT)]
  lineStyle = [style.Fit]
  thickArray = [thick]

  idx = (Keyword_Set(IRS)) ? $
        SED.Cafe.Fit.Index.IRS : LIndGen(N_Elements(SED.Cafe.Fit.Wave))

  IF ((plotCIR) AND $
      (Max((SED.Cafe.Fit.Com.CIR)[idx]) GT yRange[0])) THEN BEGIN
     components = [components, input.Label_CIR]
     colorList = [colorList, FSC_Color(colors.CIR)]
     lineStyle = [lineStyle, style.CIR]
     thickArray = [thickArray, thick]
  ENDIF

  IF ((plotCLD) AND $
      (Max((SED.Cafe.Fit.Com.CLD)[idx]) GT yRange[0])) THEN BEGIN
     components = [components, input.Label_CLD]
     colorList = [colorList, FSC_Color(colors.CLD)]
     lineStyle = [lineStyle, style.CLD]
     thickArray = [thickArray, thick]
  ENDIF

  IF ((plotCOO) AND $
      (Max((SED.Cafe.Fit.Com.COO)[idx]) GT yRange[0])) THEN BEGIN
     components = [components, input.Label_COO]
     colorList = [colorList, FSC_Color(colors.COO)]
     lineStyle = [lineStyle, style.COO]
     thickArray = [thickArray, thick]
  ENDIF

  IF ((plotWRM) AND $
      (Max((SED.Cafe.Fit.Com.WRM)[idx]) GT yRange[0])) THEN BEGIN
     components = [components, input.Label_WRM]
     colorList = [colorList, FSC_Color(colors.WRM)]
     lineStyle = [lineStyle, style.WRM]
     thickArray = [thickArray, thick]
  ENDIF

  IF ((plotHOT) AND $
      (Max((SED.Cafe.Fit.Com.HOT)[idx]) GT yRange[0])) THEN BEGIN
     components = [components, input.Label_HOT]
     colorList = [colorList, FSC_Color(colors.HOT)]
     lineStyle = [lineStyle, style.HOT]
     thickArray = [thickArray, thick]
  ENDIF

  IF (Keyword_Set(IRS)) THEN BEGIN
     addLIN = (input.Plot_IRS_LINONCON) ? SED.Cafe.Fit.Com.CON : 0.D
     addPAH = (input.Plot_IRS_PAHONCON) ? SED.Cafe.Fit.Com.CON : 0.D
  ENDIF $
  ELSE BEGIN
     addLIN = 0.D
     addPAH = 0.D
  ENDELSE

  maxRatioLIN = (Max(addLIN) EQ 0.D) ? 2.D : $
                Max(1.D + (SED.Cafe.Fit.Com.LIN / SED.Cafe.Fit.Com.CON)[idx])
  IF ((plotLIN) AND $
      (Max((addLIN + SED.Cafe.Fit.Com.LIN)[idx]) GT yRange[0]) AND $
      (maxRatioLIN GT 1.05)) THEN BEGIN
     components = [components, input.Label_LIN]
     colorList = [colorList, FSC_Color(colors.LIN)]
     lineStyle = [lineStyle, style.LIN]
     thickArray = [thickArray, thick]
  ENDIF

  maxRatioPAH = (Max(addPAH) EQ 0.D) ? 2.D : $
                Max(1.D + (SED.Cafe.Fit.Com.PAH / SED.Cafe.Fit.Com.CON)[idx])
  IF ((plotPAH) AND $
      (Max((addPAH + SED.Cafe.Fit.Com.PAH)[idx]) GT yRange[0]) AND $
      (maxRatioPAH GT 1.05)) THEN BEGIN
     components = [components, input.Label_PAH]
     colorList = [colorList, FSC_Color(colors.PAH)]
     lineStyle = [lineStyle, style.PAH]
     thickArray = [thickArray, thick]
  ENDIF

  IF ((plotSTR) AND $
      (Max((SED.Cafe.Fit.Com.STR)[idx]) GT yRange[0])) THEN BEGIN
     components = [components, input.Label_STR]
     colorList = [colorList, FSC_Color(colors.STR)]
     lineStyle = [lineStyle, style.STR]
     thickArray = [thickArray, thick]
  ENDIF

  IF ((plotDSK) AND $
      (Max((SED.Cafe.Fit.Com.DSK)[idx]) GT yRange[0])) THEN BEGIN
     components = [components, input.Label_DSK]
     colorList = [colorList, FSC_Color(colors.DSK)]
     lineStyle = [lineStyle, style.DSK]
     thickArray = [thickArray, thick]
  ENDIF

  IF ((plotSTB) AND $
      (Max((SED.Cafe.Fit.Com.STB)[idx]) GT yRange[0])) THEN BEGIN
     components = [components, input.Label_STB]
     colorList = [colorList, FSC_Color(colors.STB)]
     lineStyle = [lineStyle, style.STB]
     thickArray = [thickArray, thick]
  ENDIF

  IF ((plotCON) AND $
      (Max((SED.Cafe.Fit.Com.CON)[idx]) GT yRange[0])) THEN BEGIN
     components = [components, 'Cont.']
     colorList = [colorList, FSC_Color(colors.CON)]
     lineStyle = [lineStyle, style.CON]
     thickArray = [thickArray, 2.D*thick]
  ENDIF

  nComponents = N_Elements(components)
  charSize = (Keyword_Set(PS)) ? 1.D : 1.1D

  posXScale = (Keyword_Set(PS)) ? 0.02D : 0.01D
  posYScale = (Keyword_Set(PS)) ? 0.03D : 0.02D
  XYouts, position[0] + posXScale, position[1], SED.Object, $
          Color=FSC_Color(colors.AXS), CharSize=(1.1 * charSize), /Normal
  position[1] -= 0.005D
  IF (N_Elements(subTitle) NE 0) THEN BEGIN
     position[1] -= posYScale
     XYOuts, position[0] + posXScale, position[1], $
             subTitle, Color=FSC_Color(colors.AXS), $
             CharSize=(1.0 * charSize), /Normal
     position[1] -= 0.005D
  ENDIF

  AL_Legend, components, $
             LineStyle=lineStyle, $
             Colors=colorList, $
             TextColors=Replicate(FSC_Color(colors.AXS), nComponents), $
             Box=0, $
             Position=position, $
             /Normal, $
             Thick=thickArray, $
             CharSize=charSize

  RETURN

END

;;+ ===================================================================
; NAME:
; CAFE_PLOT_OUT
;
; PURPOSE:
; This routine produces plot output.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

PRO Cafe_Plot_Out, SED, colors, input, PS=PS, YRange=yRange, $
                   RangeIRS=rangeIRS, IRS=IRS

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Get output structure.
  output = Cafe_Output(SED, input, YRange=yRange, RangeIRS=rangeIRS, $
                             IRS=IRS)

  ;; Print fit parameter output.
  position = (Keyword_Set(PS)) ? $
             [0.70, 0.1, 0.95, 0.99] : [0.70, 0.1, 0.95, 0.985]
  IF (Keyword_Set(PS)) THEN BEGIN
     xStart = 0.10
     yStart = 0.98
     spacing = 0.035
     charSize = 0.91
  ENDIF $
  ELSE BEGIN
     xStart = 0.08
     yStart = 0.98
     spacing = 0.032
     charSize = 1.15
  ENDELSE
  Plot, [0,1], [0,1], /NoData, XStyle=4, YStyle=4, /NoErase, Position=position
  nPrint = N_Elements(output.Name)
  x = xStart
  y = yStart
  target = SED.Object
  XYOutS, x, y, target, CharSize=charSize * 1.2, Color=FSC_Color(colors.AXS)
  y -= spacing
  FOR i=0,nPrint-1 DO BEGIN
     line = ''
     IF (output[i].Name NE '') THEN BEGIN
        thisFormat = output[i].Format
        sci = thisFormat[0]
        noTrim = thisFormat[1]
        sigFig = thisFormat[2]
        sigFigErr = thisFormat[3]
        line = output[i].Name + ' = ' + $
               Jam_Print(output[i].Value, sigFig, Scientific=sci, $
                         NoTrim=noTrim)
        IF (output[i].Err GT 0.D) THEN BEGIN
           line += (TeXtoIDL(' \pm ') + $
                    Jam_Print(output[i].Err, sigFigErr, Scientific=sci, $
                              NoTrim=noTrim))
        ENDIF
        IF (output[i].Units NE '') THEN line += ' ' + output[i].Units
     ENDIF
     XYOutS, x, y, StrCompress(line), CharSize=charSize, $
             Color=FSC_Color(colors.AXS)
     y -= spacing
  ENDFOR

  RETURN

END

;;+ ===================================================================
; NAME:
; CAFE_PLOT
;
; PURPOSE:
; This routine produces plot output.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

PRO Cafe_Plot, SED, input, PS=PS, OutPath=outPath, SubTitle=subTitle, $
               OPlotFit=oPlotFit, Lines=lines, XRange=xRange

  Compile_Opt IDL2, Hidden
  Catch, theError
  IF (theError NE 0) THEN BEGIN
     Catch, /Cancel
     IF ((Keyword_Set(PS)) AND (!D.Name EQ 'PS')) THEN Jam_PS, /Close
     ok = Error_Message(/Traceback)
     RETURN
  ENDIF

  ;; Default input file.
  IF (N_Elements(input) EQ 0) THEN input = SED.Cafe.Extra.Input

  ;; Setup window device.
  IF (~Keyword_Set(PS)) THEN BEGIN
     IF ((Get_Screen_Size())[0] EQ 1024.0) THEN BEGIN
        xSize = 800.D
        ySize = 650.D
     ENDIF $
     ELSE BEGIN
        xSize = 1000.D
        ySize = 800.D
     ENDELSE
     scrSize = Get_Screen_Size()
     xOffset = scrSize[0] - xSize
     yOffset = scrSize[1] - ySize
     TLB_IRS = Jam_TLB(Title=('CAFE - IRS - ' + SED.Object), $
                       XOffSet=xOffset, YOffSet=yOffset)
     tabID_IRS = Widget_Tab(TLB_IRS -> ID())
     TLB_SED = Jam_TLB(Title=('CAFE - SED - ' + SED.Object), $
                       XOffSet=xOffset, YOffSet=yOffset)
     tabID_SED = Widget_Tab(TLB_SED -> ID())
     IF (input.Make_IRS_Plot EQ 1) THEN BEGIN
        baseID_IRS = Widget_Base(tabID_IRS, Title='IRS')
        drawID_IRS = Widget_Draw(baseID_IRS, Scr_XSize=xSize, Scr_YSize=ySize)
     ENDIF
     IF (input.Make_SED_Plot EQ 1) THEN BEGIN
        baseID_SED = Widget_Base(tabID_SED, Title='SED')
        drawID_SED = Widget_Draw(baseID_SED, Scr_XSize=xSize, Scr_YSize=ySize)
     ENDIF
     CenterTLB, TLB_IRS->ID(), 0, 0, /NoCenter
     CenterTLB, TLB_SED->ID(), 0, 0, /NoCenter
     TLB_IRS->Realize
     TLB_SED->Realize
     IF (input.Make_IRS_Plot EQ 1) THEN $
        Widget_Control, drawID_IRS, Get_Value=WID_IRS
     IF (input.Make_SED_Plot EQ 1) THEN $
        Widget_Control, drawID_SED, Get_Value=WID_SED
  ENDIF

  ;; Set colors.
  colorsX = { BCK: 'Gray', $
              AXS: 'Black', $
              DAT: 'Black', $
              PAH: 'Purple', $
              LIN: 'Red', $
              CIR: 'Chocolate', $
              CLD: 'Red', $
              COO: 'ForestGreen', $
              WRM: 'Blue', $
              HOT: 'VioletRed', $
              STR: 'Gold', $
              STB: 'Goldenrod', $
              DSK: 'Crimson', $
              FIT: 'Orange', $
              CON: 'DarkGray', $
              ERR: 'SlateGray', $
              FER: 'MediumGray' }
  colorsPS = { BCK: 'White', $
               AXS: 'Black', $
               DAT: 'Black', $
               PAH: 'Purple', $
               LIN: 'Red', $
               CIR: 'Chocolate', $
               CLD: 'Red', $
               COO: 'ForestGreen', $
               WRM: 'Blue', $
               HOT: 'VioletRed', $
               STR: 'Gold', $
               STB: 'Goldenrod', $
               DSK: 'Crimson', $
               FIT: 'Orange', $
               CON: 'DarkGray', $
               ERR: 'Gray', $
               FER: 'LightGray' }
  colorsBW = { BCK: 'White', $
               AXS: 'Black', $
               DAT: 'Black', $
               PAH: 'DarkGray', $
               LIN: 'MediumGray', $
               CIR: 'DarkGray', $
               CLD: 'DarkGray', $
               COO: 'DarkGray', $
               WRM: 'DarkGray', $
               HOT: 'DarkGray', $
               STR: 'DarkGray', $
               STB: 'DarkGray', $
               DSK: 'DarkGray', $
               FIT: 'DarkGray', $
               CON: 'Black', $
               ERR: 'Gray', $
               FER: 'LightGray' }

  ;; Define linestyles.
  style   = { FIT:0, $
              CON:0, $
              LIN:0, $
              PAH:0, $
              STR:0, $
              STB:0, $
              DSK:0, $
              CIR:0, $
              CLD:0, $
              COO:0, $
              WRM:0, $
              HOT:0 }
  styleMX = { FIT:0, $
              CON:5, $
              LIN:1, $
              PAH:1, $
              STR:2, $
              STB:5, $
              DSK:2, $
              CIR:2, $
              CLD:1, $
              COO:5, $
              WRM:3, $
              HOT:4 }
  styleBW = { FIT:0, $
              CON:5, $
              LIN:1, $
              PAH:1, $
              STR:2, $
              STB:5, $
              DSK:2, $
              CIR:2, $
              CLD:1, $
              COO:5, $
              WRM:3, $
              HOT:4 }

  ;; Set thickness.
  thick =  (Keyword_Set(PS)) ? 2.5 : 1.0

  IF (N_Elements(outPath) EQ 0) THEN outPath = !Cafe.Out

  IF (Keyword_Set(PS)) THEN BEGIN
     fileName = FilePath(SED.Object + '.cafe.ps', Root_Dir=outPath)
     Jam_PS, fileName, /Color, Times=input.Plot_IRS_TIM, /Landscape
  ENDIF ELSE $
     IF (input.Make_SED_Plot EQ 1) THEN WSet, WID_SED

  IF (input.Make_SED_Plot) THEN BEGIN
     check = Cafe_Plot_Data(SED, colorsX, style, input, Thick=thick, $
                                  PS=PS, XRange=xRangeSED, YRange=yRangeSED, $
                                  OPlotFit=oPlotFit)
     IF (check) THEN BEGIN
        Cafe_Plot_LEG, SED, colorsX, style, input, Thick=thick, PS=PS, $
                             YRange=yRangeSED, SubTitle=subTitle
        Cafe_Plot_RES, SED, colorsX, input, Thick=thick, PS=PS, $
                             XRange=xRangeSED
        Cafe_Plot_OUT, SED, colorsX, input, PS=PS, YRange=yRangeSED
     ENDIF

  ENDIF

  IF (~Keyword_Set(PS)) THEN WSet, WID_IRS

  IF (input.Make_IRS_Plot) THEN BEGIN
     check = Cafe_Plot_Data(SED, colorsX, style, input, /IRS, $
                                  Thick=thick, PS=PS, XRange=xRangeIRS, $
                                  YRange=yRangeIRS, OPlotFit=oPlotFit, $
                                  Lines=lines)
     IF (check) THEN BEGIN
        Cafe_Plot_LEG, SED, colorsX, style, input, /IRS, Thick=thick, $
                             PS=PS, YRange=yRangeIRS, SubTitle=subTitle
        Cafe_Plot_RES, SED, colorsX, input, /IRS, Thick=thick, PS=PS, $
                             XRange=xRangeIRS
        IF (N_Elements(yRangeSED) EQ 0) THEN yRangeSED = yRangeIRS
        Cafe_Plot_OUT, SED, colorsX, input, PS=PS, YRange=yRangeSED, $
                             RangeIRS=yRangeIRS, /IRS
     ENDIF
  ENDIF

  IF (Keyword_Set(PS)) THEN Jam_PS, /Close

  IF (Keyword_Set(PS)) THEN BEGIN

     IF (input.Make_IRS_Plot) THEN BEGIN

        check = 0
        IF (input.Plot_IRS_COL) THEN BEGIN
           fileName = FilePath(SED.Object + '.cafe.irs.col.eps', $
                               Root_Dir=outPath)
           Jam_PS, fileName, Times=(input.Plot_IRS_TIM), /EPS, /Color
           check = Cafe_Plot_Data(SED, colorsPS, style, input, /IRS, $
                                        /CRC, /PS, Thick=thick, $
                                        XRange=xRangeIRS, YRange=yRange, $
                                        OPlotFit=oPlotFit, Lines=lines)
           IF (check) THEN $
              Cafe_Plot_LEG, SED, colorsPS, style, input, /IRS, /CRC, $
                                   Thick=thick, /PS, YRange=yRange, $
                                   SubTitle=subTitle
           Jam_PS, /Close
        ENDIF

        IF (input.Plot_IRS_MIX) THEN BEGIN
           fileName = FilePath(SED.Object + '.cafe.irs.mix.eps', $
                               Root_Dir=outPath)
           Jam_PS, fileName, Times=(input.Plot_IRS_TIM), /EPS, /Color
           check = Cafe_Plot_Data(SED, colorsPS, styleMX, input, /IRS, $
                                        /CRC, /PS, Thick=thick, $
                                        XRange=xRangeIRS, YRange=yRange, $
                                        OPlotFit=oPlotFit, Lines=lines)
           IF (check) THEN $
              Cafe_Plot_LEG, SED, colorsPS, styleMX, input, /IRS, /CRC, $
                                   Thick=thick, /PS, YRange=yRange, $
                                   SubTitle=subTitle
           Jam_PS, /Close
        ENDIF

        IF (input.Plot_IRS_BNW) THEN BEGIN
           fileName = FilePath(SED.Object + '.cafe.irs.bnw.eps', $
                               Root_Dir=outPath)
           Jam_PS, fileName, Times=(input.Plot_IRS_TIM), /EPS
           check = Cafe_Plot_Data(SED, colorsBW, styleBW, input, /IRS, $
                                        /CRC, /PS, Thick=thick, $
                                        XRange=xRangeIRS, YRange=yRange, $
                                        OPlotFit=oPlotFit, Lines=lines)
           IF (check) THEN $
              Cafe_Plot_LEG, SED, colorsBW, styleBW, input, /IRS, /CRC, $
                                   Thick=thick, /PS, YRange=yRange, $
                                   SubTitle=subTitle
           Jam_PS, /Close
        ENDIF

        IF (input.Plot_IRS_RES) THEN BEGIN
           fileName = FilePath(SED.Object + '.cafe.irs.res.eps', $
                               Root_Dir=outPath)
           Jam_PS, fileName, Times=(input.Plot_IRS_TIM), /EPS
           IF (check) THEN $
              Cafe_Plot_RES, SED, colorsPS, input, Thick=thick, /IRS, $
                                   /CRC, /PS, XRange=xRangeIRS
           Jam_PS, /Close
        ENDIF

     ENDIF

     IF (input.Make_SED_Plot) THEN BEGIN

        check = 0
        IF (input.Plot_SED_COL) THEN BEGIN
           fileName = FilePath(SED.Object + '.cafe.sed.col.eps', $
                               Root_Dir=outPath)
           Jam_PS, fileName, Times=(input.Plot_SED_TIM), /EPS, /Color
           check = Cafe_Plot_Data(SED, colorsPS, style, input, /CRC, $
                                        /PS, Thick=thick, XRange=xRangeSED, $
                                        YRange=yRange, OPlotFit=oPlotFit)
           IF (check) THEN $
              Cafe_Plot_LEG, SED, colorsPS, style, input, /CRC, $
                                   Thick=thick, /PS, YRange=yRange, $
                                   SubTitle=subTitle
           Jam_PS, /Close
        ENDIF

        IF (input.Plot_SED_MIX) THEN BEGIN
           fileName = FilePath(SED.Object + '.cafe.sed.mix.eps', $
                               Root_Dir=outPath)
           Jam_PS, fileName, Times=(input.Plot_SED_TIM), /EPS, /Color
           check = Cafe_Plot_Data(SED, colorsPS, styleMX, input, /CRC, $
                                        /PS, Thick=thick, XRange=xRangeSED, $
                                        YRange=yRange, OPlotFit=oPlotFit)
           IF (check) THEN $
              Cafe_Plot_LEG, SED, colorsPS, styleMX, input, /CRC, $
                                   Thick=thick, /PS, YRange=yRange, $
                                   SubTitle=subTitle
           Jam_PS, /Close
        ENDIF

        IF (input.Plot_SED_BNW) THEN BEGIN
           fileName = FilePath(SED.Object + '.cafe.sed.bnw.eps', $
                               Root_Dir=outPath)
           Jam_PS, fileName, Times=(input.Plot_SED_TIM), /EPS
           check = Cafe_Plot_Data(SED, colorsBW, styleBW, input, /CRC, $
                                        /PS, Thick=thick, XRange=xRangeSED, $
                                        YRange=yRange, OPlotFit=oPlotFit)
           IF (check) THEN $
              Cafe_Plot_LEG, SED, colorsBW, styleBW, input, /CRC, $
                                   Thick=thick, /PS, YRange=yRange, $
                                   SubTitle=subTitle
           Jam_PS, /Close
        ENDIF

        IF (input.Plot_SED_RES) THEN BEGIN
           fileName = FilePath(SED.Object + '.cafe.sed.res.eps', $
                               Root_Dir=outPath)
           Jam_PS, fileName, Times=(input.Plot_SED_TIM), /EPS
           IF (check) THEN $
              Cafe_Plot_RES, SED, colorsPS, input, Thick=thick, /CRC, $
                                   /PS, XRange=xRangeSED
           Jam_PS, /Close
        ENDIF

     ENDIF

  ENDIF

  RETURN

END

;;+ ===================================================================
; NAME:
; CAFE_SAVE
;
; PURPOSE:
; This routine produces an output text file.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

PRO Cafe_Save, SED, input, p, OutPath=outPath, FitLINs=fitLINs, $
					FitPAHs=fitPAHs, SubTitle=subTitle, Wave0=wave0, $
					Flux0=flux0, OPlotFit=oPlotFit, Lines=lines

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Save postscript output.
  IF (N_Elements(outPath) EQ 0) THEN outPath = !Cafe.Out
  Cafe_Plot, SED, input, /PS, OutPath=outPath, SubTitle=subTitle, $
                   OPlotFit=oPlotFit, Lines=lines

  ;; Create file names.
  fileINF = FilePath(SED.Object + '.cafe.info.txt', Root_Dir=outPath)
  fileLOG = FilePath(SED.Object + '.cafe.log.txt', Root_Dir=outPath)
  fileXDR = FilePath(SED.Object + '.cafe.irslow.xdr', Root_Dir=outPath)
  fileParPAH = FilePath(SED.Object + '.cafe.pah.par.txt', Root_Dir=outPath)
  fileParLIN = FilePath(SED.Object + '.cafe.lin.par.txt', Root_Dir=outPath)
  fileErrPAH = FilePath(SED.Object + '.cafe.pah.err.txt', Root_Dir=outPath)
  fileErrLIN = FilePath(SED.Object + '.cafe.lin.err.txt', Root_Dir=outPath)
  fileDatPAH = FilePath(SED.Object + '.cafe.pah.dat.txt', Root_Dir=outPath)
  fileDatLIN = FilePath(SED.Object + '.cafe.lin.dat.txt', Root_Dir=outPath)

  ;; Create padded parameter names vector.
  maxLength = Max(StrLen(SED.Cafe.Params.Names))
  padNames = Jam_PadString(SED.Cafe.Params.Names, maxLength)
  nParams = N_Elements(padNames)

  ;; Create structure of output info.
  output = { Object: SED.Object, $
             Date: SysTime(), $
             z: SED.Cafe.FIT.z, $
             ChiSqrSED: SED.Cafe.ChiSqrSED, $
             ChiSqrIRS: SED.Cafe.ChiSqrIRS, $
             ChiSqrOPT: SED.Cafe.ChiSqrOPT, $
             ChiSqrNIR: SED.Cafe.ChiSqrNIR, $
             ChiSqrMIR: SED.Cafe.ChiSqrMIR, $
             ChiSqrFIR: SED.Cafe.ChiSqrFIR, $
             ChiSqrRAD: SED.Cafe.ChiSqrRAD, $
             ChiSqrFTR: SED.Cafe.ChiSqrFTR, $
             DOF_SED: SED.Cafe.DOF_SED, $
             DOF_IRS: SED.Cafe.DOF_IRS, $
             DOF_OPT: SED.Cafe.DOF_OPT, $
             DOF_NIR: SED.Cafe.DOF_NIR, $
             DOF_MIR: SED.Cafe.DOF_MIR, $
             DOF_FIR: SED.Cafe.DOF_FIR, $
             DOF_RAD: SED.Cafe.DOF_RAD, $
             DOF_FTR: SED.Cafe.DOF_FTR, $
             N_Free: SED.Cafe.N_Free, $
             Wave0_CIR: wave0.CIR, $
             Wave0_CLD: wave0.CLD, $
             Wave0_COO: wave0.COO, $
             Wave0_WRM: wave0.WRM, $
             Wave0_HOT: wave0.HOT, $
             Wave0_STR: wave0.STR, $
             Wave0_PAH: wave0.PAH, $
             Flux0_CIR: flux0.CIR, $
             Flux0_CLD: flux0.CLD, $
             Flux0_COO: flux0.COO, $
             Flux0_WRM: flux0.WRM, $
             Flux0_HOT: flux0.HOT, $
             Flux0_STR: flux0.STR, $
             Flux0_PAH: flux0.PAH, $
             Wave_DST: input.Wave_DST }

  ;; Append parameter values and errors.
  FOR i=0,nParams-1 DO $
     output = Create_Struct(output, $
                            (SED.Cafe.Params.Names)[i], $
                            (SED.Cafe.Params.Values)[i])
  FOR i=0,nParams-1 DO $
     output = Create_Struct(output, (SED.Cafe.Params.Names)[i] + '_ERR', $
                            (SED.Cafe.Params.Errors)[i])

  ;; Append tau0 values.
  tau0 = SED.Cafe.Tau0
  output = Create_Struct(output, $
                         'COO_TAU0', tau0.COO, 'COO_TAU0_ERR', tau0.COO_ERR, $
                         'WRM_TAU0', tau0.WRM, 'WRM_TAU0_ERR', tau0.WRM_ERR, $
                         'HOT_TAU0', tau0.HOT, 'HOT_TAU0_ERR', tau0.HOT_ERR, $
                         'STB_TAU0', tau0.STB, 'STB_TAU0_ERR', tau0.STB_ERR, $
                         'DSK_TAU0', tau0.DSK, 'DSK_TAU0_ERR', tau0.DSK_ERR)

  ;; Append mean component temperatures.
  T_Mean = SED.Cafe.T_Mean
  output = Create_Struct(output, $
                         'CIR_TMP_AVG', T_Mean.CIR, $
                         'CLD_TMP_AVG', T_Mean.CLD, $
                         'COO_TMP_AVG', T_Mean.COO, $
                         'WRM_TMP_AVG', T_Mean.WRM, $
                         'HOT_TMP_AVG', T_Mean.HOT, $
                         'CIR_TMP_AVG_ERR', T_Mean.CIR_ERR, $
                         'CLD_TMP_AVG_ERR', T_Mean.CLD_ERR, $
                         'COO_TMP_AVG_ERR', T_Mean.COO_ERR, $
                         'WRM_TMP_AVG_ERR', T_Mean.WRM_ERR, $
                         'HOT_TMP_AVG_ERR', T_Mean.HOT_ERR)

  ;; Append component luminosities.
  tags = Tag_Names(SED.Cafe.Luminosity)
  FOR i=0,N_Tags(SED.Cafe.Luminosity)-1 DO $
     output = Create_Struct(output, 'LUM_' + tags[i], $
                            (SED.Cafe.Luminosity).(i))

  ;; Append component masses.
  tags = Tag_Names(SED.Cafe.Mass)
  FOR i=0,N_Tags(SED.Cafe.Mass)-1 DO $
     output = Create_Struct(output, 'MASS_' + tags[i], $
                            (SED.Cafe.Mass).(i))

  ;; Append synggthetic photometry.
  IF (Jam_Check(SED.Cafe.SynPhot, TName='STRUCT', Tag_Name='Flux')) THEN BEGIN
     tags = Tag_Names(SED.Cafe.SynPhot.Flux)
     FOR i=0,N_Tags(SED.Cafe.SynPhot.Flux)-1 DO $
        output = Create_Struct(output, tags[i], (SED.Cafe.SynPhot.Flux).(i))
  ENDIF

  ;; Write info .txt file.
  IF (input.Save_INF) THEN Jam_WriteData, output, FileName=fileINF

  ;; Write log .txt file.
  IF (input.Save_LOG) THEN BEGIN
     OpenW, LUN, fileLOG, /Get_LUN
     PrintF, LUN, Transpose(SED.Cafe.Log)
     Free_LUN, LUN
  ENDIF

  ;; Write CAFE .xdr file.
  IF (input.Save_XDR) THEN Save, FileName=fileXDR, SED

  ;; Write PAH files.
  IF ((input.Save_PAH) AND (fitPAHs) AND (p.PAH_FLX GT 1D-2)) THEN BEGIN
     Jam_WriteData, SED.Cafe.PAH.Drude, FileName=fileParPAH
     Jam_WriteData, SED.Cafe.PAH.ErrDrude, FileName=fileErrPAH
     Jam_WriteData, SED.Cafe.PAH.Data, FileName=fileDatPAH
  ENDIF

  ;; Write LIN file.
  IF ((input.Save_LIN) AND (fitLINs)) THEN BEGIN
     Jam_WriteData, SED.Cafe.LIN.Gauss, FileName=fileParLIN
     Jam_WriteData, SED.Cafe.LIN.ErrGauss, FileName=fileErrLIN
     Jam_WriteData, SED.Cafe.LIN.Data, FileName=fileDatLIN
  ENDIF

END

;;+ ===================================================================
; NAME:
; CAFE_EFITFLUX
;
; PURPOSE:
; This routine is called to calculate flux errors.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

FUNCTION Cafe_EFitFlux, wave, params, _Extra=extra

  Compile_Opt IDL2, Hidden
  On_Error, 2

  flux = Cafe_Flux(wave, params, Com=com, Ext=ext, E0=E0, Tau0=tau0, $
                   ModCom=modCom, _Extra=extra)

  RETURN, { TOT_FLX: flux, $
            SRC_FLX: com.SRC, $
            DST_FLX: com.DST, $
            CON_FLX: com.CON, $
            CIR_FLX: com.CIR, $
            CLD_FLX: com.CLD, $
            COO_FLX: com.COO, $
            WRM_FLX: com.WRM, $
            HOT_FLX: com.HOT, $
            STR_FLX: com.STR, $
            STB_FLX: com.STB, $
            STB_100_FLX: com.STB_100, $
            STB_010_FLX: com.STB_010, $
            STB_002_FLX: com.STB_002, $
            DSK_FLX: com.DSK, $
            LIN_FLX: com.LIN, $
            PAH_FLX: com.PAH, $

            SRC0_FLX: modCom.SRC, $
            COO0_FLX: modCom.COO, $
            WRM0_FLX: modCom.WRM, $
            HOT0_FLX: modCom.HOT, $
            STR0_FLX: modCom.STR, $
            STB0_FLX: modCom.STB, $
            STB0_100_FLX: modCom.STB_100, $
            STB0_010_FLX: modCom.STB_010, $
            STB0_002_FLX: modCom.STB_002, $
            DSK0_FLX: modCom.DSK, $
            PAH0_FLX: modCom.PAH, $

            COO_EXT: ext.COO, $
            WRM_EXT: ext.WRM, $
            HOT_EXT: ext.HOT, $
            STR_EXT: ext.STR, $
            STB_EXT: ext.STB, $
            DSK_EXT: ext.DSK, $
            PAH_EXT: ext.PAH, $

            COO_TAU0: tau0.COO, $
            WRM_TAU0: tau0.WRM, $
            HOT_TAU0: tau0.HOT, $

            CIR_E0: E0.CIR, $
            CLD_E0: E0.CLD, $
            COO_E0: E0.COO, $
            WRM_E0: E0.WRM, $
            HOT_E0: E0.HOT }

END

;;+ ===================================================================
; NAME:
; CAFE_INPUT
;
; PURPOSE:
; This routine is called to update the input structure.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

FUNCTION Cafe_Input, input, p

  Compile_Opt IDL2, Hidden
  On_Error, 2

  pTags = '_' + Tag_Names(p)
  N_PTAGS = N_Tags(p)
  inputTags = Tag_Names(input)
  newInput = input
  FOR i=0,N_PTAGS-1 DO BEGIN
     idx = Where(inputTags EQ pTags[i], cnt)
     IF (cnt EQ 1) THEN newInput.(idx) = p.(i)
  ENDFOR

  RETURN, newInput

END

;;+ ===================================================================
; NAME:
; CAFE
;
; PURPOSE:
; This is where the magic of the Continuum And Feature Extrator happens.
;
; NOTE:
; See the header block at the top of this file for a description of the
; inputs and outputs of the procedure.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, July 2005.
;;-

PRO Cafe, object, $
			 SED=SED, $ ;; <- Optional structure containing output of fit.
			 ;; The following are optional input parameters to modify default behavior.
			 Input=_input, $
			 MinWave=_minWave, $
			 MaxWave=_maxWave, $
			 InPath=inPath, $
			 OutPath=outPath, $
			 UseEFlux=useEFlux, $
			 Lines=lines, $
			 LimitOPT=limitOPT, $
			 LimitNIR=limitNIR, $
			 LimitFIR=limitFIR, $
			 ForceMIPS=forceMIPS, $
			 NoPAH3=noPAH3, $
			 SubTitle=subTitle, $
			 NoView=noView, $
			 NoSave=noSave, $
			 MinSN=minSN, $
			 OPlotFit=oPlotFit, $
			 Quiet=quiet, $
			 NoCatch=noCatch, $
			 ShowAllIRS=showAllIRS, $
			 IRAS=IRAS, $
			 Wave0=wave0, $
			 Flux0=flux0, $
			 L_dust=L_dust, $
			 L_IR=L_IR, $
			 IRSErrSCL=IRSErrSCL, $
			 Debug=debug, $
			 _Extra=extra, $
			 ;; Internal use only below...
			 Internal=internal, $
			 Log=log

  Compile_Opt IDL2
  IF (~Keyword_Set(noCatch)) THEN BEGIN
     Catch, theError
     IF (theError NE 0) THEN BEGIN
        Catch, /Cancel
        ok = Error_Message(/Traceback)
        RETURN
     ENDIF
  ENDIF ELSE $
     On_Error, 2

  ;; Set default SED path.
  pathSED = (N_Elements(inPath) EQ 0) ? !Cafe.In : inPath

  ;; Read the CAFE input file.
  IF (N_Elements(_input) NE 0) THEN BEGIN
     input = (Jam_Check(_input, TName='STRING')) ? $
             Jam_ReadData(File_Which(_input + '.txt')) : _input
  ENDIF ELSE $
     input = Jam_ReadData(File_Which('cafe.txt'))

  ;; Update CAFE input with values passed via command line.
  IF (N_Elements(extra) NE 0) THEN BEGIN
     extraTags = Tag_Names(extra)
     inputTags = Tag_Names(input)
     FOR i=0,N_Elements(extraTags)-1 DO BEGIN
        idx = Where(extraTags[i] EQ inputTags, cnt)
        IF (cnt EQ 1) THEN BEGIN
           IF (Keyword_Set(internal)) THEN BEGIN
              IF ((StrMid(extraTags[i], 0, 1) NE '_') AND $
                  (StrUpCase(StrMid(extraTags[i], 0, 3)) NE 'FIX')) THEN BEGIN
                 input.(idx) = extra.(i)
              ENDIF
           ENDIF ELSE $
              input.(idx) = extra.(i)
        ENDIF ELSE $
           input = Create_Struct(input, extraTags[i], extra.(i))
     ENDFOR
  ENDIF

  ;; Produce dialog widget to select and restore SED.
  SED, object, Path_SED=pathSED, Path_Phot=pathSED, SED=SED, Phot=phot, $
       IRAS=IRAS, /NoCatch, _Extra=extra
  IF (Jam_Check(SED, TName='STRING')) THEN RETURN
  IF (N_Elements(object) EQ 0) THEN object = SED.Object

  ;; Calculate redshift.
  z = SED.cz / 3D5

  ;; Check if IRS data exists and set fitting options.
  fitIRS = input.FitIRS
  plotIRS = input.PlotIRS
  IF (Finite((SED.Wave)[0], /NaN)) THEN BEGIN
     fitIRS = 0L
     plotIRS = 0L
  ENDIF

  ;; Update IRS scaling factor.
  IF ((fitIRS EQ 1L) OR (plotIRS EQ 1L)) THEN BEGIN
     doScale = 0
     sclFluxTo = 'MIPS24'
     idxScale = Where(phot.Name EQ 'MIPS24-12"', cntScale)
     IF (cntScale NE 1) THEN BEGIN
        idxScale = Where((phot.Filter EQ 'IRAS25') AND $
                         (phot.Reference EQ '2003AJ....126.1607S'), $
                         cntScale)
        sclFluxTo = 'IRAS25'
     ENDIF
     IF (cntScale NE 1) THEN BEGIN
        idxScale = Where((phot.Filter EQ 'IRAS25') AND $
                         (phot.Reference EQ '1990IRASF.C...0000M'), $
                         cntScale)
        sclFluxTo = 'IRAS25'
     ENDIF
     synPhot = Jam_SynPhot(SED.Wave * (1.D + z), SED.Flux.Jy * (1.D + z)) ;; ??
     synPhotNames = Tag_Names(synPhot.Flux)
     idxScaleSyn = Where(synPhotNames EQ sclFluxTo, cntScaleSyn)
     IF ((cntScale EQ 1) AND (cntScaleSyn EQ 1)) THEN BEGIN
        waveScale = (phot.WRest)[idxScale]
        IF ((waveScale GE Min(SED.Wave)) AND $
            (waveScale LE Max(SED.Wave)) AND $
            ((phot.Stat)[idxScale] NE 1)) THEN BEGIN
           doScale = 1
           input.ScaleIRS = (phot.FRest)[idxScale] / $
                            synPhot.Flux.(idxScaleSyn)
        ENDIF
     ENDIF
  ENDIF

  ;; Create wave and flux vectors.
  IF (fitIRS) THEN BEGIN
     scaleIRS = input.ScaleIRS
     wave = SED.Wave
     flux = scaleIRS * SED.Flux.Jy
     sigma = scaleIRS * SED.EFlux.Jy
     nod1Flux = scaleIRS * SED.Nod1Flux.Jy
     nod2Flux = scaleIRS * SED.Nod2Flux.Jy
     IF ((Total(nod1Flux) EQ 0.D) OR (Total(nod2Flux) EQ 0.D)) THEN BEGIN
        dF = SED.Flux.Jy - Smooth(SED.Flux.Jy, 5)
        nod1Flux = scaleIRS * (SED.Flux.Jy + dF)
        nod2Flux = scaleIRS * (SED.Flux.Jy - dF)
     ENDIF
  ENDIF $
  ELSE BEGIN
     N_WAVE = 5
     wave = Jam_Dist(N_WAVE, 5.D, 35.D) / (1.D + z)
     null = Replicate(0.D, N_WAVE)
     flux = null
     sigma = null
     nod1Flux = null
     nod2Flux = null
     useEFlux = 1
  ENDELSE
  waveIRS = wave
  waveIRS_Min = Min(waveIRS, Max=waveIRS_Max)

  ;; Check and fix NaNs in sigma vector.
  idxNaN = Where(Finite(sigma, /NaN), cntNaN, Complement=idxNotNaN)
  FOR i=0,cntNaN-1 DO BEGIN
     sigma[idxNaN[i]] = Interpol(sigma[idxNotNaN], wave[idxNotNaN], wave[idxNaN[i]])
  ENDFOR

  ;; Create various flag vectors.
  N_WAVE = N_Elements(wave)
  stat = Replicate(0L, N_WAVE)
  name = Replicate('IRS', N_WAVE)
  filter = Replicate('', N_WAVE)
  fit = (fitIRS) ? Replicate(1L, N_WAVE) : Replicate(0L, N_WAVE)
  plot = (fitIRS AND plotIRS) ? Replicate(1L, N_WAVE) : Replicate(0L, N_WAVE)

  ;; Omit point with negative flux values from fits and plots.
  IF (fitIRS) THEN BEGIN
     idxNeg = Where(flux LE 0.D, cntNeg)
     IF (cntNeg GT 0) THEN BEGIN
        fit[idxNeg] = 0L
        plot[idxNeg] = 0L
     ENDIF
  ENDIF

  ;; Weigh/un-weigh IRS data at the depth of the 10 micron silicate feature.
  IF ((fitIRS) AND (input.WeighSIL EQ 0)) THEN BEGIN
     idxSIL = Where((wave GE 9.2D) AND (wave LE 11.D), cntSIL)
     IF (cntSIL GT 0) THEN fit[idxSIL] = 0L
  ENDIF

  ;; Enforce minimum and maximum wavelength limits.
  minWave = (N_Elements(_minWave) EQ 0) ? Min(wave) : (_minWave > Min(wave))
  maxWave = (N_Elements(_maxWave) EQ 0) ? Max(wave) : (_maxWave < Max(wave))
  idx = Where(wave LT minWave, count)
  IF (count GT 0) THEN BEGIN
     fit[idx] = 0L
     plot[idx] = (Keyword_Set(showAllIRS)) ? 1L : 0L
  ENDIF
  idx = Where(wave GT maxWave, count)
  IF (count GT 0) THEN BEGIN
     fit[idx] = 0L
     plot[idx] = (Keyword_Set(showAllIRS)) ? 1L : 0L
  ENDIF

  ;; Obtain index of flux data with NaN's and set flags.
  idxNaN = Where(Finite(flux, /NaN), cntNaN)
  IF (cntNaN GT 0) THEN BEGIN
     fit[idxNaN] = 0L
     plot[idxNaN] = 0L
  ENDIF

  ;; Calculate flux errors.
  IF (~Keyword_Set(useEFlux)) THEN BEGIN
     sigma = EFluxIRS(wave, nod1Flux, nod2Flux, dFlux=dFlux, ErrIdx=errIdx)
;     IF (errIdx[0] NE -1L) THEN sigma[errIdx] = Abs(dFlux[errIdx])
  ENDIF
  IF (N_Elements(IRSErrSCL) NE 0) THEN sigma *= IRSErrSCL
  IF (input.ErrIRS GT 0.D) THEN sigma = Sqrt(sigma^2 + (input.ErrIRS * flux)^2)

  ;; Only use IRS data with S/N > minSN.
  IF (N_Elements(minSN) EQ 0) THEN minSN = 2.D
  IF (minSN GT 0.D) THEN BEGIN
     idxSN = Where((flux / sigma) LE minSN, cntSN, NComplement=cntGood)
     IF (cntGood EQ 0) THEN $
        Message, 'Error: Not enough points with S/N > ' + String(minSN) + '!'
     IF (cntSN GT 0) THEN fit[idxSN] = 0L
  ENDIF

  ;; Create total wavelength and flux vectors for fit.
  IF ((phot.Name)[0] NE '') THEN BEGIN

     ;; OPT
     indexOPT = Where(phot.WRest LT 1.D, countOPT)
     IF (countOPT GT 0) THEN BEGIN
        nameOPT = (phot.Name)[indexOPT]
        filterOPT = (phot.Filter)[indexOPT]
        waveOPT = (phot.WRest)[indexOPT]
        fluxOPT = input.ScaleOPT * (phot.FRest)[indexOPT]
        sigmaOPT = input.ScaleOPT * (phot.EFRest)[indexOPT]
        IF (input.ErrOPT GT 0.D) THEN $
           sigmaOPT = Sqrt(sigmaOPT^2 + (input.ErrOPT * fluxOPT)^2)
        name = [name, nameOPT]
        filter = [filter, filterOPT]
        wave = [wave, waveOPT]
        flux = [flux, fluxOPT]
        sigma = [sigma, sigmaOPT]
        statOPT = (Keyword_Set(limitOPT)) ? $
                  Replicate(1, countOPT) : (phot.Stat)[indexOPT]
        stat = [stat, statOPT]
        fitOPT = (phot.Fit)[indexOPT]
        plotOPT = (phot.Plot)[indexOPT]
        IF (input.FitOPT EQ 0) THEN fitOPT *= 0
        IF (input.PlotOPT EQ 0) THEN plotOPT *= 0
        fit = [fit, fitOPT]
        plot = [plot, plotOPT]
     ENDIF

     ;; NIR
     indexNIR = Where((phot.WRest GE 1.D) AND $
                      (phot.WRest LT waveIRS_Min), countNIR)
     IF (countNIR GT 0) THEN BEGIN
        nameNIR = (phot.Name)[indexNIR]
        filterNIR = (phot.Filter)[indexNIR]
        waveNIR = (phot.WRest)[indexNIR]
        fluxNIR = input.ScaleNIR * (phot.FRest)[indexNIR]
        sigmaNIR = input.ScaleNIR * (phot.EFRest)[indexNIR]
        IF (input.ErrNIR GT 0.D) THEN $
           sigmaNIR = Sqrt(sigmaNIR^2 + (input.ErrNIR * fluxNIR)^2)
        IF (input.ErrLBand GT 0.D) THEN BEGIN
           idxLBand = Where((waveNIR GE 3.D) AND (waveNIR LE 3.6D), cntLBand)
           IF (cntLBand GT 0) THEN BEGIN
              sigmaNIR[idxLBand] = Sqrt(sigmaNIR[idxLBand]^2 + $
                                        (input.ErrLBand * fluxNIR[idxLBand])^2)
           ENDIF
        ENDIF
        IF (input.ErrIRAC1 GT 0.D) THEN BEGIN
           idxIRAC1 = Where(filterNIR EQ 'IRAC1', cntIRAC1)
           IF (cntIRAC1 GT 0) THEN BEGIN
              sigmaNIR[idxIRAC1] = Sqrt(sigmaNIR[idxIRAC1]^2 + $
                                         (input.ErrIRAC1 * $
                                          fluxNIR[idxIRAC1])^2)
           ENDIF
        ENDIF
        IF (input.ErrIRAC2 GT 0.D) THEN BEGIN
           idxIRAC2 = Where(filterNIR EQ 'IRAC2', cntIRAC2)
           IF (cntIRAC2 GT 0) THEN BEGIN
              sigmaNIR[idxIRAC2] = Sqrt(sigmaNIR[idxIRAC2]^2 + $
                                         (input.ErrIRAC2 * $
                                          fluxNIR[idxIRAC2])^2)
           ENDIF
        ENDIF
        name = [name, nameNIR]
        filter = [filter, filterNIR]
        wave = [wave, waveNIR]
        flux = [flux, fluxNIR]
        sigma = [sigma, sigmaNIR]
        statNIR = (Keyword_Set(limitNIR)) ? $
                  Replicate(1, countNIR) : (phot.Stat)[indexNIR]
        stat = [stat, statNIR]
        fitNIR = (phot.Fit)[indexNIR]
        plotNIR = (phot.Plot)[indexNIR]
        IF (input.FitNIR EQ 0) THEN fitNIR *= 0
        IF (input.PlotNIR EQ 0) THEN plotNIR *= 0
        fit = [fit, fitNIR]
        plot = [plot, plotNIR]
     ENDIF

     ;; MIR
     indexMIR = Where((phot.WRest GE waveIRS_Min) AND $
                      (phot.WRest LE waveIRS_Max), countMIR)
     IF (countMIR GT 0) THEN BEGIN
        nameMIR = (phot.Name)[indexMIR]
        filterMIR = (phot.Filter)[indexMIR]
        waveMIR = (phot.WRest)[indexMIR]
        fluxMIR = input.ScaleMIR * (phot.FRest)[indexMIR]
        sigmaMIR = input.ScaleMIR * (phot.EFRest)[indexMIR]
        IF (input.ErrMIR GT 0.D) THEN $
           sigmaMIR = Sqrt(sigmaMIR^2 + (input.ErrMIR * fluxMIR)^2)
        IF (input.ErrMIPS24 GT 0.D) THEN BEGIN
           idxMIPS24 = Where((nameMIR EQ 'MIPS24-12"') OR $
                             (nameMIR EQ 'MIPS24-70"'), cntMIPS24)
           IF (cntMIPS24 GT 0) THEN BEGIN
              sigmaMIR[idxMIPS24] = Sqrt(sigmaMIR[idxMIPS24]^2 + $
                                         (input.ErrMIPS24 * $
                                          fluxMIR[idxMIPS24])^2)
           ENDIF
        ENDIF
        name = [name, nameMIR]
        filter = [filter, filterMIR]
        wave = [wave, waveMIR]
        flux = [flux, fluxMIR]
        sigma = [sigma, sigmaMIR]
        stat = [stat, (phot.Stat)[indexMIR]]
        fitMIR = (phot.Fit)[indexMIR]
        plotMIR = (phot.Plot)[indexMIR]
        IF (input.FitMIR EQ 0) THEN fitMIR *= 0
        IF (input.PlotMIR EQ 0) THEN plotMIR *= 0
        fit = [fit, fitMIR]
        plot = [plot, plotMIR]
     ENDIF

     ;; FIR
     indexFIR = Where((phot.WRest GT waveIRS_Max) AND $
                      (phot.WRest LE 1.3D3), countFIR)
     IF (countFIR GT 0) THEN BEGIN
        nameFIR = (phot.Name)[indexFIR]
        filterFIR = (phot.Filter)[indexFIR]
        waveFIR = (phot.WRest)[indexFIR]
        fluxFIR = input.ScaleFIR * (phot.FRest)[indexFIR]
        sigmaFIR = input.ScaleFIR * (phot.EFRest)[indexFIR]
        IF (input.ErrFIR GT 0.D) THEN $
           sigmaFIR = Sqrt(sigmaFIR^2 + (input.ErrFIR * fluxFIR)^2)
        IF (input.ErrMIPS70 GT 0.D) THEN BEGIN
           idxMIPS70 = Where((nameFIR EQ 'MIPS70-16"') OR $
                             (nameFIR EQ 'MIPS70-70"'), cntMIPS70)
           IF (cntMIPS70 GT 0) THEN BEGIN
              sigmaFIR[idxMIPS70] = Sqrt(sigmaFIR[idxMIPS70]^2 + $
                                         (input.ErrMIPS70 * $
                                          fluxFIR[idxMIPS70])^2)
           ENDIF
        ENDIF
        IF (input.ErrMIPS160 GT 0.D) THEN BEGIN
           idxMIPS160 = Where((nameFIR EQ 'MIPS160-40"') OR $
                              (nameFIR EQ 'MIPS160-100"'), cntMIPS160)
           IF (cntMIPS160 GT 0) THEN BEGIN
              sigmaFIR[idxMIPS160] = Sqrt(sigmaFIR[idxMIPS160]^2 + $
                                          (input.ErrMIPS160 * $
                                           fluxFIR[idxMIPS160])^2)
           ENDIF
        ENDIF
        refFIR = (phot.Reference)[indexFIR]
        name = [name, nameFIR]
        filter = [filter, filterFIR]
        wave = [wave, waveFIR]
        flux = [flux, fluxFIR]
        sigma = [sigma, sigmaFIR]
        fitFIR = (phot.Fit)[indexFIR]
        plotFIR = (phot.Plot)[indexFIR]
        statFIR = (Keyword_Set(limitFIR)) ? $
                  Replicate(1, countFIR) : (phot.Stat)[indexFIR]
        IF (Keyword_Set(limitFIR)) THEN BEGIN
           idxMIPS70 = Where(nameFIR EQ 'MIPS70-16"', cntMIPS70)
           IF (cntMIPS70 EQ 1) THEN BEGIN
              IF (fluxFIR[idxMIPS70] GT (3.D * sigmaFIR[idxMIPS70])) THEN $
                 statFIR[idxMIPS70] = 0
              fitFIR[idxMIPS70] = 1
              plotFIR[idxMIPS70] = 1
           ENDIF
           idxMIPS160 = Where(nameFIR EQ 'MIPS160-40"', cntMIPS160)
           IF (cntMIPS160 EQ 1) THEN BEGIN
              IF (fluxFIR[idxMIPS160] GT (3.D * sigmaFIR[idxMIPS160])) THEN $
                 statFIR[idxMIPS160] = 0
              fitFIR[idxMIPS160] = 1
              plotFIR[idxMIPS160] = 1
           ENDIF
           idxMIPS160 = Where(nameFIR EQ 'MIPS160-100"', cntMIPS160)
           IF (cntMIPS160 EQ 1) THEN BEGIN
              fitFIR[idxMIPS160] = 0
              plotFIR[idxMIPS160] = 0
           ENDIF
        ENDIF
        IF (Keyword_Set(forceMIPS)) THEN BEGIN
           idxMIPS70 = Where(nameFIR EQ 'MIPS70-70"', cntMIPS70)
           IF (cntMIPS70 EQ 1) THEN BEGIN
              fitFIR[idxMIPS70] = 1
              plotFIR[idxMIPS70] = 1
           ENDIF
           idxMIPS160 = Where(nameFIR EQ 'MIPS160-100"', cntMIPS160)
           IF (cntMIPS160 EQ 1) THEN BEGIN
              fitFIR[idxMIPS160] = 1
              plotFIR[idxMIPS160] = 1
           ENDIF
        ENDIF
        IF (input.FitFIR EQ 0) THEN fitFIR *= 0
        IF (input.PlotFIR EQ 0) THEN plotFIR *= 0
        fit = [fit, fitFIR]
        plot = [plot, plotFIR]
        stat = [stat, statFIR]
     ENDIF

     ;; RAD
     indexRAD = Where(phot.WRest GT 1.3D3, countRAD)
     IF (countRAD GT 0) THEN BEGIN
        nameRAD = (phot.Name)[indexRAD]
        filterRAD = (phot.Filter)[indexRAD]
        waveRAD = (phot.WRest)[indexRAD]
        fluxRAD = input.ScaleRAD * (phot.FRest)[indexRAD]
        sigmaRAD = input.ScaleRAD * (phot.EFRest)[indexRAD]
        IF (input.ErrRAD GT 0.D) THEN $
           sigmaRAD = Sqrt(sigmaRAD^2 + (input.ErrRAD * fluxRAD)^2)
        name = [name, nameRAD]
        filter = [filter, filterRAD]
        wave = [wave, waveRAD]
        flux = [flux, fluxRAD]
        sigma = [sigma, sigmaRAD]
        stat = [stat, (phot.Stat)[indexRAD]]
        fitRAD = (phot.Fit)[indexRAD]
        plotRAD = (phot.Plot)[indexRAD]
        IF (input.FitRAD EQ 0) THEN fitRAD *= 0
        IF (input.PlotRAD EQ 0) THEN plotRAD *= 0
        fit = [fit, fitRAD]
        plot = [plot, plotRAD]
     ENDIF

  ENDIF

  ;; Get rid of ill-defined wavelength data.
  idx = Where(wave GT 0.D, cnt)
  name = name[idx]
  filter = filter[idx]
  wave = wave[idx]
  flux = flux[idx]
  sigma = sigma[idx]
  stat = stat[idx]
  fit = fit[idx]
  plot = plot[idx]

  ;; Sort by wavelength.
  idx = Sort(wave)
  name = name[idx]
  filter = filter[idx]
  wave = wave[idx]
  flux = flux[idx]
  sigma = sigma[idx]
  stat = stat[idx]
  fit = fit[idx]
  plot = plot[idx]

  ;; Create index vectors.
  indexOPT = Where(wave LT 1.D, countOPT)
  indexNIR = Where((wave GE 1.D) AND (wave LT waveIRS_Min), countNIR)
  indexMIR = Where((wave GE waveIRS_Min) AND $
                   (wave LE waveIRS_Max) AND $
                   (name NE 'IRS'), countMIR)
  indexIRS = Where(name EQ 'IRS', countIRS)
  indexFIR = Where((wave GT waveIRS_Max) AND (wave LE 1.3D3), countFIR)
  indexRAD = Where(wave GT 1.3D3, countRAD)
  index = { OPT:indexOPT, NIR:indexNIR, MIR:indexMIR, $
            IRS:indexIRS, FIR:indexFIR, RAD:indexRAD }

  ;; Change all upper-limits to 3*sigma style.
  idxUL = Where((stat EQ 1L) AND (flux GT (3.D * sigma)), cntUL)
  IF (cntUL GT 0) THEN BEGIN
     sigma[idxUL] = flux[idxUL] / 3.D
     flux[idxUL] = 0.D
  ENDIF

  ;; Get vector of weights.
  weights = Replicate(0.D, N_Elements(wave))
  idx = Where((fit EQ 1L) AND (stat NE -1L))
  thisWave = wave[idx]
  thisName = name[idx]
  thisSigma = sigma[idx]
  thisIdxOPT = Where(thisWave LT 1.D)
  thisIdxNIR = Where((thisWave GE 1.D) AND (thisWave LT waveIRS_Min))
  thisIdxMIR = Where((thisWave GE waveIRS_Min) AND $
                     (thisWave LE waveIRS_Max) AND $
                     (thisName NE 'IRS'))
  thisIdxIRS = Where(thisName EQ 'IRS')
  thisIdxFIR = Where((thisWave GT waveIRS_Max) AND (thisWave LE 1.3D3))
  thisIdxRAD = Where(thisWave GT 1.3D3)
  thisIdx = { OPT:thisIdxOPT, NIR:thisIdxNIR, MIR:thisIdxMIR, $
              IRS:thisIdxIRS, FIR:thisIdxFIR, RAD:thisIdxRAD }
  weights[idx] = Cafe_Weights(thisWave, thisIdx, input)

  ;; Set fit flag to null for wavelengths with zero weight.
  idx = Where((weights EQ 0.D) AND (stat NE -1L), cnt)
  IF (cnt GT 0) THEN fit[idx] = 0L

  ;; Create data structure.
  DAT = { Name:name, $
          Filter:filter, $
          Wave:wave, $
          Flux:flux, $
          Sigma:sigma, $
          Stat:stat, $
          Fit:fit, $
          Plot:plot, $
          Weights:weights, $
          Index:index }

  ;; Create arrays of data to fit.
  indexFit = Where(fit EQ 1, cntFit)
  nameFit = name[indexFit]
  filterFit = filter[indexFit]
  waveFit = wave[indexFit]
  fluxFit = flux[indexFit]
  sigmaFit = sigma[indexFit]
  statFit = stat[indexFit]
  weightsFit = weights[indexFit]

  ;; Add data points for special case when lower+upper limits are given.
  fLim = 2.D
  waves = waveFit[Uniq(waveFit, Sort(waveFit))]
  _cnt = 0
  FOR i=0,N_Elements(waves)-1 DO BEGIN
     idx = Where(waveFit EQ waves[i], cntFit)
     ;; Add "Fit-between-limits" data points...
     IF (cntFit EQ 2) THEN BEGIN
        idxUL = Where(statFit[idx] EQ 1L, cntUL)
        idxLL = Where(statFit[idx] EQ -1L, cntLL)
        IF ((cntUL EQ 1) AND (cntLL EQ 1)) THEN BEGIN
           nameFit0 = nameFit[idx[0]] + '-(LIMIT)'
           _nameFit = (N_Elements(_nameFit) EQ 0) ? $
                      [nameFit0] : [_nameFit, nameFit0]
           _filterFit = (N_Elements(_filterFit) EQ 0) ? $
                        [filterFit[idx[0]]] : [_filterFit, filterFit[idx[0]]]
           _waveFit = (N_Elements(_waveFit) EQ 0) ? $
                      [waveFit[idx[0]]] : [_waveFit, waveFit[idx[0]]]
           fluxFitMin = (fluxFit[idx])[idxLL]
           fluxFitMax = 3.D * (sigmaFit[idx])[idxUL]
           fluxFit0 = 0.5D * (fluxFitMin + fluxFitMax)
           sigmaFit0 = (fluxFitMax - fluxFit0) / (3.D / fLIM)
           _fluxFit = (N_Elements(_fluxFit) EQ 0) ? $
                      [fluxFit0] : [_fluxFit, fluxFit0]
           _sigmaFit = (N_Elements(_sigmaFit) EQ 0) ? $
                       [sigmaFit0] : [_sigmaFit, sigmaFit0]
           _statFit = (N_Elements(_statFit) EQ 0) ? [0] : [_statFit, 0]
           _weightsFit = (N_Elements(_weightsFit) EQ 0) ? $
                         [Mean(weightsFit[idx])] : $
                         [_weightsFit, Mean(weightsFit[idx])]
           _cnt++
        ENDIF ELSE $
           _idx = (N_Elements(_idx) EQ 0) ? idx : [_idx, idx]
     ENDIF ELSE $
        _idx = (N_Elements(_idx) EQ 0) ? idx : [_idx, idx]
  ENDFOR
  IF (_cnt GT 0) THEN BEGIN
     nameFit = [nameFit[_idx], _nameFit]
     filterFit = [filterFit[_idx], _filterFit]
     waveFit = [waveFit[_idx], _waveFit]
     fluxFit = [fluxFit[_idx], _fluxFit]
     sigmaFit = [sigmaFit[_idx], _sigmaFit]
     statFit = [statFit[_idx], _statFit]
     weightsFit = [weightsFit[_idx], _weightsFit]
     idx = Sort(waveFit)
     nameFit = nameFit[idx]
     filterFit = filterFit[idx]
     waveFit = waveFit[idx]
     fluxFit = fluxFit[idx]
     sigmaFit = sigmaFit[idx]
     statFit = statFit[idx]
     weightsFit = weightsFit[idx]
  ENDIF

  ;; Create indices into fit vectors.
  indexFitOPT = Where(waveFit LT 1.D, countFitOPT)
  indexFitNIR = Where((waveFit GE 1.D) AND $
                      (waveFit LT waveIRS_Min), countFitNIR)
  indexFitMIR = Where((waveFit GE waveIRS_Min) AND $
                      (waveFit LE waveIRS_Max) AND $
                      (nameFit NE 'IRS'), countFitMIR)
  indexFitIRS = Where(nameFit EQ 'IRS', countFitIRS)
  indexFitFIR = Where((waveFit GT waveIRS_Max) AND $
                      (waveFit LE 1.3D3), countFitFIR)
  indexFitRAD = Where(waveFit GT 1.3D3, countFitRAD)

  ;; Gather filters list for synthetic photometry.
  filterList = filterFit[Uniq(filterFit, Sort(filterFit))]
  idxFilters = Where((filterList NE '') AND (filterList NE '...'), cntFilters)
  IF (cntFilters EQ 0) THEN BEGIN
     input.DoFilter = 0
     filters = 0
  ENDIF $
  ELSE BEGIN
     filterList = StrLowCase(filterList[idxFilters])
     filters = Jam_SynPhot(FilterList=filterList, /Initialize)
  ENDELSE

  ;; Perform LIN and/or PAH fitting?
  fitPAHs = input.FitPAHs
  fitLINs = input.FitLINs
  IF (countFitIRS EQ 0) THEN BEGIN
     fitPAHs = 0L
     fitLINs = 0L
  ENDIF

  ;; Create IRS-SED wavelength vector.
  waveIRS_SED = (N_Elements(waveIRS) GT 10) ? $
                Jam_Dist(N_Elements(waveIRS)*3, Min(waveIRS), Max(waveIRS)) : $
                Jam_Dist(400, 5.D, 40.D)
  samplingIRS = ALog10(Max(waveIRS_SED) / Min(waveIRS_SED)) / $
                Double(N_Elements(waveIRS_SED))

  ;; Create UV wavelength vector.
  samplingUV = 20.D
  idx_UV = Where((plot EQ 1) OR (fit EQ 1))
  MIN_UV = Min(wave[idx_UV]) < 1D-3
  MAX_UV = 0.3D
  N_UV = Ceil(ALog10(MAX_UV / MIN_UV) / (samplingUV * samplingIRS))
  waveUV = Jam_Dist(N_UV, MIN_UV, MAX_UV)

  ;; Create OPT-NIR wavelength vector.
  samplingNIR = 4.D
  MIN_NIR = 0.3D
  MAX_NIR = waveIRS_SED[0] - (waveIRS_SED[1] - waveIRS_SED[0])
  N_NIR = Ceil(ALog10(MAX_NIR / MIN_NIR) / (samplingNIR * samplingIRS))
  waveNIR = Jam_Dist(N_NIR, MIN_NIR, MAX_NIR)

  ;; Create FIR-RAD wavelength vector.
  samplingFIR = 4.D
  N_IRS = N_Elements(waveIRS_SED)
  MIN_FIR = waveIRS_SED[N_IRS-1] + $
            (waveIRS_SED[N_IRS-1] - waveIRS_SED[N_IRS-2])
  idx_FIR = Where((plot EQ 1) OR (fit EQ 1))
  MAX_FIR = Max(wave[idx_FIR]) > 1.3D3
  N_FIR = Ceil(ALog10(MAX_FIR / MIN_FIR) / (samplingFIR * samplingIRS))
  waveFIR = Jam_Dist(N_FIR, MIN_FIR, MAX_FIR)

  ;; Create full SED wavelength vector.
  waveSED = [waveUV, waveNIR, waveIRS_SED, waveFIR]
  waveSED = waveSED[Sort(waveSED)]
  N_SED = N_Elements(waveSED)

  ;; Calculate indices into OPT, NIR, FIR, RAD, and IRS portions of vector.
  indexSEDOPT = Where(waveSED LT 1.D, N_OPT)
  indexSEDNIR = Where((waveSED GE 1.D) AND (waveSED LT waveIRS_Min), N_NIR)
  indexSEDIRS = Where((waveSED GE waveIRS_Min) AND $
                      (waveSED LE waveIRS_Max), N_IRS)
  indexSEDFIR = Where((waveSED GT waveIRS_Max) AND (waveSED LE 1.3D3), N_FIR)
  indexSEDRAD = Where(waveSED GT 1.3D3, N_RAD)

  ;; Calculate dust, water ice, and HAC opacities.
  IF (input.Draine_Or_OHMc EQ 'OHMc') THEN begin
	  scaleOHMc = JAM_ReadData(FilePath('ohmc_scale.txt', $
   	                                 Root_Dir=!JAM.Path.Tables))
  ENDIF
  K_DST = Jam_GrainOpacity(waveSED, 0.D, K_Ext=K_EXT, C_Table=C_Table, $
                           T_Table=T_Table, dnda_Table=dnda_Table, $
                           NoPAH=~fitPAHs, /Big, ScaleSIL=scaleOHMc)
  K_ICE = Jam_IceOpacity(waveSED)
  K_HAC = Jam_HACOpacity(waveSED)

  ;; Calculate emissivities.
  IF ((input.Source_HOT EQ 'AGN') OR $
      (input.Source_WRM EQ 'AGN') OR $
      (input.Source_COO EQ 'AGN') OR $
      (input.Source_CLD EQ 'AGN') OR $
      (input.Source_CIR EQ 'AGN')) THEN BEGIN
     E_AGN = Jam_GrainEmissivity(waveSED, T_bb, C_Table=C_Table, $
                                 T_Table=T_Table, dnda_Table=dnda_Table, $
                                 SourceType='AGN', /Big, ScaleSIL=scaleOHMc)
  ENDIF
  IF ((input.Source_HOT EQ 'SB2Myr') OR $
      (input.Source_WRM EQ 'SB2Myr') OR $
      (input.Source_COO EQ 'SB2Myr') OR $
      (input.Source_CLD EQ 'SB2Myr') OR $
      (input.Source_CIR EQ 'SB2Myr')) THEN BEGIN
     E_SB2Myr = Jam_GrainEmissivity(waveSED, T_bb, C_Table=C_Table, $
                                    T_Table=T_Table, dnda_Table=dnda_Table, $
                                    SourceType='SB2Myr', /Big, ScaleSIL=scaleOHMc)
  ENDIF
  IF ((input.Source_HOT EQ 'SB10Myr') OR $
      (input.Source_WRM EQ 'SB10Myr') OR $
      (input.Source_COO EQ 'SB10Myr') OR $
      (input.Source_CLD EQ 'SB10Myr') OR $
      (input.Source_CIR EQ 'SB10Myr')) THEN BEGIN
     E_SB10Myr = Jam_GrainEmissivity(waveSED, T_bb, C_Table=C_Table, $
                                     T_Table=T_Table, dnda_Table=dnda_Table, $
                                     SourceType='SB10Myr', /Big, ScaleSIL=scaleOHMc)
  ENDIF
  IF ((input.Source_HOT EQ 'SB100Myr') OR $
      (input.Source_WRM EQ 'SB100Myr') OR $
      (input.Source_COO EQ 'SB100Myr') OR $
      (input.Source_CLD EQ 'SB100Myr') OR $
      (input.Source_CIR EQ 'SB100Myr')) THEN BEGIN
     E_SB100Myr = Jam_GrainEmissivity(waveSED, T_bb, C_Table=C_Table, $
                                      T_Table=T_Table, dnda_Table=dnda_Table, $
                                      SourceType='SB100Myr', /Big, ScaleSIL=scaleOHMc)
  ENDIF
  IF ((input.Source_HOT EQ 'ISRF') OR $
      (input.Source_WRM EQ 'ISRF') OR $
      (input.Source_COO EQ 'ISRF') OR $
      (input.Source_CLD EQ 'ISRF') OR $
      (input.Source_CIR EQ 'ISRF')) THEN BEGIN
     E_ISRF = Jam_GrainEmissivity(waveSED, T_bb, C_Table=C_Table, $
                                  T_Table=T_Table, dnda_Table=dnda_Table, $
                                  SourceType='ISRF', /Big, ScaleSIL=scaleOHMc)
  ENDIF
  CASE (input.Source_HOT) OF
     'AGN': E_HOT = E_AGN
     'SB2Myr': E_HOT = E_SB2Myr
     'SB10Myr': E_HOT = E_SB10Myr
     'SB100Myr': E_HOT = E_SB100Myr
     'ISRF': E_HOT = E_ISRF
  ENDCASE
  CASE (input.Source_WRM) OF
     'AGN': E_WRM = E_AGN
     'SB2Myr': E_WRM = E_SB2Myr
     'SB10Myr': E_WRM = E_SB10Myr
     'SB100Myr': E_WRM = E_SB100Myr
     'ISRF': E_WRM = E_ISRF
  ENDCASE
  CASE (input.Source_COO) OF
     'AGN': E_COO = E_AGN
     'SB2Myr': E_COO = E_SB2Myr
     'SB10Myr': E_COO = E_SB10Myr
     'SB100Myr': E_COO = E_SB100Myr
     'ISRF': E_COO = E_ISRF
  ENDCASE
  CASE (input.Source_CLD) OF
     'AGN': E_CLD = E_AGN
     'SB2Myr': E_CLD = E_SB2Myr
     'SB10Myr': E_CLD = E_SB10Myr
     'SB100Myr': E_CLD = E_SB100Myr
     'ISRF': E_CLD = E_ISRF
  ENDCASE
  CASE (input.Source_CIR) OF
     'AGN': E_CIR = E_AGN
     'SB2Myr': E_CIR = E_SB2Myr
     'SB10Myr': E_CIR = E_SB10Myr
     'SB100Myr': E_CIR = E_SB100Myr
     'ISRF': E_CIR = E_ISRF
  ENDCASE

  ;; Calculate source component SEDs.
  source2Myr = Jam_SourceSED(waveSED, 'SB2Myr', /Jy)
  source10Myr = Jam_SourceSED(waveSED, 'SB10Myr', /Jy)
  source100Myr = Jam_SourceSED(waveSED, 'SB100Myr', /Jy)
  sourceSTR = Jam_SourceSED(waveSED, 'ISRF', /Jy)
  sourceDSK = Jam_SourceSED(waveSED, 'AGN', /Jy)
  const = 3D14 / waveSED
  source2Myr /= TSum(ALog(waveSED), const * source2Myr)
  source10Myr /= TSum(ALog(waveSED), const * source10Myr)
  source100Myr /= TSum(ALog(waveSED), const * source100Myr)
  sourceSTR /= TSum(ALog(waveSED), const * sourceSTR)
  sourceDSK /= TSum(ALog(waveSED), const * sourceDSK)

  ;; Create a structure of reference wavelengths.
  waveFitMin = Min(waveFit, Max=waveFitMax)
  IF (Jam_Check(input.Wave_CIR, TName='STRING')) THEN BEGIN
     idxWave = Where(filterFit EQ input.Wave_CIR, cntWave)
     waveCIR = (cntWave EQ 1) ? (waveFit[idxWave])[0] : 100.D
  ENDIF ELSE $
     waveCIR = input.Wave_CIR
  IF (Jam_Check(input.Wave_CLD, TName='STRING')) THEN BEGIN
     idxWave = Where(filterFit EQ input.Wave_CLD, cntWave)
     waveCLD = (cntWave EQ 1) ? (waveFit[idxWave])[0] : 100.D
  ENDIF ELSE $
     waveCLD = input.Wave_CLD
  IF (Jam_Check(input.Wave_COO, TName='STRING')) THEN BEGIN
     idxWave = Where(filterFit EQ input.Wave_COO, cntWave)
     waveCOO = (cntWave EQ 1) ? (waveFit[idxWave])[0] : 35.D
  ENDIF ELSE $
     waveCOO = input.Wave_COO
  IF (Jam_Check(input.Wave_WRM, TName='STRING')) THEN BEGIN
     idxWave = Where(filterFit EQ input.Wave_WRM, cntWave)
     waveWRM = (cntWave EQ 1) ? (waveFit[idxWave])[0] : 15.D
  ENDIF ELSE $
     waveWRM = input.Wave_WRM
  IF (Jam_Check(input.Wave_HOT, TName='STRING')) THEN BEGIN
     idxWave = Where(filterFit EQ input.Wave_HOT, cntWave)
     waveHOT = (cntWave EQ 1) ? (waveFit[idxWave])[0] : 3.5D
  ENDIF ELSE $
     waveHOT = input.Wave_HOT
  IF (Jam_Check(input.Wave_STR, TName='STRING')) THEN BEGIN
     idxWave = Where(filterFit EQ input.Wave_STR, cntWave)
     waveSTR = (cntWave EQ 1) ? (waveFit[idxWave])[0] : 1.1D
  ENDIF ELSE $
     waveSTR = input.Wave_STR
  IF (Jam_Check(input.Wave_PAH, TName='STRING')) THEN BEGIN
     idxWave = Where(filterFit EQ input.Wave_PAH, cntWave)
     wavePAH = (cntWave EQ 1) ? (waveFit[idxWave])[0] : 1.1D
  ENDIF ELSE $
     wavePAH = input.Wave_PAH
  IF (N_Elements(wave0) EQ 0) THEN BEGIN
     wave0 = { CIR:waveCIR > waveFitMin < waveFitMax, $
               CLD:waveCLD > waveFitMin < waveFitMax, $
               COO:waveCOO > waveFitMin < waveFitMax, $
               WRM:waveWRM > waveFitMin < waveFitMax, $
               HOT:waveHOT > waveFitMin < waveFitMax, $
               STR:waveSTR > waveFitMin < waveFitMax, $
               PAH:wavePAH }
  ENDIF

  ;; Create a structure of reference fluxes.
  fluxFit0 = fluxFit
  idx0 = Where(fluxFit0 EQ 0.D, cnt0)
  IF (cnt0 GT 0) THEN fluxFit0[idx0] = 3.D * sigmaFit[idx0]
  logWaveFit = ALog(waveFit)
  minFlux0 = Min(fluxFit0)
  IF (N_Elements(flux0) EQ 0) THEN BEGIN
     flux0 = { CIR:Interpol(fluxFit0, logWaveFit, ALog(wave0.CIR)) > minFlux0, $
               CLD:Interpol(fluxFit0, logWaveFit, ALog(wave0.CLD)) > minFlux0, $
               COO:Interpol(fluxFit0, logWaveFit, ALog(wave0.COO)) > minFlux0, $
               WRM:Interpol(fluxFit0, logWaveFit, ALog(wave0.WRM)) > minFlux0, $
               HOT:Interpol(fluxFit0, logWaveFit, ALog(wave0.HOT)) > minFlux0, $
               STR:Interpol(fluxFit0, logWaveFit, ALog(wave0.STR)) > minFlux0, $
               PAH:0.D }
  ENDIF

  ;; Create a vector of fit parameter names and values.
  tagNames = Tag_Names(input)
  paramIndex = Where(StrMid(tagNames, 0, 1) EQ '_', nParams)
  paramNames = StrMid(tagNames[paramIndex], 1)
  paramTied = IndGen(nParams)
  paramValues = DblArr(nParams)
  FOR i=0,nParams-1 DO paramValues[i] = input.(paramIndex[i])

  ;; Create MPFit parameter info structure.
  pi = Replicate( { Fixed:0, Limited:[1,0], Limits:[0.D,0.D], ParName:'', $
                    Tied:'', Value:1.D, RelStep:0.D, Step:0.D }, nParams)

  ;; Fill out parameter info structure.
  pi.ParName = paramNames
  FOR i=0,nParams-1 DO BEGIN
     indexP = Where(tagNames EQ ('FIX_' + paramNames[i]), countP)
     IF (countP EQ 1) THEN pi[i].Fixed = input.(indexP)
     indexP = Where(tagNames EQ ('MIN_' + paramNames[i]), countP)
     IF (countP EQ 1) THEN BEGIN
        pi[i].Limited[0] = 1
        pi[i].Limits[0] = input.(indexP)
     ENDIF
     indexP = Where(tagNames EQ ('MAX_' + paramNames[i]), countP)
     IF (countP EQ 1) THEN BEGIN
        pi[i].Limited[1] = 1
        pi[i].Limits[1] = input.(indexP)
     ENDIF
     indexP = Where(tagNames EQ ('TIE_' + paramNames[i]), countP)
     IF (countP EQ 1) THEN BEGIN
        IF (input.(indexP) NE '') THEN BEGIN
           paramTied[i] = Where(paramNames EQ input.(indexP))
           pi[i].Tied = 'P[' + String(paramTied[i]) + ']'
        ENDIF
     ENDIF
     indexP = Where(tagNames EQ ('REL_' + paramNames[i]), countP)
     IF (countP EQ 1) THEN pi[i].RelStep = input.(indexP)
     indexP = Where(tagNames EQ ('STP_' + paramNames[i]), countP)
     IF (countP EQ 1) THEN pi[i].Step = input.(indexP)
  ENDFOR
  p = Cafe_Params(paramValues, paramTied)
  FOR i=0,N_Tags(p)-1 DO paramValues[i] = p.(i)
  pi.Value = paramValues
  values = paramValues

  ;; Create parameter index vectors.
  param3 = StrMid(paramNames, 0, 3)
  idxICE = Where(param3 EQ 'TAU')
  idxCIR = Where(param3 EQ 'CIR')
  idxCLD = Where(param3 EQ 'CLD')
  idxCOO = Where(param3 EQ 'COO')
  idxWRM = Where(param3 EQ 'WRM')
  idxHOT = Where(param3 EQ 'HOT')
  idxSTR = Where(param3 EQ 'STR')
  idxSTB = Where(param3 EQ 'STB')
  idxDSK = Where(param3 EQ 'DSK')
  idxPAH = Where(param3 EQ 'PAH')
  idxCIR = [idxCIR, idxICE]
  idxCLD = [idxCLD, idxICE]
  idxCOO = [idxCOO, idxICE]
  idxWRM = [idxWRM, idxICE]
  idxHOT = [idxHOT, idxICE]
  idxSTR = [idxSTR, idxICE]
  idxSTB = [idxSTB, idxICE]
  idxDSK = [idxDSK, idxICE]
  idxPAH = [idxPAH, idxICE]
  idxSRC = [idxSTR, idxSTB, idxDSK, idxICE]
  idxDST = [idxCIR, idxCLD, idxCOO, idxWRM, idxHOT, idxICE]
  idxCON = [idxDST, idxSRC, idxICE]

  ;; Obtain initial feature profiles.
  minWave = Min(waveFit[indexFitIRS], Max=maxWave)
  paramFTR = Jam_FitFeatures(waveFit, fluxFit, sigmaFit, SigDat=sigmaFit, $
                             Redshift=z, MinWave=minWave, MaxWave=maxWave, $
                             Wave0PAH0=wave0.PAH, /Initialize, NoPAH3=noPAH3)
  idxLIN = Where(paramFTR.Profile EQ 'G', cntLIN, Complement=idxPAH)
  IF (cntLIN EQ 0) THEN BEGIN
     fitLINs = 0
     gauss = { Wave0:-1.D }
  ENDIF $
  ELSE BEGIN
     gauss = { Name:paramFTR.Name[idxLIN], Wave0:paramFTR.Wave0[idxLIN], $
               Gamma:paramFTR.Gamma[idxLIN], Peak:paramFTR.Peak[idxLIN] }
  ENDELSE
  drude = { Wave0:paramFTR.Wave0[idxPAH], Gamma:paramFTR.Gamma[idxPAH], $
            Peak:paramFTR.Peak[idxPAH], Complex:paramFTR.Complex[idxPAH] }
  wavePAH = Jam_Dist(500, 4.D, 20.D)
  flux0.PAH = Interpol(Jam_DrudeFlux(wavePAH, drude), $
                       ALog(wavePAH), ALog(wave0.PAH))

  ;; Create a structure to pass information to the fit procedure.
  functArgs = { Input:input, $
                FilterDat:filterFit, $
                Filters:filters, $
                Wave0:wave0, $
                Flux0:flux0, $
                WaveMOD:waveSED, $
                E_HOT:E_HOT, $
                E_WRM:E_WRM, $
                E_COO:E_COO, $
                E_CLD:E_CLD, $
                E_CIR:E_CIR, $
                K_DST:K_DST, $
                K_EXT:K_EXT, $
                K_ICE:K_ICE, $
                K_HAC:K_HAC, $
                SourceSTR:sourceSTR, $
                Source2Myr:source2Myr, $
                Source10Myr:source10Myr, $
                Source100Myr:source100Myr, $
                SourceDSK:sourceDSK, $
                Gauss:gauss, $
                Drude:drude, $
                z:z, $
                Debug:Keyword_Set(debug) }

  ;; Calculate weighting.
  theseWeights = weightsFit / sigmaFit^2

  ;; Perform the fit with MPFitFun.
  msg = StrCompress('<CAFE - Continuum Fit>')
  log = (N_Elements(log) EQ 0) ? [msg] : [log, msg]
  IF (~Keyword_Set(quiet)) THEN Print, msg
  MP_Quiet = (Keyword_Set(quiet)) ? 1 : input.MP_Quiet
  values = MPFitFun('Cafe_Flux', waveFit, fluxFit, $
                    FunctArgs=functArgs, FTol=input.MP_EPS, $
                    MaxIter=input.MP_MaxIter, ParInfo=pi, $
                    PError=pError, Quiet=MP_Quiet, Status=status, $
                    NFree=nFree, Weights=theseWeights, Covar=covar, $
                    BestNorm=bestNorm, DOF=DOF, ErrMsg=errMsg)

  ;; Check for fit errors.
  IF (status LE 0) THEN BEGIN
     msg = 'MPFitFun: ' + errMsg
     log = [log, msg]
     IF (~Keyword_Set(quiet)) THEN Print, msg
     Message, Jam_Message('MPFitFun', $
                          StrCompress('Status = ' + String(status)))
  ENDIF

  ;; Update parameter errors for tied parameters.
  FOR i=0,nParams-1 DO pError[i] = pError[paramTied[i]]

  ;; Obtain fit components.
  fluxCAFE = Cafe_Flux(waveFit, values, Com=com, Ext=ext, Tau0=tau0, $
                       ModCom=modCom, fPAH0_Tot=fPAH0_Tot, _Extra=functArgs)

  ;; Set total PAH flux in functArgs.
  functArgs = Create_Struct(functArgs, 'fPAH0_Tot', fPAH0_Tot)

  ;; Calculate chi-squared values.
  indexFitStat = Where(statFit EQ 0L, countFitStat)
  chiSqr = ((fluxFit - fluxCAFE) / sigmaFit)^2
  chiSqrFit = theseWeights * (fluxFit - fluxCAFE)^2
  chiSqrSED = Total(chiSqrFit);;[indexFitStat])
  chiSqrIRS = (countFitIRS GT 0) ? Total(chiSqr[indexFitIRS]) : 0.D
  DOF_SED = N_Elements(chiSqrFit) - nFree;;countFitStat - nFree
  DOF_IRS = countFitIRS > 0.D

  ;; Print chi2 values.
  msg = StrCompress(' -> Chi2/DOF = ' + String(chiSqrSED / DOF_SED))
  log = [log, msg]
  IF (~Keyword_Set(quiet)) THEN Print, msg
  IF (countFitIRS GT 0) THEN BEGIN
     msg = StrCompress(' -> Chi2/N(IRS) = ' + String(chiSqrIRS / DOF_IRS))
     log = [log, msg]
     IF (~Keyword_Set(quiet)) THEN Print, msg
  ENDIF

  ;; Obtain parameter values and errors structures.
  p = Cafe_Params(values)
  pE = Cafe_Params(pError)

  ;; Obtain fit errrors.
  ;; Note: These numerical derivative routines could certainly have some
  ;;       "problems" in certain cases. There are no doubt more robust
  ;;       finite-difference (I think that's the right term) algorithms
  ;;       for handling this.
  derivsCAFE = Jam_Deriv('Cafe_EFitFlux', waveFit, values, $
                         _Extra=functArgs, PError=pError, Covar=covar)
  errFluxCON = Jam_EFunc(derivsCAFE, pError, Covar=covar, Index=idxCON, $
                         Tag='CON_FLX')
  errExtPAH = Jam_EFunc(derivsCAFE, pError, Covar=covar, Index=idxPAH, $
                        Tag='PAH_EXT')

  ;; Perform LIN+PAH fit.
  chiSqrFTR = 0.D
  DOF_FTR = -1
  IF (fitPAHs OR fitLINs) THEN BEGIN
     msg = '<CAFE - LIN+PAH Fit>'
     log = [log, msg]
     IF (~Keyword_Set(quiet)) THEN Print, msg
     fFTR = (fluxFit - com.CON) > 0.D
     thisSigmaFTR = sigmaFit;;Sqrt(sigmaFit^2 + errFluxCON^2)
     minWave = Min(waveFit[indexFitIRS], Max=maxWave)
     paramFTR = Jam_FitFeatures(waveFit, fFTR, thisSigmaFTR, $
                                F0_PAH=p.PAH_FLX, $
                                Wave0PAH0=wave0.PAH, $
                                SigDat=sigmaFit, Redshift=z, $
                                ExtPAH=ext.PAH, EExtPAH=errExtPAH, $
                                MinWave=minWave, MaxWave=maxWave, $
                                FixWave0_PAH=input.FixWave0_PAH, $
                                FixGamma_PAH=input.FixGamma_PAH, $
                                FixWave0_LIN=input.FixWave0_LIN, $
                                FixGamma_LIN=input.FixGamma_LIN, $
                                EPSWave0_PAH=input.EPSWave0_PAH, $
                                EPSGamma_PAH=input.EPSGamma_PAH, $
                                EPSPeak_PAH=input.EPSPeak_PAH, $
                                EPSWave0_LIN=input.EPSWave0_LIN, $
                                EPSGamma_LIN=input.EPSGamma_LIN, $
                                EPSPeak_LIN=input.EPSPeak_LIN, $
                                fPAH=fluxPAH, eFPAH=errFluxPAH, $
                                fLIN=fluxLIN, eFLIN=errFluxLIN, $
                                ChiSqr=chiSqrFTR, DOF=DOF_FTR, $
                                Drude=drude, EDrude=errDrude, $
                                Gauss=gauss, EGauss=errGauss, $
                                PowPAH=powerPAH, EPowPAH=errPowerPAH, $
                                Pow0PAH=power0PAH, EPow0PAH=errPower0PAH, $
                                PowLIN=powerLIN, EPowLIN=errPowerLIN, $
                                PowComPAH=cPowerPAH, EPowComPAH=errCPowerPAH, $
                                Pow0ComPAH=cPower0PAH, EPow0ComPAH=errCPower0PAH, $
                                RatioPAH=ratioPAH, ERatioPAH=errRatioPAH, $
                                Ratio0PAH=ratio0PAH, ERatio0PAH=errRatio0PAH, $
                                NoPAH3=noPAH3, Verbose=0)

     ;; Correct lines for extinction (same as PAH extinction).
     extCOR = Interpol(ext.PAH, waveFit, gauss.Wave0)
     errExtCOR = 0.D;;Interpol(errExtPAH, waveFit, gauss.Wave0)
     powerLIN /= extCOR
     errPowerLIN = Sqrt((errPowerLIN / extCOR)^2 + $
                        (powerLIN / extCOR^2 * errExtCOR)^2)

     ;; Check if the fit around each line is good.
     fRES = fluxFit - com.CON - fluxPAH - fluxLIN
     nLIN = N_Elements(powerLIN)
     FWHM = gauss.Gamma * gauss.Wave0
     N_SIGMA = 10.D
     scale = N_SIGMA / (2.D * Sqrt(2.D * ALog(2.D)))
     waveMin = gauss.Wave0 - scale * FWHM
     waveMax = gauss.Wave0 + scale * FWHM
     flagLIN = IntArr(nLIN)
     peakLIN = gauss.Peak
     gauss.Peak = peakLIN
     gauss = Create_Struct(gauss, 'Poor_Fit_Flag', flagLIN)
     functArgs = Jam_rmTag(functArgs, 'GAUSS')
     functArgs = Jam_rmTag(functArgs, 'DRUDE')
     functArgs = Jam_rmTag(functArgs, 'FLUX0')
     flux0.PAH = Interpol(fluxPAH, Alog(WaveFit), ALog(wave0.PAH))
     p.PAH_FLX = 1.D
     idxP = Where(paramNames EQ 'PAH_FLX', cntP)
     IF (cntP EQ 1) THEN values[idxP] = 1.D
     functArgs = Create_Struct(functArgs, 'GAUSS', gauss, 'DRUDE', drude, $
                               'FLUX0', flux0)

     msg = StrCompress(' -> Chi2/DOF(FTR) = '+ String(chiSqrFTR / DOF_FTR))
     log = [log, msg]
     IF (~Keyword_Set(quiet)) THEN Print, msg
  ENDIF $
  ELSE BEGIN
     msg = '<CAFE - No LIN+PAH Fit>'
     log = [log, msg]
     IF (~Keyword_Set(quiet)) THEN Print, msg
     void = 0.D * waveFit
     fluxPAH = void
     fluxLIN = void
     errFluxPAH = void
     errFluxLIN = void
     drude = { Wave0:-1.D }
     errDrude = { Wave0:-1.D }
     gauss = { Wave0:-1.D }
     errGauss = { Wave0:-1.D }
  ENDELSE

  ;; Obtain new fit components.
  fluxCAFE = Cafe_Flux(waveFit, values, Com=com, _Extra=functArgs)

  ;; Calculate final chi-squared values.
  chiSqr = ((fluxFit - fluxCAFE) / sigmaFit)^2
  chiSqrFit = theseWeights * (fluxFit - fluxCAFE)^2
  chiSqrSED = Total(chiSqrFit);;[indexFitStat])
  chiSqrOPT = (countFitOPT GT 0) ? Total(chiSqr[indexFitOPT]) : 0.D
  chiSqrNIR = (countFitNIR GT 0) ? Total(chiSqr[indexFitNIR]) : 0.D
  chiSqrMIR = (countFitMIR GT 0) ? Total(chiSqr[indexFitMIR]) : 0.D
  chiSqrIRS = (countFitIRS GT 0) ? Total(chiSqr[indexFitIRS]) : 0.D
  chiSqrFIR = (countFitFIR GT 0) ? Total(chiSqr[indexFitFIR]) : 0.D
  chiSqrRAD = (countFitRAD GT 0) ? Total(chiSqr[indexFitRAD]) : 0.D
  DOF_SED = N_Elements(chiSqrFit) - nFree;;countFitStat - nFree
  DOF_OPT = countFitOPT > 0.D
  DOF_NIR = countFitNIR > 0.D
  DOF_MIR = countFitMIR > 0.D
  DOF_IRS = countFitIRS > 0.D
  DOF_FIR = countFitFIR > 0.D
  DOF_RAD = countFitRAD > 0.D

  ;; Print final chi2 values.
  msg = StrCompress('<CAFE - Final Values>')
  log = [log, msg]
  IF (~Keyword_Set(quiet)) THEN Print, msg
  msg = StrCompress(' -> Chi2/DOF = ' + String(chiSqrSED / DOF_SED))
  log = [log, msg]
  IF (~Keyword_Set(quiet)) THEN Print, msg
  IF ((countFitOPT + countFitNIR) GT 0) THEN BEGIN
     msg = StrCompress(' -> Chi2/N(OPT+NIR) = ' + $
                       String((chiSqrOPT + chiSqrNIR) / $
                              (DOF_OPT + DOF_NIR)))
     log = [log, msg]
     IF (~Keyword_Set(quiet)) THEN Print, msg
  ENDIF
  IF ((countFitMIR + countFitIRS) GT 0) THEN BEGIN
     msg = StrCompress(' -> Chi2/N(IRS+MIR) = ' + $
                       String((chiSqrIRS + chiSqrMIR) / $
                              (DOF_IRS + DOF_MIR)))
     log = [log, msg]
     IF (~Keyword_Set(quiet)) THEN Print, msg
  ENDIF
  IF ((countFitFIR + countFitRAD) GT 0) THEN BEGIN
     msg = StrCompress(' -> Chi2/N(FIR+RAD) = ' + $
                       String((chiSqrFIR + chiSqrRAD) / $
                              (DOF_FIR + DOF_RAD)))
     log = [log, msg]
     IF (~Keyword_Set(quiet)) THEN Print, msg
  ENDIF

  ;; Get SED flux and components.
  functArgs.Input.DoFilter = 0
  fluxSED = Cafe_Flux(waveSED, values, E0=E0, Com=com, Ext=ext, $
                      ModCom=com0, _Extra=functArgs)

  ;; Calculate flux and extinction curve errors.
  dSED = Jam_Deriv('Cafe_EFitFlux', waveSED, values, $
                   PError=pError, Covar=covar, _Extra=functArgs)
  ;; Extinguished flux errors.
  sigmaCIR = Jam_EFunc(dSED, pError, Covar=covar, Index=idxCIR, Tag='CIR_FLX')
  sigmaCLD = Jam_EFunc(dSED, pError, Covar=covar, Index=idxCLD, Tag='CLD_FLX')
  sigmaCOO = Jam_EFunc(dSED, pError, Covar=covar, Index=idxCOO, Tag='COO_FLX')
  sigmaWRM = Jam_EFunc(dSED, pError, Covar=covar, Index=idxWRM, Tag='WRM_FLX')
  sigmaHOT = Jam_EFunc(dSED, pError, Covar=covar, Index=idxHOT, Tag='HOT_FLX')
  sigmaSTR = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSTR, Tag='STR_FLX')
  sigmaSTB = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSTB, Tag='STB_FLX')
  sigmaSTB_100 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSTB, $
                           Tag='STB_100_FLX')
  sigmaSTB_010 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSTB, $
                           Tag='STB_010_FLX')
  sigmaSTB_002 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSTB, $
                           Tag='STB_002_FLX')
  sigmaDSK = Jam_EFunc(dSED, pError, Covar=covar, Index=idxDSK, Tag='DSK_FLX')
  sigmaSRC = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSRC, Tag='SRC_FLX')
  sigmaDST = Jam_EFunc(dSED, pError, Covar=covar, Index=idxDST, Tag='DST_FLX')
  sigmaCON = Jam_EFunc(dSED, pError, Covar=covar, Index=idxCON, Tag='CON_FLX')

  ;; Unextinguished flux errors.
  sigmaCOO0 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxCOO, $
                        Tag='COO0_FLX')
  sigmaWRM0 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxWRM, $
                        Tag='WRM0_FLX')
  sigmaHOT0 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxHOT, $
                        Tag='HOT0_FLX')
  sigmaSTR0 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSTR, $
                        Tag='STR0_FLX')
  sigmaSTB0 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSTB, $
                        Tag='STB0_FLX')
  sigmaSTB0_100 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSTB, $
                           Tag='STB0_100_FLX')
  sigmaSTB0_010 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSTB, $
                           Tag='STB0_010_FLX')
  sigmaSTB0_002 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSTB, $
                           Tag='STB0_002_FLX')
  sigmaDSK0 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxDSK, $
                        Tag='DSK0_FLX')
  sigmaSRC0 = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSRC, $
                        Tag='SRC0_FLX')
  ;; Extinction curve errors.
  sigExtCOO = Jam_EFunc(dSED, pError, Covar=covar, Index=idxCOO, Tag='COO_EXT')
  sigExtWRM = Jam_EFunc(dSED, pError, Covar=covar, Index=idxWRM, Tag='WRM_EXT')
  sigExtHOT = Jam_EFunc(dSED, pError, Covar=covar, Index=idxHOT, Tag='HOT_EXT')
  sigExtSTR = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSTR, Tag='STR_EXT')
  sigExtSTB = Jam_EFunc(dSED, pError, Covar=covar, Index=idxSTB, Tag='STB_EXT')
  sigExtDSK = Jam_EFunc(dSED, pError, Covar=covar, Index=idxDSK, Tag='DSK_EXT')
  sigExtPAH = Jam_EFunc(dSED, pError, Covar=covar, Index=idxPAH, Tag='PAH_EXT')
  ;; E0 errors.
  sigE0CIR = Jam_EFunc(dSED, pError, Covar=covar, Index=idxCIR, Tag='CIR_E0')
  sigE0CLD = Jam_EFunc(dSED, pError, Covar=covar, Index=idxCLD, Tag='CLD_E0')
  sigE0COO = Jam_EFunc(dSED, pError, Covar=covar, Index=idxCOO, Tag='COO_E0')
  sigE0WRM = Jam_EFunc(dSED, pError, Covar=covar, Index=idxWRM, Tag='WRM_E0')
  sigE0HOT = Jam_EFunc(dSED, pError, Covar=covar, Index=idxHOT, Tag='HOT_E0')
  ;; Tau0 errors.
  sigTau0COO = Jam_EFunc(dSED, pError, Covar=covar, Index=idxCOO, $
                         Tag='COO_TAU0')
  sigTau0WRM = Jam_EFunc(dSED, pError, Covar=covar, Index=idxWRM, $
                         Tag='WRM_TAU0')
  sigTau0HOT = Jam_EFunc(dSED, pError, Covar=covar, Index=idxHOT, $
                         Tag='HOT_TAU0')

  ;; Calculate tau(V)/tau(9.7) ratio.
  K_Abs = Jam_GrainOpacity(waveRatio, 0.D, K_Ext=K_Ext)
  K_DSK_Ratio = (input.ExtOrAbs EQ 'EXT') ? $
                K_Ext.Carb + K_Ext.Sil.Amo.Tot : $
                K_Abs.Carb + K_Abs.Sil.Amo.Tot
  K_STB_Ratio = K_Abs.Carb + K_Abs.Sil.Amo.Tot
  ratio_DSK = Interpol(K_DSK_Ratio, waveRatio, 0.55D) / $
              Interpol(K_DSK_Ratio, waveRatio, 9.7D)
  ratio_STB = Interpol(K_STB_Ratio, waveRatio, 0.55D) / $
              Interpol(K_STB_Ratio, waveRatio, 9.7D)

  ;; Calculate DSK_TAU.
  IF (Jam_Check(input, Tag_Name='TIE_DSK_TAU')) THEN BEGIN
     DSK_TAU = (input.Tie_DSK_TAU EQ 'HOT_TAU') ? $
               tau0.HOT : ratio_DSK * p.DSK_TAU
     DSK_TAU_ERR = (input.Tie_DSK_TAU EQ 'HOT_TAU') ? $
                   sigTau0HOT : ratio_DSK * pE.DSK_TAU
  ENDIF $
  ELSE BEGIN
     DSK_TAU = ratio_DSK * p.DSK_TAU
     DSK_TAU_ERR = ratio_DSK * pE.DSK_TAU
  ENDELSE

  ;; Add tau0 errors to tau0 structure.
  tau0 = Create_Struct(tau0, $
                       'STB', ratio_STB * p.STB_TAU, $
                       'DSK', DSK_TAU, $
                       'COO_ERR', sigTau0COO[0], $
                       'WRM_ERR', sigTau0WRM[0], $
                       'HOT_ERR', sigTau0HOT[0], $
                       'STB_ERR', ratio_STB * pE.STB_TAU, $
                       'DSK_ERR', DSK_TAU_ERR)

  ;; Calculate PAH and LIN uncertainties for SED wavelength grid.
  errFluxPAH0 = 0.D * waveSED
  errFluxPAH = errFluxPAH0
  errFluxLIN = 0.D * waveSED
  IF (fitPAHs) THEN BEGIN
     fluxPAH0 = Jam_DrudeFlux(waveSED, drude, EDrude=errDrude, $
                              Covar=covarFTR, EFlux=errFluxPAH0)
     fluxPAH = ext.PAH * fluxPAH0
     errFluxPAH = Sqrt((ext.PAH * errFluxPAH0)^2 + (sigExtPAH * fluxPAH0)^2)
  ENDIF
  IF (fitLINs) THEN BEGIN
     fluxLIN = Jam_GaussianFlux(waveSED, gauss, EGauss=errGauss, $
                                Covar=covarLIN, EFlux=errFluxLIN)
  ENDIF

  ;; Create error structures.
  errTOT = Sqrt(sigmaCON^2 + errFluxLIN^2 + errFluxPAH^2)
  errCom = { Wave:wave, $
             SRC:sigmaSRC, $
             DST:sigmaDST, $
             CON:sigmaCON, $
             LIN:errFluxLIN, $
             PAH:errFluxPAH, $
             CIR:sigmaCIR, $
             CLD:sigmaCLD, $
             COO:sigmaCOO, $
             WRM:sigmaWRM, $
             HOT:sigmaHOT, $
             STR:sigmaSTR, $
             STB:sigmaSTB, $
             STB_100:sigmaSTB_100, $
             STB_010:sigmaSTB_010, $
             STB_002:sigmaSTB_002, $
             DSK:sigmaDSK }
  errCom0 = { Wave:wave, $
              SRC:sigmaSRC0, $
              DST:sigmaDST, $
              CON:sigmaCON, $
              LIN:errFluxLIN, $
              PAH:errFluxPAH0, $
              CIR:sigmaCIR, $
              CLD:sigmaCLD, $
              COO:sigmaCOO0, $
              WRM:sigmaWRM0, $
              HOT:sigmaHOT0, $
              STR:sigmaSTR0, $
              STB:sigmaSTB0, $
              STB_100:sigmaSTB0_100, $
              STB_010:sigmaSTB0_010, $
              STB_002:sigmaSTB0_002, $
              DSK:sigmaDSK0 }
  errExt = { Wave:wave, $
             COO:sigExtCOO, $
             WRM:sigExtWRM, $
             HOT:sigExtHOT, $
             PAH:sigExtPAH, $
             STR:sigExtSTR, $
             STB:sigExtSTB, $
             DSK:sigExtDSK }
  errE0 = { CIR:sigE0CIR[0], $
            CLD:sigE0CLD[0], $
            COO:sigE0COO[0], $
            WRM:sigE0WRM[0], $
            HOT:sigE0HOT[0] }

  ;; Create structure of fit data.
  FIT = { z:z, $
          Wave:waveSED, $
          Flux:fluxSED, $
          Sigma:errTOT, $
          Index: { OPT:indexSEDOPT, $
                   NIR:indexSEDNIR, $
                   IRS:indexSEDIRS, $
                   FIR:indexSEDFIR, $
                   RAD:indexSEDRAD }, $
          Com:com, $
          Com0:com0, $
          Ext:ext, $
          E0:E0, $
          ErrCom:errCom, $
          ErrCom0:errCom0, $
          ErrExt:errExt, $
          ErrE0:errE0 }

  ;; Create structure of parameter names and values.
  pE = Cafe_Params(pError, paramTied)
  _pError = pError
  FOR i=0,N_Tags(pE)-1 DO _pError[i] = pE.(i)
  params = { Names:paramNames, Values:values, Errors:_pError, Tied:paramTied }

  ;; Calculate component luminosities, masses, and mean temperatures.
  luminosity = Cafe_Luminosity(SED, FIT, input, p, pE, wave0)
  IF (N_Elements(L_dust) NE 0) THEN BEGIN
     IF (L_dust NE -1.D) THEN luminosity.DST_TOT = L_dust
  ENDIF
  IF (N_Elements(L_IR) NE 0) THEN BEGIN
     IF (L_IR NE -1.D) THEN luminosity.DST_IR = L_IR
  ENDIF
  mass = Cafe_Mass(SED, FIT, input, p, pE, wave0, flux0, luminosity)
  T_Mean = Cafe_MeanTemp(p, pE, com, input)

  ;; Calculate synthetic photometry.
  synPhot = Jam_SynPhot(waveSED * (1.D + z), fluxSED, errTOT)
  synPhotRest = Jam_SynPhot(waveSED, fluxSED * (1.D + z), errTOT)
  synPhotObs = Jam_SynPhot(waveSED * (1.D + z), fluxSED * (1.D + z), errTOT)

  ;; Gather fitted PAH data.
  IF (fitPAHs) THEN BEGIN

     ;; Calculate PAH EQW. (Note these are the extinction corrected values!)
     ;; cont = [Jy] -> [erg s-1 cm-2 um-1] --> Jam_Jy2um(wave0, flux0)
     cont = Jam_Jy2um(FIT.Com.Wave, FIT.Com.CON)
     errCont = Jam_Jy2um(FIT.Com.Wave, FIT.ErrCom.CON)
     cont6 = Interpol(cont, FIT.Com.Wave, 6.22D)
     cont7 = Interpol(cont, FIT.Com.Wave, 7.7D)
     cont8 = Interpol(cont, FIT.Com.Wave, 8.6D)
     cont11 = Interpol(cont, FIT.Com.Wave, 11.3D)
     cont12 = Interpol(cont, FIT.Com.Wave, 12.7D)
     cont17 = Interpol(cont, FIT.Com.Wave, 17.1D)
     eCont6 = Interpol(errCont, FIT.Com.Wave, 6.22D)
     eCont7 = Interpol(errCont, FIT.Com.Wave, 7.7D)
     eCont8 = Interpol(errCont, FIT.Com.Wave, 8.6D)
     eCont11 = Interpol(errCont, FIT.Com.Wave, 11.3D)
     eCont12 = Interpol(errCont, FIT.Com.Wave, 12.7D)
     eCont17 = Interpol(errCont, FIT.Com.Wave, 17.1D)
     ;; power = [erg s-1 cm-2]
     ;; EQW0 = Apparent (extinguished) EQW, EQW = Intrinsic (unextinguished) EQW
     EQW0_PAH6 = cPower0PAH.PAH6 / cont6
     EQW0_PAH7 = cPower0PAH.PAH7 / cont7
     idx8 = Where((drude.Wave0 GT 8.5) AND (drude.Wave0 LT 8.7))
     EQW0_PAH8 = power0PAH[idx8] / cont8
     EQW0_PAH11 = cPower0PAH.PAH11 / cont11
     EQW0_PAH12 = cPower0PAH.PAH12 / cont12
     EQW0_PAH17 = cPower0PAH.PAH17 / cont17
     err_EQW0_PAH6 = Sqrt((errCPower0PAH.PAH6 / cont6)^2 + $
                          (cPower0PAH.PAH6 / cont6^2 * eCont6)^2)
     err_EQW0_PAH7 = Sqrt((errCPower0PAH.PAH7 / cont7)^2 + $
                          (cPower0PAH.PAH7 / cont7^2 * eCont7)^2)
     err_EQW0_PAH8 = Sqrt((errPower0PAH[idx8] / cont8)^2 + $
                          (power0PAH[idx8] / cont8^2 * eCont8)^2)
     err_EQW0_PAH11 = Sqrt((errCPower0PAH.PAH11 / cont11)^2 + $
                           (cPower0PAH.PAH11 / cont11^2 * eCont11)^2)
     err_EQW0_PAH12 = Sqrt((errCPower0PAH.PAH12 / cont12)^2 + $
                           (cPower0PAH.PAH12 / cont12^2 * eCont12)^2)
     err_EQW0_PAH17 = Sqrt((errCPower0PAH.PAH17 / cont17)^2 + $
                           (cPower0PAH.PAH17 / cont17^2 * eCont17)^2)
     EQW_PAH6 = cPowerPAH.PAH6 / cont6
     EQW_PAH7 = cPowerPAH.PAH7 / cont7
     EQW_PAH8 = powerPAH[idx8] / cont8
     EQW_PAH11 = cPowerPAH.PAH11 / cont11
     EQW_PAH12 = cPowerPAH.PAH12 / cont12
     EQW_PAH17 = cPowerPAH.PAH17 / cont17
     err_EQW_PAH6 = Sqrt((errCPowerPAH.PAH6 / cont6)^2 + $
                         (cPowerPAH.PAH6 / cont6^2 * eCont6)^2)
     err_EQW_PAH7 = Sqrt((errCPowerPAH.PAH7 / cont7)^2 + $
                         (cPowerPAH.PAH7 / cont7^2 * eCont7)^2)
     err_EQW_PAH8 = Sqrt((errPowerPAH[idx8] / cont8)^2 + $
                         (powerPAH[idx8] / cont8^2 * eCont8)^2)
     err_EQW_PAH11 = Sqrt((errCPowerPAH.PAH11 / cont11)^2 + $
                          (cPowerPAH.PAH11 / cont11^2 * eCont11)^2)
     err_EQW_PAH12 = Sqrt((errCPowerPAH.PAH12 / cont12)^2 + $
                          (cPowerPAH.PAH12 / cont12^2 * eCont12)^2)
     err_EQW_PAH17 = Sqrt((errCPowerPAH.PAH17 / cont17)^2 + $
                          (cPowerPAH.PAH17 / cont17^2 * eCont17)^2)

     ;; Create PAH structure.
     dataPAH = { Wave0:drude.Wave0, $
                 Power:powerPAH, $   ;; Intrinsic (unextinguished) power
                 ErrPower:errPowerPAH, $
                 PowerExt:power0PAH, $ ;; Apparent (extinguished) power
                 ErrPowerExt:errPower0PAH, $
                 Power6:cPowerPAH.PAH6, $
                 Power7:cPowerPAH.PAH7, $
                 Power8:(powerPAH[idx8])[0], $
                 Power11:cPowerPAH.PAH11, $
                 Power12:cPowerPAH.PAH12, $
                 Power17:cPowerPAH.PAH17, $
                 EPower6:errCPowerPAH.PAH6, $
                 EPower7:errCPowerPAH.PAH7, $
                 EPower8:(errPowerPAH[idx8])[0], $
                 EPower11:errCPowerPAH.PAH11, $
                 EPower12:errCPowerPAH.PAH12, $
                 EPower17:errCPowerPAH.PAH17, $
                 PowerExt6:cPower0PAH.PAH6, $
                 PowerExt7:cPower0PAH.PAH7, $
                 PowerExt8:(power0PAH[idx8])[0], $
                 PowerExt11:cPower0PAH.PAH11, $
                 PowerExt12:cPower0PAH.PAH12, $
                 PowerExt17:cPower0PAH.PAH17, $
                 EPowerExt6:errCPower0PAH.PAH6, $
                 EPowerExt7:errCPower0PAH.PAH7, $
                 EPowerExt8:(errPower0PAH[idx8])[0], $
                 EPowerExt11:errCPower0PAH.PAH11, $
                 EPowerExt12:errCPower0PAH.PAH12, $
                 EPowerExt17:errCPower0PAH.PAH17, $
                 Ratio6:ratioPAH.PAH6, $
                 Ratio11:ratioPAH.PAH11, $
                 Ratio12:ratioPAH.PAH12, $
                 Ratio17:ratioPAH.PAH17, $
                 ERatio6:errRatioPAH.PAH6, $
                 ERatio11:errRatioPAH.PAH11, $
                 ERatio12:errRatioPAH.PAH12, $
                 ERatio17:errRatioPAH.PAH17, $
                 RatioExt6:ratio0PAH.PAH6, $
                 RatioExt11:ratio0PAH.PAH11, $
                 RatioExt12:ratio0PAH.PAH12, $
                 RatioExt17:ratio0PAH.PAH17, $
                 ERatioExt6:errRatio0PAH.PAH6, $
                 ERatioExt11:errRatio0PAH.PAH11, $
                 ERatioExt12:errRatio0PAH.PAH12, $
                 ERatioExt17:errRatio0PAH.PAH17, $
                 EQW6:EQW_PAH6, $
                 EQW7:EQW_PAH7, $
                 EQW8:EQW_PAH8[0], $
                 EQW11:EQW_PAH11, $
                 EQW12:EQW_PAH12, $
                 EQW17:EQW_PAH17, $
                 EEQW6:err_EQW_PAH6, $
                 EEQW7:err_EQW_PAH7, $
                 EEQW8:err_EQW_PAH8[0], $
                 EEQW11:err_EQW_PAH11, $
                 EEQW12:err_EQW_PAH12, $
                 EEQW17:err_EQW_PAH17, $
                 EQWExt6:EQW0_PAH6, $
                 EQWExt7:EQW0_PAH7, $
                 EQWExt8:EQW0_PAH8[0], $
                 EQWExt11:EQW0_PAH11, $
                 EQWExt12:EQW0_PAH12, $
                 EQWExt17:EQW0_PAH17, $
                 EEQWExt6:err_EQW0_PAH6, $
                 EEQWExt7:err_EQW0_PAH7, $
                 EEQWExt8:err_EQW0_PAH8[0], $
                 EEQWExt11:err_EQW0_PAH11, $
                 EEQWExt12:err_EQW0_PAH12, $
                 EEQWExt17:err_EQW0_PAH17 }

  ENDIF ELSE $
     dataPAH = { Wave0:-1.D }

  ;; Gather LIN data.
  IF (fitLINs) THEN BEGIN
     dataLIN = { Name:gauss.Name, Wave0:gauss.Wave0, $
                 Power:powerLIN, ErrPower:errPowerLIN }
  ENDIF ELSE $
     dataLIN = { Name:'', Wave0:-1 }

  ;; Create structure of fit info.
  CAFE = { ChiSqrSED:chiSqrSED, $
           ChiSqrOPT:chiSqrOPT, $
           ChiSqrNIR:chiSqrNIR, $
           ChiSqrMIR:chiSqrMIR, $
           ChiSqrIRS:chiSqrIRS, $
           ChiSqrFIR:chiSqrFIR, $
           ChiSqrRAD:chiSqrRAD, $
           ChiSqrFTR:chiSqrFTR, $
           DOF_SED:DOF_SED, $
           DOF_OPT:DOF_OPT, $
           DOF_NIR:DOF_NIR, $
           DOF_MIR:DOF_MIR, $
           DOF_IRS:DOF_IRS, $
           DOF_FIR:DOF_FIR, $
           DOF_RAD:DOF_RAD, $
           DOF_FTR:DOF_FTR, $
           N_Free:nFree, $
           Params:params, $
           DAT:DAT, $
           FIT:FIT, $
           PAH:{ Data:dataPAH, Drude:drude, ErrDrude:errDrude }, $
           LIN:{ Data:dataLIN, Gauss:gauss, ErrGauss:errGauss }, $
           Luminosity:luminosity, $
           Mass:mass, $
           T_Mean:T_Mean, $
           Tau0:tau0, $
           SynPhot:synPhot, $
           SynPhotObs:synPhotObs, $
           SynPhotRest:synPhotRest, $
           Log:log, $
           Extra:{ Input:input, $
                   Wave0:wave0, $
                   Flux0:flux0, $
                   FitLINs:fitLINs, $
                   FitPAHs:fitPAHs } }
  SED = Create_Struct(SED, 'CAFE', CAFE)

  ;; Call plot procedure.
  IF (N_Elements(outPath) EQ 0) THEN outPath = !Cafe.Out
  IF (~Keyword_Set(noView)) THEN BEGIN
     Cafe_Plot, SED, input, OutPath=outPath, SubTitle=subTitle, $
                      OPlotFit=oPlotFit, Lines=lines
  ENDIF

  ;; Call save procedure.
  IF (~Keyword_Set(noSave)) THEN BEGIN
     Cafe_Save, SED, input, p, OutPath=outPath, FitPAHs=fitPAHs, $
                      FitLINs=fitLINs, SubTitle=subTitle, Wave0=wave0, $
                      Flux0=flux0, OPlotFit=oPlotFit, Lines=lines
  ENDIF

  RETURN

END
