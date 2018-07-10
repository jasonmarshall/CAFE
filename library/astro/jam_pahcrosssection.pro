;+ ====================================================================
; NAME:
;   JAM_PAHCROSSSECTION
;
; PURPOSE:
;   This function returns absorption cross sections of PAH molecules.
;
; CATEGORY:
;   Astronomy, Physics.
;
; CALLING SEQUENCE:
;   Result = JAM_PAHCROSSSECTION([Wave, a])
;
; KEYWORD PARAMETERS:
;   C_ABS:
;   E:
;
; OUTPUTS:
;   This function returns an anonymous structure of absorption cross
;   sections in units of cm^2. The structure takes the form:
;
;     { Result }.
;               |- [ <Wave> ]
;               |- [ <a> ]
;               |- [[ Sil ]]
;               |- { Carb }.
;                          |- { Neu }.
;                                    |- [[  Tot ]]
;                                    |- [[ Cont ]]
;                                    |- [[ Feat ]]
;                          |- { Ion }.
;                                    |- [[  Tot ]]
;                                    |- [[ Cont ]]
;                                    |- [[ Feat ]]
;
;   The fields 'Wave' and 'a' contain the wavelength and grain radius
;   vectors corresponding to the first and second indices of the cross
;   section arrays for each grain type.
;
; RESTRICTIONS:
;   <JamLib>
;   JAM_SYNTAX, JAM_MESSAGE, JAM_CHECK, JAM_DIST, JAM_READDATA,
;   JAM_COMPARE, JAM_DRUDEFLUX
;
; PROCEDURE:
;
;   Absorption efficiencies, Q_Abs, of silicate and graphite grains
;   are calculated according to the methods described in Laor &
;   Draine (1993) [see http://www.astro.princeton.edu/~draine/dust/]
;   and C_Abs(Wave,a) = Pi * a^2 * Q_Abs(Wave,a).
;
;   The absorption cross sections of PAHs are calculated according
;   to the model described in Li & Draine (2001) in which carbon-
;   aceous grains have PAH properties at small radii and graphitic
;   properties at large radii.
;
; REFERENCES:
;   Laor & Draine, ApJ, 402, 1993.
;   Li & Draine, ApJ, 554, 2001.
;
; EXAMPLE:
;   To obtain absorption cross sections of astronomical dust enter:
;     IDL> C_Abs = JAM_PAHCROSSSECTION()
;
; MODIFICATION HISTORY:
;   Written by: Jason Marshall, October 2005.
;   2005 Dec 01 - JAM - Added PAH grain properties.
;-

FUNCTION Jam_PAHCrossSection, wave, a, C_Gra, C_Cont=C_Cont, C_Feat=C_Feat, $
                              Feature=feature

  Compile_Opt IDL2
  On_Error, 2

  ;; Create "continuum" Drude profile table.
  IF (~Keyword_Set(feature)) THEN BEGIN
     drude_C = Replicate({ Wave0:0.D, Gamma:0.D, Neu:0.D, Ion:0.D }, 6)
     drude_C[0] = { Wave0:7.220D-2, Gamma:0.195D, Neu:7.97D7, Ion:7.97D7 }
     drude_C[1] = { Wave0:2.175D-1, Gamma:0.217D, Neu:1.23D7, Ion:1.23D7 }
     drude_C[2] = { Wave0:1.050D+0, Gamma:0.055D, Neu:0.00D0, Ion:2.00D4 }
     drude_C[3] = { Wave0:1.260D+0, Gamma:0.110D, Neu:0.00D0, Ion:7.8D-2 }
     drude_C[4] = { Wave0:1.905D+0, Gamma:0.090D, Neu:0.00D0, Ion:-146.5D }
     drude_C[5] = { Wave0:1.500D+1, Gamma:0.800D, Neu:5.00D1, Ion:5.00D1 }
     wave0_C = drude_C.Wave0
     gamma_C = drude_C.Gamma
     sigmaNeu_C = 1D-20 * drude_C.Neu
     sigmaIon_C = 1D-20 * drude_C.Ion
     peakNeu_C = (2.D / !DPi) * (wave0_C / 1D4) * sigmaNeu_C / gamma_C
     peakIon_C = (2.D / !DPi) * (wave0_C / 1D4) * sigmaIon_C / gamma_C
  ENDIF

  ;; Create "features" Drude profile table.
  N_DRUDE = 21
  drude_F = Replicate({ Wave0:0.D, Gamma:0.D, Neu:0.D, Ion:0.D }, N_DRUDE)
  drude_F[0] = { Wave0:3.300D+0, Gamma:0.012D, Neu:394.D, Ion:89.4D }
  drude_F[1] = { Wave0:5.250D+0, Gamma:0.030D, Neu:2.5D, Ion:20.D }
  drude_F[2] = { Wave0:5.700D+0, Gamma:0.040D, Neu:7.5D, Ion:60.D }
  drude_F[3] = { Wave0:6.220D+0, Gamma:0.0284D, Neu:29.4D, Ion:236.D }
  drude_F[4] = { Wave0:6.690D+0, Gamma:0.070D, Neu:5.88D, Ion:47.2D }
  drude_F[5] = { Wave0:7.417D+0, Gamma:0.126D, Neu:15.8D, Ion:142.D }
  drude_F[6] = { Wave0:7.598D+0, Gamma:0.044D, Neu:23.8D, Ion:214.D }
  drude_F[7] = { Wave0:7.850D+0, Gamma:0.053D, Neu:21.3D, Ion:192.D }
  drude_F[8] = { Wave0:8.330D+0, Gamma:0.052D, Neu:6.94D, Ion:48.4D }
  drude_F[9] = { Wave0:8.610D+0, Gamma:0.039D, Neu:27.8D, Ion:194.D }
  drude_F[10] = { Wave0:1.123D+1, Gamma:0.010D, Neu:12.8D, Ion:12.D }
  drude_F[11] = { Wave0:1.130D+1, Gamma:0.029D, Neu:58.4D, Ion:54.7D }
  drude_F[12] = { Wave0:1.199D+1, Gamma:0.050D, Neu:24.2D, Ion:20.5D }
  drude_F[13] = { Wave0:1.261D+1, Gamma:0.0435D, Neu:34.8D, Ion:31.D }
  drude_F[14] = { Wave0:1.360D+1, Gamma:0.020D, Neu:0.5D, Ion:0.5D }
  drude_F[15] = { Wave0:1.419D+1, Gamma:0.025D, Neu:0.5D, Ion:0.5D }
  drude_F[16] = { Wave0:1.590D+1, Gamma:0.020D, Neu:0.5D, Ion:0.5D }
  drude_F[17] = { Wave0:1.6447D+1, Gamma:0.014D, Neu:0.75D, Ion:0.75D }
  drude_F[18] = { Wave0:1.7038D+1, Gamma:0.065D, Neu:3.08D, Ion:3.08D }
  drude_F[19] = { Wave0:1.7377D+1, Gamma:0.012D, Neu:0.28D, Ion:0.28D }
  drude_F[20] = { Wave0:1.7873D+1, Gamma:0.016D, Neu:0.14D, Ion:0.14D }
  wave0_F = drude_F.Wave0
  gamma_F = drude_F.Gamma
  sigmaNeu_F = 1D-20 * drude_F.Neu
  sigmaIon_F = 1D-20 * drude_F.Ion

  ;; Calculate the number of carbon atoms in each PAH.
  N_C = ((1D4 / 1.286D) * a)^3 ;; density(gra) = 2.24 g cm-3

  ;; Calculate hydrogenation (H/C) for each PAH.
  idxA = Where(N_C LE 25.D, cntA)
  idxB = Where((N_C GT 25.D) AND (N_C LT 100.D), cntB)
  idxC = Where(N_C GE 100.D, cntC)
  HtoC = 0.D * N_C
  IF (cntA GT 0) THEN HtoC[idxA] = 0.5D
  IF (cntB GT 0) THEN HtoC[idxB] = 0.5D / Sqrt(N_C[idxB] / 25.D)
  IF (cntC GT 0) THEN HtoC[idxC] = 0.25D

  ;; Calculate the dimensionless wavenumber: x = (wave / [um])^-1
  x = 1.D / wave

  ;; Calculate C_PAH.
  N_WAVE = N_Elements(wave)
  N_A = N_Elements(a)
  C_Cont_Neu = DblArr(N_WAVE, N_A)
  C_Cont_Ion = DblArr(N_WAVE, N_A)
  C_Feat_Neu = DblArr(N_WAVE, N_A)
  C_Feat_Ion = DblArr(N_WAVE, N_A)
  big_N_C = Rebin(Reform(N_C, 1, N_A), N_WAVE, N_A)
  IF (~Keyword_Set(feature)) THEN BEGIN
     ;; *** x > 17.25 *** ;;
     idx = Where(x GE 17.25D, cnt)
     IF (cnt GT 0) THEN BEGIN
        C_Cont_Neu[idx, *] = C_Gra[idx, *]
        C_Cont_Ion[idx, *] = C_Gra[idx, *]
     ENDIF
     ;; *** 15 < x < 17.25 *** ;;
     idx = Where((x GE 15.D) AND (x LT 17.25D), cnt)
     IF (cnt GT 0) THEN BEGIN
        C_Cont = big_N_C[idx, *] * $
                 Rebin((126.D - 6.4943D * x[idx]) * 1D-18, cnt, N_A)
        C_Cont_Neu[idx, *] = C_Cont
        C_Cont_Ion[idx, *] = C_Cont
     ENDIF
     ;; *** 10 < x < 15 *** ;;
     idx = Where((x GE 10.D) AND (x LT 15.D), cnt)
     IF (cnt GT 0) THEN BEGIN
        thisDrudeNeu = { Wave0:wave0_C[0], Gamma:gamma_C[0], $
                         Peak:peakNeu_C[0] }
        thisDrudeIon = { Wave0:wave0_C[0], Gamma:gamma_C[0], $
                         Peak:peakIon_C[0] }
        drudeFluxNeu = Jam_DrudeFlux(wave[idx], thisDrudeNeu)
        drudeFluxIon = Jam_DrudeFlux(wave[idx], thisDrudeIon)
        C_Cont = (-3.D + 1.35D * x[idx]) * 1D-18
        C_Cont_Neu[idx, *] = big_N_C[idx, *] * $
                             Rebin(drudeFluxNeu + C_Cont, cnt, N_A)
        C_Cont_Ion[idx, *] = big_N_C[idx, *] * $
                             Rebin(drudeFluxIon + C_Cont, cnt, N_A)
     ENDIF
     ;; *** 7.7 < x < 10 *** ;;
     idx = Where((x GE 7.7D) AND (x LT 10.D), cnt)
     IF (cnt GT 0) THEN BEGIN
        thisX = x[idx]
        C_Cont = big_N_C[idx, *] * $
                 Rebin((66.302D - 24.367D * thisX + 2.95D * thisX^2 - $
                        0.1057D * thisX^3) * 1D-18, cnt, N_A)
        C_Cont_Neu[idx, *] = C_Cont
        C_Cont_Ion[idx, *] = C_Cont
     ENDIF
     ;; *** 5.9 < x < 7.7 *** ;;
     idx = Where((x GE 5.9D) AND (x LT 7.7D), cnt)
     IF (cnt GT 0) THEN BEGIN
        thisDrudeNeu = { Wave0:wave0_C[1], Gamma:gamma_C[1], $
                         Peak:peakNeu_C[1] }
        thisDrudeIon = { Wave0:wave0_C[1], Gamma:gamma_C[1], $
                         Peak:peakIon_C[1] }
        drudeFluxNeu = Jam_DrudeFlux(wave[idx], thisDrudeNeu)
        drudeFluxIon = Jam_DrudeFlux(wave[idx], thisDrudeIon)
        thisX = x[idx]
        C_Cont = (1.8687D + 0.1905D * thisX + 0.4175D * (thisX - 5.9D)^2 + $
                  0.04370D * (thisX - 5.9D)^3) * 1D-18
        C_Cont_Neu[idx, *] = big_N_C[idx, *] * $
                             Rebin(drudeFluxNeu + C_Cont, cnt, N_A)
        C_Cont_Ion[idx, *] = big_N_C[idx, *] * $
                             Rebin(drudeFluxIon + C_Cont, cnt, N_A)
     ENDIF
     ;; *** 3.3 < x < 5.9 *** ;;
     idx = Where((x GE 3.3D) AND (x LT 5.9D), cnt)
     IF (cnt GT 0) THEN BEGIN
        thisDrudeNeu = { Wave0:wave0_C[1], Gamma:gamma_C[1], $
                         Peak:peakNeu_C[1] }
        thisDrudeIon = { Wave0:wave0_C[1], Gamma:gamma_C[1], $
                         Peak:peakIon_C[1] }
        drudeFluxNeu = Jam_DrudeFlux(wave[idx], thisDrudeNeu)
        drudeFluxIon = Jam_DrudeFlux(wave[idx], thisDrudeIon)
        C_Cont = (1.8687D + 0.1905D * x[idx]) * 1D-18
        C_Cont_Neu[idx, *] = big_N_C[idx, *] * $
                             Rebin(drudeFluxNeu + C_Cont, cnt, N_A)
        C_Cont_Ion[idx, *] = big_N_C[idx, *] * $
                             Rebin(drudeFluxIon + C_Cont, cnt, N_A)
     ENDIF
  ENDIF
  ;; *** x < 3.3 *** ;;
  idx = Where(x LT 3.3D, cnt)
  IF (cnt GT 0) THEN BEGIN
     FOR i=0,N_A-1 DO BEGIN
        _sigmaNeu_F = sigmaNeu_F
        _sigmaIon_F = sigmaIon_F
        _sigmaNeu_F[0] *= HtoC[i]
        _sigmaIon_F[0] *= HtoC[i]
        _sigmaNeu_F[8:13] *= HtoC[i]
        _sigmaIon_F[8:13] *= HtoC[i]
        peakNeu_F = (2.D / !DPi) * (wave0_F / 1D4) * _sigmaNeu_F / gamma_F
        peakIon_F = (2.D / !DPi) * (wave0_F / 1D4) * _sigmaIon_F / gamma_F
        thisDrudeNeu = { Wave0:wave0_F, Gamma:gamma_F, Peak:peakNeu_F }
        thisDrudeIon = { Wave0:wave0_F, Gamma:gamma_F, Peak:peakIon_F }
        drudeFluxNeu = Jam_DrudeFlux(wave[idx], thisDrudeNeu)
        drudeFluxIon = Jam_DrudeFlux(wave[idx], thisDrudeIon)
        C_Feat_Neu[idx, i] = N_C[i] * drudeFluxNeu
        C_Feat_Ion[idx, i] = N_C[i] * drudeFluxIon
        IF (~Keyword_Set(feature)) THEN BEGIN
           C_Cont = 34.58D * 10^((-(18.D + (3.431D / x[idx]))) > $
                                 !Jam.Log10Min) > 1D-100
           M = (N_C[i] GE 40.D) ? 0.4D * N_C[i] : 0.3D * N_C[i]
           waveCutNeu = 1.D / ((3.804D / Sqrt(M)) + 1.052D)
           waveCutIon = 1.D / ((2.282D / Sqrt(M)) + 0.889D)
           yNeu = waveCutNeu / wave[idx]
           yIon = waveCutIon / wave[idx]
           cutNeu = (ATan(1D3 * (yNeu - 1.D)^3 / yNeu) / !DPi) + 0.5D
           cutIon = (ATan(1D3 * (yIon - 1.D)^3 / yIon) / !DPi) + 0.5D
           thisDrudeNeu = { Wave0:wave0_C[5], Gamma:gamma_C[5], $
                            Peak:peakNeu_C[5] }
           thisDrudeIon = { Wave0:wave0_C[5], Gamma:gamma_C[5], $
                            Peak:peakIon_C[5] }
           drudeFluxNeu = Jam_DrudeFlux(wave[idx], thisDrudeNeu)
           drudeFluxIon = Jam_DrudeFlux(wave[idx], thisDrudeIon)
           C_Cont_Neu[idx, i] = N_C[i] * (cutNeu * C_Cont + drudeFluxNeu)
           C_Cont_Ion[idx, i] = N_C[i] * (cutIon * C_Cont + drudeFluxIon)
        ENDIF
     ENDFOR
  ENDIF

  ;; Near-IR term (see Draine+Li 2006).
  IF (~Keyword_Set(feature)) THEN BEGIN
     thisDrudeNeu = { Wave0:wave0_C[2:4], Gamma:gamma_C[2:4], $
                      Peak:peakNeu_C[2:4] }
     thisDrudeIon = { Wave0:wave0_C[2:4], Gamma:gamma_C[2:4], $
                      Peak:peakIon_C[2:4] }
     drudeFluxNeu = Jam_DrudeFlux(wave, thisDrudeNeu)
     drudeFluxIon = Jam_DrudeFlux(wave, thisDrudeIon)
     C_Cont = 3.5D * 10^(-19.D - (1.45D / x)) * Exp(-0.1D * x^2)
     C_Cont_Neu += (big_N_C * Rebin(drudeFluxNeu, N_WAVE, N_A))
     C_Cont_Ion += (big_N_C * Rebin(drudeFluxIon + C_Cont, N_WAVE, N_A))
  ENDIF

  C_Cont = { Neu:C_Cont_Neu, Ion:C_Cont_Ion }
  C_Feat = { Neu:C_Feat_Neu, Ion:C_Feat_Ion }

  RETURN, { Neu:C_Feat_Neu + C_Cont_Neu, Ion:C_Feat_Ion + C_Cont_Ion }

END
