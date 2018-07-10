;+ ===================================================================
; NAME:
; JAM_GRAINSIZEDF
;
; PURPOSE:
; This routine calculates the grain-size distribution function of
; graphitic and silicate astronomical dust.
;
; CATEGORY:
; Astronomy, Physics.
;
; CALLING SEQUENCE:
; Result = JAM_GRAINSIZEDF([a, T_bb])
;
; OPTIONAL INPUTS:
; a: A scalar or vector of grain radii in microns.
; T_bb:
;
; KEYWORD PARAMETERS:
; MODEL: 
;
; OUTPUTS:
; This function returns an anonymous structure containing the grain-size
; distribution function values (1/n_H) * dn(a)/da of astronomical dust in
; units of um^-1 H^-1. The structure takes the form:
;
;   { Result }.
;             |- [ <a> ]
;             |- [ <T_bb> ]
;             |- [[  Sil ]]
;             |- [[ Carb ]]
;
; RESTRICTIONS:
; JamLib: !JAM, JAM_SYNTAX, JAM_MESSAGE, JAM_CHECK, JAM_DIST
;
; PROCEDURE:
; The grain-size distribution function is parametrized by:
;
;   (1/n_H) * dn(a)/da = (1/n_H) * dn(<a>)/da *
;                        (a / <a>)^-ALPHA *
;                        exp{-[(a - <a>) / A_MAX]^BETA_MAX} *
;                        (1 - exp{-[ a  / A_MIN-or-A_SUB]^BETA_MIN}) * F(a)
;
; which provides a power-law with smooth cutoffs at small and large
; grain sizes, with control of the steepness of the cutoff. The default
; parameter values produce a standard MRN distribution:
;
;   ALPHA  A_SUB  A_MIN  A_MAX    BETA_MIN  BETA_MAX  Log10(C)
;   3.5    50 A   50 A   0.25 um   -> Inf    -> Inf   Gra: -25.16
;                                                     Sil: -25.11
;
; The distribution is normalized by requiring that the dust mass per
; hydrogen nucleon, with the minimum grain turnover equal to A_MIN, is
; equivalent to that of an MRN distribution. The actual distribution
; is calculated with the minimum grain turnover equal to A_SUB.
;
; The abundance determining factor (1/n_H) * dn(<a>)/da = N is then
; found from:
;
;   Int[(1/n_H) * dn/da * a^3 * da] = N_MRN / (4-ALPHA_MRN) *
;                         [A_MAX_MRN^(4-ALPHA_MRN) - A_MIN_MRN^(4-ALPHA_MRN)]
;
; REFERENCES:
;
; Mathis, Rumpl, & Nordsieck, ApJ, 217, 1977 (MRN).
; Kim, Martin, & Hendry, ApJ, 422, 1994.
; Weingartner & Draine, ApJ, 548, 2001 (WD).
;
; EXAMPLE:
; To calculate the standard MRN grain-size DF enter:
;   IDL> dnda = Jam_GrainSizeDF()
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, October 2005.
;-

FUNCTION Jam_GrainSizeDF, _a, T_bb, Model=model, C_Table=C_Table, $
                          T_Table=T_Table, dnda_Table=dnda_Table, $
                          aMinValue=aMinValue, aMaxValue=aMaxValue, $
                          Big=big, Small=small, _Extra=extra, Debug=debug

  Compile_Opt IDL2
  On_Error, 2

  ;; Setup debugging.
  IF (Keyword_Set(debug)) THEN BEGIN
     except = !Except
     !Except = 2
  ENDIF

  ;; Calculate grain-size vector.
  a = Jam_GrainRadii()
  N_A = N_Elements(a)
  loga = ALog(a)
  IF (N_Elements(_a) EQ 0) THEN _a = a
  _N_A = N_Elements(_a)

  ;; Set default temperature vector.
  IF (N_Elements(T_bb) EQ 0) THEN T_bb = Jam_T_bb()
  N_T_bb = N_Elements(T_bb)

  ;; Set default model.
  IF (N_Elements(model) EQ 0) THEN model = '-WD01-RV31-'

  ;; Read parameter values.
  IF (N_Elements(dnda_Table) EQ 0) THEN $
     dnda_Table = Jam_ReadData(!Jam.Path.Tables + 'grainsizedf_params.txt')
  idx = Where(dnda_Table.Name EQ model, cnt)
  b_C = ((dnda_Table.b_C)[idx])[0]
  aMin = ((dnda_Table.a_Min)[idx])[0]
  aMax = ((dnda_Table.a_Max)[idx])[0]
  C_Gra = ((dnda_Table.C_G)[idx])[0]
  C_Sil = ((dnda_Table.C_S)[idx])[0]
  alphaGra = ((dnda_Table.Alpha_G)[idx])[0]
  alphaSil = ((dnda_Table.Alpha_S)[idx])[0]
  a_t_Gra = ((dnda_Table.a_t_G)[idx])[0]
  a_t_Sil = ((dnda_Table.a_t_S)[idx])[0]
  a_c_Gra = ((dnda_Table.a_c_G)[idx])[0]
  a_c_Sil = ((dnda_Table.a_c_S)[idx])[0]
  betaGra = ((dnda_Table.Beta_G)[idx])[0]
  betaSil = ((dnda_Table.Beta_S)[idx])[0]
  gammaMin = ((dnda_Table.Gamma_Min)[idx])[0]
  gammaMax = ((dnda_Table.Gamma_Max)[idx])[0]

  ;; Calculate grain-size DF without large and small grain-size cutoffs.
  dndaGra0 = (C_Gra / a) * (a / a_t_Gra)^(-alphaGra)
  dndaGra0 = (betaGra GE 0.D) ? $
             dndaGra0 * (1.D + (betaGra * a / a_t_Gra)) : $
             dndaGra0 / (1.D - (betaGra * a / a_t_Gra))
  dndaSil0 = (C_Sil / a) * (a / a_t_Sil)^(-alphaSil)
  dndaSil0 = (betaSil GE 0.D) ? $
             dndaSil0 * (1.D + (betaSil * a / a_t_Sil)) : $
             dndaSil0 / (1.D - (betaSil * a / a_t_Sil))

  ;; Apply large grain-size cutoff.
  idxGra = Where(a GT a_t_Gra, cntGra)
  idxSil = Where(a GT a_t_Sil, cntSil)
  IF (cntGra GT 0) THEN $
     dndaGra0[idxGra] *= Exp(-((((a[idxGra] - a_t_Gra) / $
                                 a_c_Gra)^gammaMax) < !Jam.LogMax))
  IF (cntSil GT 0) THEN $
     dndaSil0[idxSil] *= Exp(-((((a[idxSil] - a_t_Sil) / $
                                 a_c_Sil)^gammaMax) < !Jam.LogMax))
  
  ;; Apply absolute minimum and maximum grain-size cutoffs.
  idxS = Where(a LT (1D-4 * aMin), cntS)
  IF (cntS GT 0) THEN dndaGra0[idxS] = 0.D
  IF (cntS GT 0) THEN dndaSil0[idxS] = 0.D
  idxB = Where(a GT aMax, cntB)
  IF (cntB GT 0) THEN dndaGra0[idxB] = 0.D
  IF (cntB GT 0) THEN dndaSil0[idxB] = 0.D

  ;; Calculate VSG distribution function.
  m_C = 1.99D-23       ;; Carbon atom mass [g].
  a0_i = [3.5, 30.0]   ;; Log-normal peak locations [A].
  sigma = 0.4          ;; Log-normal width.
  b_C_i = [0.75, 0.25] ;; Fraction of b_C in each peak.
  densityGra = 2.24D   ;; Graphite density in [g cm-3].
  cm_per_ang = 1D-8
  um_per_ang = 1D-4
  dndaVSG0 = 0.D * dndaGra0
  IF (b_C GT 0.D) THEN BEGIN
     B = 3.D / (Sqrt(2.D * !DPi))^3 * $
         Exp(-4.5D * sigma^2) / $
         (densityGra * (cm_per_ang * a0_i)^3 * sigma) * $
         ((1D-6 * b_C) * b_C_i) * m_C / $
         (1.D + Erf((3.D * sigma / Sqrt(2.D)) + $
                    (Sqrt(2.D) * ALog(a0_i / 3.5D) / sigma))) ;; [H-1]
     dndaVSG0 = (B[0] / a) * $
                Exp(-0.5D * (ALog(a / $
                                  (um_per_ang * a0_i[0])) / sigma)^2) + $
                (B[1] / a) * $
                Exp(-0.5D * (ALog(a / $
                                  (um_per_ang * a0_i[1])) / sigma)^2)
     IF (cntS GT 0) THEN dndaVSG0[idxS] = 0.D
     IF (cntB GT 0) THEN dndaVSG0[idxB] = 0.D
  ENDIF

  ;; Calculate carbonaceous grain-size DF.
  dndaCarb0 = dndaGra0 + dndaVSG0

  ;; Calculate effective sublimation grain-size for each temperature.
  N_T_bb = N_Elements(T_bb)
  IF (Total(T_bb) EQ 0.D) THEN BEGIN
     aSub = { Sil:0.D, Carb:0.D }
     aSub = Replicate(aSub, N_T_bb)
  ENDIF ELSE $
     aSub = Jam_GrainMinSize((T_bb > 1.D), C_Table=C_Table, $
                             T_Table=T_Table, _Extra=extra, $
                             Debug=debug)

  ;; Apply small grain-size cutoff for each temperature.
  dndaSil = DblArr(N_A, N_T_bb, /NoZero)
  dndaCarb = DblArr(N_A, N_T_bb, /NoZero)
  FOR i=0,N_T_bb-1 DO BEGIN
     dndaSil[*,i] = dndaSil0 * $
                    (1.D - Exp(-((a / (aSub.Sil[i] > 1D-5))^gammaMin) > $
                               !Jam.LogMin))
     dndaCarb[*,i] = dndaCarb0 * $
                     (1.D - Exp(-((a / (aSub.Carb[i] > 1D-5))^gammaMin) > $
                                !Jam.LogMin))
  ENDFOR
  
  ;; Apply "Big" and "Small" cutoffs.
  IF (Keyword_Set(big)) THEN aMinValue = 50D-4
  IF (Keyword_Set(small)) THEN aMaxValue = 50D-4

  ;; Apply aMinValue and aMaxValue cutoffs.
  IF (N_Elements(aMinValue) NE 0) THEN BEGIN
     idx = Where(a LT aMinValue, cnt)
     IF (cnt GT 0) THEN BEGIN
        dndaSil[idx,*] = 0.D
        dndaCarb[idx,*] = 0.D
     ENDIF
  ENDIF
  IF (N_Elements(aMaxValue) NE 0) THEN BEGIN
     idx = Where(a GT aMaxValue, cnt)
     IF (cnt GT 0) THEN BEGIN
        dndaSil[idx,*] = 0.D
        dndaCarb[idx,*] = 0.D
     ENDIF
  ENDIF

  ;; Interpolate to find values at input radii.
  IF (~Array_Equal(a, _a)) THEN BEGIN
     _loga = ALog(_a)
     IF (N_T_bb EQ 1) THEN BEGIN
        dndaSil = Exp(Interpol(ALog(dndaSil), loga, _loga))
        dndaCarb = Exp(Interpol(ALog(dndaCarb), loga, _loga))
     ENDIF $
     ELSE BEGIN
        idx_T_bb = LIndGen(N_T_bb)
        idx_a = Interpol(LIndGen(N_A), loga, _loga)
        dndaSil = Exp(Interpolate(ALog(dndaSil), idx_a, idx_T_bb, /Grid, $
                                 Cubic=-0.5))
        dndaCarb = Exp(Interpolate(ALog(dndaCarb), idx_a, idx_T_bb, /Grid, $
                                  Cubic=-0.5))
     ENDELSE
  ENDIF

  IF (Keyword_Set(debug)) THEN !Except = except

  RETURN, { a:_a, $
            T_bb:T_bb, $
            Sil:(N_Elements(dndaSil) EQ 1) ? dndaSil[0] : Reform(dndaSil), $
            Carb:(N_Elements(dndaCarb) EQ 1) ? dndaCarb[0] : Reform(dndaCarb) }

END
