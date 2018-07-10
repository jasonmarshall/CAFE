;+ ===========================================================================
; NAME:
;       Jam_SourceSED
;
; PURPOSE:
;       This function....
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_SourceSED()
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
; Copyright (C) 2006, 2007, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ---------------------------------------------------------------------------

FUNCTION Jam_SourceSED_ISRF, wave, T_bb=T_bb, Chi_MMP=chi_MMP

  Compile_Opt IDL2
  On_Error, 2

  ;; Wave is a vector
  ;; T_bb is a scalar
  ;; Chi_MMP is an output scalar

  ;; Create u_Wave vector.
  nWave = N_Elements(wave)
  u_Wave = DblArr(nWave)

  ;; Calculate UV part.
  idx = Where((wave GE 0.0912D) AND (wave LT 0.11D), cnt)
  IF (cnt GT 0) THEN u_Wave[idx] = (38.57D / 3D10) * (wave[idx])^(3.4172D)
  idx = Where((wave GE 0.11D) AND (wave LT 0.134D), cnt)
  IF (cnt GT 0) THEN u_Wave[idx] = 2.045D-2 / 3D10
  idx = Where((wave GE 0.134D) AND (wave LT 0.246D), cnt)
  IF (cnt GT 0) THEN u_Wave[idx] = (7.115D-4 / 3D10) * (wave[idx])^(-1.6678D)

  ;; Calculate blackbody part.
  idx = Where(wave GE 0.246D, cnt)
  IF (cnt GT 0) THEN BEGIN
     W = 4.D * !DPi * [1D-14, 1D-13, 4D-13] / 3D10
     T = [7.5D3, 4D3, 3D3]
     planck = Jam_Planck(wave[idx], T, /um)
     u_Wave[idx] = $
        W[0] * planck[*,0] + W[1] * planck[*,1] + W[2] * planck[*,2]
  ENDIF

  ;; Scale to T_bb.
  IF ((N_Elements(T_bb) NE 0) OR (Arg_Present(T_bb))) THEN BEGIN
     sigma_SB = 5.67D-5 ;; [ergs s-1 cm-2 K-4]
     u_Wave_Old = u_Wave
     u_Tot_Old = Int_Tabulated(ALog(wave), wave * u_Wave_Old)
     T_bb_Old = (3D10 / 4.D / sigma_SB * u_Tot_Old)^0.25D
     IF (N_Elements(T_bb) NE 0) THEN BEGIN
        u_Wave *= (T_bb / T_bb_Old)^4
        IF (Arg_Present(chi_MMP)) THEN BEGIN
           idx = (Where(u_Wave EQ Max(u_Wave)))[0]
           chi_MMP = u_Wave[idx] / u_Wave_Old[idx]
        ENDIF
     ENDIF ELSE $
        T_bb = T_bb_Old
  ENDIF

  RETURN, 3D10 * u_Wave ;; [erg s-1 cm-2 um-1]

END

;; ...........................................................................

FUNCTION Jam_SourceSED_SB, _wave, age, T_bb=T_bb, L_Tot=L_Tot, R=R, $
                           Nebular=nebular

  Compile_Opt IDL2
  On_Error, 2

  ;; Wave is a vector
  ;; T_bb is a scalar

  ;; Error if wrong age.
  IF ((age NE '2') AND (age NE '10') AND (age NE '100')) THEN $
     Message, 'Invalid Starburst age!'

  ;; Read SB99 file.
  file = 'sb99-' + age + 'myr.txt'
  data = Jam_ReadData(FilePath(file, Root_Dir=!Jam.Path.Tables))
  wave = data.Wave
  f_Wave = (Keyword_Set(nebular)) ? 10^(data.Flux) : 10^(data.Stellar)

  ;; Convert wavelength from [A]->[um].
  wave /= 1D4

  ;; Convert flux from [erg s-1 cm-2 A-1]->[erg s-1 cm-2 um-1].
  f_Wave *= 1D4

  ;; Interpolate to input wavelength grid.
  IF (~Array_Equal(wave, _wave)) THEN BEGIN
     f_Wave = Interpol(f_Wave, ALog(wave), ALog(_wave))
     wave = _wave
  ENDIF

  ;; Cut-off flux at long wavelengths for 2-Myr.
  IF (age EQ '2') THEN BEGIN
     idx = Where(wave GT 70.D, cnt)
     IF (cnt GT 0) THEN f_Wave[idx] *= Exp(-(wave[idx] - 70.D) / 70.D)
  ENDIF

  ;; Zero flux for wave < 912 Ang.
;;   idx = Where(wave LT 0.0912D, cnt)
;;   IF (cnt GT 0) THEN f_Wave[idx] = 0.D

  ;; Calculate total 'luminosity' in erg s-1 cm-2.
  L_Tot = Int_Tabulated(ALog(wave), wave * f_Wave)

  ;; Scale to T_bb.
  IF ((N_Elements(T_bb) NE 0) OR (Arg_Present(T_bb))) THEN BEGIN
     sigma_SB = 5.67D-5 ;; [ergs s-1 cm-2 K-4]
     L_Tot_Old = L_Tot
     T_bb_Old = (3D10 / 4.D / sigma_SB * L_Tot_Old)^0.25D
     IF (N_Elements(T_bb) NE 0) THEN BEGIN
        f_Wave *= (T_bb / T_bb_Old)^4
     ENDIF ELSE $
        T_bb = T_bb_Old
  ENDIF

  ;; Calculate new total luminosity and R.
  L_Tot_Old = L_Tot
  L_Tot = Int_Tabulated(ALog(wave), wave * f_Wave)
  R = Sqrt(L_Tot / L_Tot_Old)

  RETURN, f_Wave ;; [erg s-1 cm-2 um-1]

END

;; ...........................................................................

FUNCTION Jam_SourceSED_AGN, wave, T_bb=T_bb, L_Tot=L_Tot, R=R

  Compile_Opt IDL2
  On_Error, 2

  ;; Wave is a vector
  ;; T_bb is a scalar

  ;; Create L_Freq vector.
  nWave = N_Elements(wave)
  L_Freq = DblArr(nWave)

  ;; Region A -> 1D-3 < wave/um < 5D-2
;;   idx = Where(wave GE 1D-3, cnt)
;;   IF (cnt GT 0) THEN $
;;      L_Freq[idx] = (wave[idx] / 1D-3)^3
  L_Freq = (wave / 1D-3)^3

  ;; Region B -> 5D-2 < wave/um < 0.1216
  idx = Where(wave GT 5D-2, cnt)
  IF (cnt GT 0) THEN $
     L_Freq[idx] = Interpol(L_Freq, wave, 5D-2) * $
                   (wave[idx] / 5D-2)^1.8D

  ;; Region C -> 0.1216 < wave/um < 10
  idx = Where(wave GT 0.1216D, cnt)
  IF (cnt GT 0) THEN $
     L_Freq[idx] = Interpol(L_Freq, wave, 0.1216D) * $
                   (wave[idx] / 0.1216D)^0.46D

  ;; Region D -> 10 < wave/um < 1D3
  planck = Jam_Planck(wave, 1D3, /Hz)
  idx = Where(wave GT 10.D)
  IF (cnt GT 0) THEN $
     L_Freq[idx] = Interpol(L_Freq, wave, 10.D) * $
                   (planck[idx] / Interpol(planck, wave, 10.D))

  ;; Convert to L_wave.
  L_Wave = L_Freq / wave^2

  ;; Normalize L_wave.
  L_Wave /= Int_Tabulated(ALog(wave), wave * L_Wave)

  ;; Calculate f_Wave a distance R away from L_Tot source.
  IF (N_Elements(L_Tot) EQ 0) THEN L_Tot = 1D11 ;; [L_Sun]
  IF ((N_Elements(R) EQ 0) OR (N_Elements(T_bb) NE 0)) THEN R = 1.D ;; [pc]
  cgs_Per_LSun = 3.827D33 ;; [erg s-1 LSun-1]
  cm_Per_pc = 3.086D18 ;; [cm pc-1]
  ;; f_Wave = [erg s-1 cm-2]
  f_Wave = (cgs_Per_LSun * L_Tot) * L_Wave / (4.D * !DPi * (cm_Per_pc * R)^2)

  ;; Scale to T_bb.
  IF ((N_Elements(T_bb) NE 0) OR (Arg_Present(T_bb))) THEN BEGIN
     sigma_SB = 5.67D-5 ;; [ergs s-1 cm-2 K-4]
     f_Wave_Old = f_Wave
     f_Tot_Old = Int_Tabulated(ALog(wave), wave * f_Wave_Old)
     T_bb_Old = (f_Tot_Old / (4.D * sigma_SB))^0.25D
     IF (N_Elements(T_bb) NE 0) THEN BEGIN
        scale = (T_bb / T_bb_Old)^4
        f_Wave *= scale
        f_Tot = f_Tot_Old * scale
        R /= Sqrt(scale)
     ENDIF ELSE $
        T_bb = T_bb_Old
  ENDIF

  RETURN, f_Wave ;; [erg s-1 cm-2 um-1]

END

;; ...........................................................................

FUNCTION Jam_SourceSED, wave, source, IR=IR, T_bb=T_bb, Hz=Hz, Jy=Jy, $
                        Chi_MMP=chi_MMP, L_Tot=L_Tot, R=R, Wave0=wave0, $
                        Tau0=tau0, Nebular=nebular, _Extra=extra, Debug=debug

  ;; Set compiler options and error handling.
  Compile_Opt IDL2
  IF (~Keyword_Set(debug)) THEN On_Error, 2

  ;; Set debugging options.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  ;; Set default wavelength vector.
  IF ((N_Elements(wave) EQ 0) OR (Keyword_Set(IR))) THEN $
     wave = Jam_Wave(IR=IR)

  ;; Set default source.
  IF (N_Elements(source) EQ 0) THEN source = 'ISRF'

  ;; Obtain desired SED.
  CASE (StrUpCase(source)) OF
     'ISRF': f_Wave = Jam_SourceSED_ISRF(wave, T_bb=T_bb, Chi_MMP=chi_MMP)
     'ISRF0': f_Wave = Jam_SourceSED_ISRF(wave, T_bb=T_bb, Chi_MMP=chi_MMP)
     'SB2MYR': f_Wave = Jam_SourceSED_SB(wave, '2', T_bb=T_bb, $
                                         L_Tot=L_Tot, R=R, Nebular=nebular)
     'SB2MYR0': f_Wave = Jam_SourceSED_SB(wave, '2', T_bb=T_bb, $
                                          L_Tot=L_Tot, R=R, Nebular=nebular)
     'SB10MYR': f_Wave = Jam_SourceSED_SB(wave, '10', T_bb=T_bb, $
                                          L_Tot=L_Tot, R=R, Nebular=nebular)
     'SB10MYR0': f_Wave = Jam_SourceSED_SB(wave, '10', T_bb=T_bb, $
                                           L_Tot=L_Tot, R=R, Nebular=nebular)
     'SB100MYR': f_Wave = Jam_SourceSED_SB(wave, '100', T_bb=T_bb, $
                                          L_Tot=L_Tot, R=R, Nebular=nebular)
     'SB100MYR0': f_Wave = Jam_SourceSED_SB(wave, '100', T_bb=T_bb, $
                                           L_Tot=L_Tot, R=R, Nebular=nebular)
     'AGN': f_Wave = Jam_SourceSED_AGN(wave, T_bb=T_bb, L_Tot=L_Tot, R=R)
     'AGN0': f_Wave = Jam_SourceSED_AGN(wave, T_bb=T_bb, L_Tot=L_Tot, R=R)
     ELSE: Message, 'Invalid source SED type!'
  ENDCASE

  ;; Set points with zero flux equal to the minimum non-zero flux.
;;   idx = Where(f_Wave EQ 0.D, cnt, Complement=cIdx)
;;   IF (cnt GT 0) THEN f_Wave[idx] = Min(f_Wave[cIdx])

  ;; Extinguish 'ISRF0', 'SB...0', and 'AGN0' SEDs.
  IF (StrMid(source, 0, 1, /Reverse_Offset) EQ '0') THEN BEGIN

     ;; Calculate the wavelength of peak energy absorption.
     waveSrc = Jam_Wave()
     K = Jam_GrainOpacity(waveSrc, 0.D, _Extra=extra)
     K = K.Carb + K.Sil.Amo.Tot
     wave0 = Int_Tabulated(ALog(waveSrc), waveSrc^2 * K * f_Wave) / $
             Int_Tabulated(ALog(waveSrc), waveSrc * K * f_Wave)

     ;; Calculate tau(wave0)=1 vector.
     K = Jam_GrainOpacity(wave, 0.D, _Extra=extra)
     K = K.Carb + K.Sil.Amo.Tot
     K0 = Exp(Interpol(ALog(K), ALog(wave), ALog(wave0)))
     tau = K / K0

     ;; Extinguish SED.
     f_Wave *= Exp(-tau)

     ;; Calculate tau0=tau(9.7um).
     tau0 = Interpol(tau, ALog(wave), ALog(9.7D))
     
  ENDIF ELSE $
     tau0 = 0.D

  ;; Convert flux to different units.
  flux = f_Wave
  IF (Keyword_Set(Hz)) THEN flux *= (wave^2 / 3D14)
  IF (Keyword_Set(Jy)) THEN flux *= ((1D23 / 3D14) * wave^2)

  ;; Restore debugging options.
  !Except = except

  RETURN, flux

END
