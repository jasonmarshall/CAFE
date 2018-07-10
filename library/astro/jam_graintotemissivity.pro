;+ ===========================================================================
; NAME:
;       Jam_GrainTotEmissivity
;
; PURPOSE:
;       This function....
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_GrainTotEmissivity()
;
; RETURN VALUE:
;       By default, emissivities are returned in [Jy sr-1 H-1 cm+2].
;
; AUTHOR:
;       Jason A. Marshall
;       Department of Astronomy, Cornell University, Ithaca, NY 14853
;       jam258@cornell.edu
;
; MODIFICATION HISTORY:
;       Written by: Jason A. Marshall, 04 May 2006.
;-
;; ---------------------------------------------------------------------------
; Copyright (C) 2006, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ---------------------------------------------------------------------------

FUNCTION Jam_GrainTotEmissivity, wave, T_bb_Max, EtaMax=etaMax, fCarb=fCarb, $
                                 fSil=fSil, fCry=fCry, fEns=fEns, $
                                 N_Eta=N_Eta, E_T=E_T, jSil=jSil, $
                                 jCry=jCry, jAmo=jAmo, _Extra=extra, $
                                 Debug=debug

  ;; Set compiler options and error handling.
  Compile_Opt IDL2
  IF (~Keyword_Set(debug)) THEN On_Error, 2
  
  ;; Set debugging options.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  IF (N_Elements(etaMax) EQ 0) THEN etaMax = 1.D
  IF (N_Elements(fCarb) EQ 0) THEN fCarb = 1.D
  IF (N_Elements(fSil) EQ 0) THEN fSil = 1.D
  IF (N_Elements(fCry) EQ 0) THEN fCry = 0.D
  IF (N_Elements(fEns) EQ 0) THEN fEns = 0.5D
  IF (N_Elements(E_T) EQ 0) THEN E_T = Jam_GrainEmissivity(wave, T_bb, _Extra=extra)

  j = fCarb * E_T.Carb + $
      fSil * $
      ((1.D - fCry) * E_T.Sil.Amo.Tot + $
       fCry * ((1.D - fEns) * E_T.Sil.Fst.Tot + $
               fEns * E_T.Sil.Ens.Tot))
  jSilDo = Arg_Present(jSil)
  jCryDo = Arg_Present(jCry)
  jAmoDo = Arg_Present(jAmo)
  IF (jSilDo) THEN BEGIN
     jSil = fCarb * E_T.Carb + $
            fSil * $
            ((1.D - fCry) * E_T.Sil.Amo.Cont + $
             fCry * ((1.D - fEns) * E_T.Sil.Fst.Cont + $
                     fEns * E_T.Sil.Ens.Cont))
  ENDIF
  IF (jCryDo) THEN BEGIN
     jCry = fCarb * E_T.Carb + $
            fSil * $
            ((1.D - fCry) * E_T.Sil.Amo.Tot + $
             fCry * ((1.D - fEns) * E_T.Sil.Fst.Cont + $
                     fEns * E_T.Sil.Ens.Cont))
  ENDIF
  IF (jAmoDo) THEN BEGIN
     jAmo = fCarb * E_T.Carb + $
            fSil * $
            ((1.D - fCry) * E_T.Sil.Amo.Cont + $
             fCry * ((1.D - fEns) * E_T.Sil.Fst.Tot + $
                     fEns * E_T.Sil.Ens.Tot))
  ENDIF

  N_Wave = N_Elements(wave)
  idxWave = LIndGen(N_Wave)
  IF (etaMax GT 1.D) THEN BEGIN
     IF (N_Elements(N_Eta) EQ 0) THEN N_Eta = 10
     eta = Jam_Dist(N_Eta, 1.D, etaMax)
     dlogEta = ALog(eta[1] / eta[0])
     T_bb = T_bb_Max / Sqrt(eta)
     idx_T_bb = Interpol(LIndGen(N_Elements(E_T.T_bb)), $
                         ALog(E_T.T_bb), ALog(T_bb))
     j = Transpose(Exp(Interpolate(ALog(j), idxWave, idx_T_bb, /Grid)))
     DF = Rebin(eta^3, N_Eta, N_Wave) * j ;; = eta * eta^2 * j
     jTot = Jam_IntTab(DF, dlogEta) / ((etaMax^3 - 1.D) / 3.D)
     IF (jSilDo) THEN BEGIN
        j = Transpose(Exp(Interpolate(ALog(jSil), idxWave, idx_T_bb, /Grid)))
        DF = Rebin(eta^3, N_Eta, N_Wave) * j ;; = eta * eta^2 * j
        jSil = Jam_IntTab(DF, dlogEta) / ((etaMax^3 - 1.D) / 3.D)
     ENDIF
     IF (jCryDo) THEN BEGIN
        j = Transpose(Exp(Interpolate(ALog(jCry), idxWave, idx_T_bb, /Grid)))
        DF = Rebin(eta^3, N_Eta, N_Wave) * j ;; = eta * eta^2 * j
        jCry = Jam_IntTab(DF, dlogEta) / ((etaMax^3 - 1.D) / 3.D)
     ENDIF
     IF (jAmoDo) THEN BEGIN
        j = Transpose(Exp(Interpolate(ALog(jAmo), idxWave, idx_T_bb, /Grid)))
        DF = Rebin(eta^3, N_Eta, N_Wave) * j ;; = eta * eta^2 * j
        jAmo = Jam_IntTab(DF, dlogEta) / ((etaMax^3 - 1.D) / 3.D)
     ENDIF
  ENDIF $
  ELSE BEGIN
     idx_T_bb = Interpol(LIndGen(N_Elements(E_T.T_bb)), $
                         ALog(E_T.T_bb), ALog(T_bb_Max))
     jTot = Exp(Interpolate(ALog(j), idxWave, idx_T_bb, /Grid))
     IF (jSilDo) THEN $
        jSil = Exp(Interpolate(ALog(jSil), idxWave, idx_T_bb, /Grid))
     IF (jCryDo) THEN $
        jCry = Exp(Interpolate(ALog(jCry), idxWave, idx_T_bb, /Grid))
     IF (jAmoDo) THEN $
        jAmo = Exp(Interpolate(ALog(jAmo), idxWave, idx_T_bb, /Grid))
  ENDELSE

  IF (~Array_Equal(wave, E_T.Wave)) THEN BEGIN
     jTot = Exp(Spline(ALog(E_T.Wave), ALog(jTot), ALog(wave)))
     IF (jSilDo) THEN $
        jSil = Exp(Spline(ALog(E_T.Wave), ALog(jSil), ALog(wave)))
     IF (jCryDo) THEN $
        jCry = Exp(Spline(ALog(E_T.Wave), ALog(jCry), ALog(wave)))
     IF (jAmoDo) THEN $
        jAmo = Exp(Spline(ALog(E_T.Wave), ALog(jAmo), ALog(wave)))
  ENDIF

  ;; Restore debugging options.
  !Except = except

  RETURN, jTot

END
