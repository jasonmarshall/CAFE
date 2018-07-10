
FUNCTION Jam_PAHOpacity, wave, C_Table=C_Table, T_Table=T_Table, $
                         dnda_Table=dnda_Table, C_Abs=_C_Abs, _dnda=_dnda, $
                         _Extra=extra, Debug=debug

  Compile_Opt IDL2
  On_Error, 2

  ;; Setup debug.
  IF (Keyword_Set(debug)) THEN BEGIN
     except = !Except
     !Except = 2
  ENDIF

  ;; Set default grain radius vector.
  a = Jam_GrainRadii(NumRadii=N_a)
  loga = ALog(a)
  dloga = ALog(a[1] / a[0])
  N_Wave = N_Elements(wave)
  big_a = Rebin(a, N_a, N_Wave)

  ;; Calculate absorption cross sections.
  IF (N_Elements(_C_Abs) EQ 0) THEN BEGIN
     _C_Abs = Jam_GrainCrossSection(wave, a, C_Table=C_Table, $
                                    _Extra=extra, Debug=debug)
     C_Abs = _C_Abs
  ENDIF ELSE $
     C_Abs = _C_Abs
  C_PAH = Transpose(C_Abs.Carb.Neu.Feat) ;; [N_a x N_Wave]

  ;; Obtain the grain-size DF.
  IF (N_Elements(_dnda) EQ 0) THEN BEGIN
     _dnda = Jam_GrainSizeDF(a, 0.D, C_Table=C_Table, T_Table=T_Table, $
                             dnda_Table=dnda_Table, _Extra=extra)
     dnda = _dnda
  ENDIF

  ;; Calculate PAH opacity.
  big_dnda_Carb = Rebin(dnda.Carb, N_a, N_Wave)
  int_Carb = big_a * big_dnda_Carb
  K_PAH = Jam_IntTab(int_Carb * C_PAH, dloga)

  ;; Create return vectors.
  idx = Where(wave LT 1D-3, cnt)
  IF (cnt GT 0) THEN K_PAH[idx] = 0.D

  IF (Keyword_Set(debug)) THEN !Except = except

  RETURN, K_PAH / Interpol(K_PAH, wave, 6.22D)

END
