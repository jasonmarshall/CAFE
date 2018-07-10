
FUNCTION Jam_GrainLumDF, a, T_bb, Gra=gra, _Extra=extra

  Compile_Opt IDL2
  On_Error, 2

  N_a = N_Elements(a)
  dloga = ALog(a[1] / a[0])

  C_Abs = Jam_GrainCrossSection(wave, a, _Extra=extra)
  C_Sil = C_Abs.Sil
  C_Carb = (Keyword_Set(gra)) ? C_Abs.Gra : $
           0.5D * (C_Abs.Carb.Neu.Cont + C_Abs.Carb.Ion.Cont)

  N_Wave = N_Elements(wave)
  dlogWave = ALog(wave[1] / wave[0])

  T_EQ = Jam_GrainEQTemp(a, T_bb, Gra=gra, C_Abs=C_Abs, _Extra=extra)
  dnda = Jam_GrainSizeDF(a, T_bb, T_EQ=T_EQ, C_Abs=C_Abs, _Extra=extra)

  planck_Sil = Jam_Planck(wave, T_EQ.Sil, /um)
  planck_Carb = Jam_Planck(wave, T_EQ.Carb, /um)

  big_Wave = Rebin(wave, N_Wave, N_a)
  f_Sil = dnda.Sil * Jam_IntTab(big_Wave * C_Sil * planck_Sil, dlogWave)
  f_Carb = dnda.Carb * Jam_IntTab(big_Wave * C_Carb * planck_Carb, dlogWave)

  f_Sil_Tot = Jam_IntTab(a * f_Sil, dloga)
  f_Carb_Tot = Jam_IntTab(a * f_Carb, dloga)

  f_Tot = f_Sil_Tot + f_Carb_Tot

  f_Sil /= f_Tot
  f_Carb /= f_Tot

  RETURN, { a:a, Sil:f_Sil, Carb:f_Carb }

END
