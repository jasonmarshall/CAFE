
FUNCTION Jam_GrainMeanX, a, T_bb, X_Carb, X_Sil, _Extra=extra

  Compile_Opt IDL2
  On_Error, 2

  N_A = N_Elements(a)
  dloga = ALog(a[1] / a[0])

  DF = Jam_GrainLumDF(a, T_bb, _Extra=extra)

  X_Tot_Carb = Jam_IntTab(a * X_Carb * DF.Carb, dloga)
  X_Tot_Sil = Jam_IntTab(a * X_Sil * DF.Sil, dloga)

  RETURN, X_Tot_Carb + X_Tot_Sil

END
