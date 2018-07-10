
FUNCTION Jam_GrainMeanRadius, T_bb, Err_T_bb=err_T_bb, $
                              Err_MeanRadius=err_MeanRadius, $
                              C_Table=C_Table, T_Table=T_Table, $
                              C_Abs=C_Abs, _Extra=extra

  Compile_Opt IDL2
  On_Error, 2

  a = Jam_GrainRadii()
  meanRadius = Jam_GrainMeanX(a, T_bb, a, a, _Extra=extra)

  ;; Calculate mean radius uncertainty.
  IF ((Arg_Present(err_MeanEQTemp)) AND (N_Elements(err_T_bb) NE 0)) THEN BEGIN
     derivs = Jam_Deriv('Jam_GrainMeanRadius', T_bb, PError=err_T_bb, $
                        C_Table=C_Table, T_Table=T_Table, C_Abs=C_Abs, $
                        _Extra=extra)
     err_MeanRadius = Jam_EFunc(derivs, err_T_bb)
  ENDIF ELSE $
     err_MeanRadius = !Values.D_NaN

  RETURN, meanRadius

END
