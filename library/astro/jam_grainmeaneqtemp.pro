
FUNCTION Jam_GrainMeanEQTemp, T_bb, Err_T_bb=err_T_bb, $
                              Err_MeanEQTemp=err_MeanEQTemp, $
                              C_Table=C_Table, T_Table=T_Table, $
                              C_Abs=C_Abs, _Extra=extra

  Compile_Opt IDL2
  On_Error, 2

  ;; Create return arrays.
  N_T_bb = N_Elements(T_bb)
  T_MeanEQTemp = DblArr(N_T_bb)
  err_MeanEQTemp = DblArr(N_T_bb)
  T_bb = double(T_bb)

  ;; Calculate equilibrium temperatures.
  T_EQ = Jam_GrainEQTemp(a, T_bb, C_Table=C_Table, T_Table=T_Table, $
                         C_Abs=C_Abs, _Extra=extra)

  ;; Loop over blackbody temperatures.
  FOR i=0,N_T_bb-1 DO BEGIN
     
     ;; Calculate mean temperature.
     T_MeanEQTemp[i] = Jam_GrainMeanX(a, T_bb[i], (T_EQ.Carb)[*,i], $
                                      (T_EQ.Sil)[*,i], _Extra=extra)
     
     ;; Calculate mean temperature uncertainty.
     IF ((Arg_Present(err_MeanEQTemp)) AND $
         (N_Elements(err_T_bb) NE 0)) THEN BEGIN
        derivs = Jam_Deriv('Jam_GrainMeanEQTemp', T_bb[i], $
                           PError=err_T_bb[i], C_Table=C_Table, $
                           T_Table=T_Table, C_Abs=C_Abs, _Extra=extra)
        err_MeanEQTemp[i] = Jam_EFunc(derivs, err_T_bb[i])
     ENDIF ELSE $
        err_MeanEQTemp = !Values.D_NaN

  ENDFOR
  err_MeanEQTemp = (N_T_bb EQ 1) ? err_MeanEQTemp[0] : err_MeanEQTemp

  RETURN, (N_T_bb EQ 1) ? T_MeanEQTemp[0] : T_MeanEQTemp

END
