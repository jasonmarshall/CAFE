
FUNCTION Jam_Dist, N_DAT, MIN_DAT, MAX_DAT, Linear=linear

  Compile_Opt IDL2, Hidden
  On_Error, 2
  
  IF (N_DAT EQ 1) THEN BEGIN
     RETURN, MIN_DAT
  ENDIF $
  ELSE BEGIN
     RETURN, (Keyword_Set(linear)) ? $
       Double(MIN_DAT) + DIndGen(N_DAT) * $
         (Double(MAX_DAT) - Double(MIN_DAT)) / (N_DAT - 1.D) : $
       10^(ALog10(Double(MIN_DAT)) + $
           ALog10(Double(MAX_DAT) / Double(MIN_DAT)) * $
           DIndGen(N_DAT)/(N_DAT - 1.D))
  ENDELSE
  
END
