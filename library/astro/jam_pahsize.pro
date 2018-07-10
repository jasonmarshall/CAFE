
FUNCTION Jam_PAHSize, size, a=a, N_C=N_C
  
  Compile_Opt IDL2
  On_Error, 2

  IF (Keyword_Set(a)) THEN thisSize = 460.D * (size / 10.D)^3
  IF (Keyword_Set(N_C)) THEN thisSize = 10.D * (thisSize / 460.D)^(1.D / 3.D)
  IF (N_Elements(thisSize) EQ 0) THEN thisSize = 460.D * (size / 10.D)^3
  
  RETURN, thisSize
  
END
