
FUNCTION Jam_GetScreenSize

  Compile_Opt IDL2
  On_Error, 2

  screenSize = Get_Screen_Size()
  IF (screenSize[0] GT (2.D * screenSize[1])) THEN screenSize[0] *= 0.5D

  RETURN, screenSize

END
