
FUNCTION Jam_um2Jy, wave, flux
  
  Compile_Opt IDL2, Hidden
  On_Error, 2
  
  RETURN, 1D23 / 3D14 * wave^2 * flux
  
END
