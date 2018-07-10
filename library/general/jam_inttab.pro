
FUNCTION Jam_IntTab, f, h
  
  Compile_Opt IDL2
  On_Error, 2
  
  ;; Calculate the number of extra points at the end of the integrand.
  xSeg = (Size(f))[1] - 1L
  xExt = (xSeg MOD 4L) + 1L

  ;; Compute the integral using the 5-point Newton-Cotes formula.
  idx = (LIndGen(xSeg / 4L) + 1L) * 4
  int = Total(2.D * h * (7.D * (f[idx-4,*,*] + f[idx,*,*]) + $
    32.D * (f[idx-3,*,*] + f[idx-1,*,*]) + 12.D * f[idx-2,*,*]) / 45.D, 1)
  
  ;; Integrate over the remaining points using 2-, 3-, or 4-point 
  ;; Newton-Cotes formulae.
  CASE (xExt) OF
     1:
     2: int += (0.5D * h * (f[xSeg-1,*,*] + f[xSeg,*,*]))
     3: int += ((h/3.D) * (f[xSeg-2,*,*] + 4.D * f[xSeg-1,*,*] + f[xSeg,*,*]))
     4: int += ((3.D * h / 8.D) * (f[xSeg-3,*,*] + f[xSeg,*,*] + $
          3.D * (f[xSeg-2,*,*] + f[xSeg-1,*,*])))
  ENDCASE
  
  RETURN, (N_Elements(int) EQ 1) ? int[0] : Reform(int)
  
END
