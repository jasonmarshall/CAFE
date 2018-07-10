
FUNCTION Jam_MakeCont, wave, flux, XRange=xRange, _Extra=extra

  Compile_Opt IDL2
  On_Error, 2

  IF (N_Elements(xRange) EQ 0) THEN xRange = MinMax(wave)
  Plot, wave, flux, XStyle=1, XRange=xRange, _Extra=extra

  Print, 'Left click on 1st point...'
  Cursor, wave1, flux1, /Down, /Data
  Print, 'Left click on 2nd point...'
  Cursor, wave2, flux2, /Down, /Data

  slope = (flux2 - flux1) / (wave2 - wave1)
  cont = flux1 + slope * (wave - wave1)

  OPlot, wave, cont, LineStyle=1

  RETURN, cont

END
