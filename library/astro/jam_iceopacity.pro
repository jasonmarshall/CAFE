
FUNCTION Jam_IceOpacity, wave, Ice_Table=ice_Table

  Compile_Opt IDL2
  On_Error, 2

  ;; Read ice opacity table.
  IF (N_Elements(ice_Table) EQ 0) THEN $
     ice_Table = Jam_ReadData(!Jam.Path.Tables + 'ice_opacity.txt')

  ;; Set default wavelength vector.
  IF (N_Elements(wave) EQ 0) THEN wave = ice_Table.Wave

  ;; Calculate opacity at input wavelength grid.
  wave_Tab = ice_Table.Wave
  K_Tab = ice_Table.Opacity
  tau = Interpol(K_Tab, ALog(wave_Tab), ALog(wave))
  idxTab = Where(K_Tab NE 0.D)
  idx = Where((wave LT Min(wave_Tab[idxTab])) OR $
              (wave GT Max(wave_Tab[idxTab])), cnt)
  IF (cnt GT 0) THEN tau[idx] = 0.D

  RETURN, tau

END
