
FUNCTION Jam_HACOpacity, wave, HAC_Table=HAC_Table

  Compile_Opt IDL2
  On_Error, 2

  ;; Read ice opacity table.
  IF (N_Elements(hac_Table) EQ 0) THEN $
     hac_Table = Jam_ReadData(!Jam.Path.Tables + 'hac_opacity.txt')

  ;; Set default wavelength vector.
  IF (N_Elements(wave) EQ 0) THEN wave = hac_Table.Wave

  ;; Calculate opacity at input wavelength grid.
  wave_Tab = hac_Table.Wave
  K_Tab = hac_Table.Opacity
  tau = Interpol(K_Tab, ALog(wave_Tab), ALog(wave))
  idxTab = Where(K_Tab NE 0.D)
  idx = Where((wave LT Min(wave_Tab[idxTab])) OR $
              (wave GT Max(wave_Tab[idxTab])), cnt)
  IF (cnt GT 0) THEN tau[idx] = 0.D

  RETURN, tau

END
