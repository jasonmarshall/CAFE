;+ ====================================================================
; NAME:
;   JAM_GRAINCROSSSECTION
;
; PURPOSE:
;   This function returns absorption cross sections of spherical
;   astronomical silicate and carbonaceous dust grains as a function
;   of grain radius and wavelength.
;
; CATEGORY:
;   Astronomy, Physics.
;
; CALLING SEQUENCE:
;   Result = JAM_GRAINCROSSSECTION([Wave, a])
;
; KEYWORD PARAMETERS:
;   C_ABS:
;   E:
;
; OUTPUTS:
;   This function returns an anonymous structure of absorption cross
;   sections in units of cm^2. The structure takes the form:
;
;     { Result }.
;               |- [ <Wave> ]
;               |- [ <a> ]
;               |- [[ Sil ]]
;               |- { Carb }.
;                          |- { Neu }.
;                                    |- [[  Tot ]]
;                                    |- [[ Cont ]]
;                                    |- [[ Feat ]]
;                          |- { Ion }.
;                                    |- [[  Tot ]]
;                                    |- [[ Cont ]]
;                                    |- [[ Feat ]]
;
;   The fields 'Wave' and 'a' contain the wavelength and grain radius
;   vectors corresponding to the first and second indices of the cross
;   section arrays for each grain type.
;
; RESTRICTIONS:
;   <JamLib>
;   JAM_SYNTAX, JAM_MESSAGE, JAM_CHECK, JAM_DIST, JAM_READDATA,
;   JAM_COMPARE, JAM_DRUDEFLUX
;
; PROCEDURE:
;
;   Absorption efficiencies, Q_Abs, of silicate and graphite grains
;   are calculated according to the methods described in Laor &
;   Draine (1993) [see http://www.astro.princeton.edu/~draine/dust/]
;   and C_Abs(Wave,a) = Pi * a^2 * Q_Abs(Wave,a).
;
;   The absorption cross sections of PAHs are calculated according
;   to the model described in Li & Draine (2001) in which carbon-
;   aceous grains have PAH properties at small radii and graphitic
;   properties at large radii.
;
; REFERENCES:
;   Laor & Draine, ApJ, 402, 1993.
;   Li & Draine, ApJ, 554, 2001.
;
; EXAMPLE:
;   To obtain absorption cross sections of astronomical dust enter:
;     IDL> C_Abs = JAM_GRAINCROSSSECTION()
;
; MODIFICATION HISTORY:
;   Written by: Jason Marshall, October 2005.
;   2005 Dec 01 - JAM - Added PAH grain properties.
;-

FUNCTION Jam_GrainCrossSection, wave, a, C_Ext=C_Ext, IR=IR, $
                                C_Table=C_Table, ScaleSIL=scaleSIL, $
                                Debug=debug

  Compile_Opt IDL2
  On_Error, 2

  ;; Setup debugging.
  IF (Keyword_Set(debug)) THEN BEGIN
     except = !Except
     !Except = 2
  ENDIF

  ;; Read grain cross section tables.
  IF (N_Elements(C_Table) EQ 0) THEN BEGIN
     C_Abs_Sil_Tab = Jam_ReadArray(!Jam.Path.Tables + 'c_abs.sil.txt', $
                                   Header_Lines=10)
     C_Abs_Gra_Tab = Jam_ReadArray(!Jam.Path.Tables + 'c_abs.gra.txt', $
                                   Header_Lines=10)
     C_Ext_Sil_Tab = Jam_ReadArray(!Jam.Path.Tables + 'c_ext.sil.txt', $
                                   Header_Lines=10)
     C_Ext_Gra_Tab = Jam_ReadArray(!Jam.Path.Tables + 'c_ext.gra.txt', $
                                   Header_Lines=10)
     waveTab = Jam_Wave(NumWave=N_WAVE_TAB)
     aTab = Jam_GrainRadii(NumRadii=N_A_TAB)
     C_Table = { Wave:waveTab, a:aTab, $
                 Abs:{ Sil:C_Abs_Sil_Tab, Gra:C_Abs_Gra_Tab }, $
                 Ext:{ Sil:C_Ext_Sil_Tab, Gra:C_Ext_Gra_Tab }}
  ENDIF $
  ELSE BEGIN
     waveTab = C_Table.Wave
     aTab = C_Table.a
     C_Abs_Sil_Tab = C_Table.Abs.Sil
     C_Abs_Gra_Tab = C_Table.Abs.Gra
     C_Ext_Sil_Tab = C_Table.Ext.Sil
     C_Ext_Gra_Tab = C_Table.Ext.Gra
  ENDELSE

  ;; Create wavelength and grain radius vectors for table data.
  logWaveTab = ALog(waveTab)
  logaTab = ALog(aTab)

  ;; Create default wavelength and grain radius vectors.
  IF ((N_Elements(wave) EQ 0) OR (Keyword_Set(IR))) THEN wave = Jam_Wave(IR=IR)
  IF (N_Elements(a) EQ 0) THEN a = aTab

  ;; Check wavelength and grain radius vectors.
  IF (Keyword_Set(debug)) THEN BEGIN
     IF (~Jam_Check(wave, TName='NUM', $
                    Max_N_Dimensions=1, Min_Value=[0.D], Msg=msg)) THEN $
                       Message, Jam_Message('Wave', msg)
     IF (~Jam_Check(a, TName='NUM', $
                    Max_N_Dimensions=1, Min_Value=[0.D], Msg=msg)) THEN $
                       Message, Jam_Message('a', msg)
  ENDIF

  ;; Calculate logarithmic vectors.
  N_WAVE = N_Elements(wave)
  N_A = N_Elements(a)
  logWave = ALog(wave)
  loga = ALog(a)

  ;; Interpolate to find silicate and graphite cross sections at
  ;; input wavelengths and radii.
  log_C_Abs_Sil_Tab = ALog(C_Abs_Sil_Tab)
  log_C_Abs_Gra_Tab = ALog(C_Abs_Gra_Tab)
  log_C_Ext_Sil_Tab = ALog(C_Ext_Sil_Tab)
  log_C_Ext_Gra_Tab = ALog(C_Ext_Gra_Tab)
  IF (~Array_Equal(a, aTab)) THEN BEGIN
     _log_C_Abs_Sil = DblArr(N_A, N_WAVE_TAB, /NoZero)
     _log_C_Abs_Gra = DblArr(N_A, N_WAVE_TAB, /NoZero)
     _log_C_Ext_Sil = DblArr(N_A, N_WAVE_TAB, /NoZero)
     _log_C_Ext_Gra = DblArr(N_A, N_WAVE_TAB, /NoZero)
     _log_C_Abs_Sil_Tab = Transpose(log_C_Abs_Sil_Tab)
     _log_C_Abs_Gra_Tab = Transpose(log_C_Abs_Gra_Tab)
     _log_C_Ext_Sil_Tab = Transpose(log_C_Ext_Sil_Tab)
     _log_C_Ext_Gra_Tab = Transpose(log_C_Ext_Gra_Tab)
     FOR i=0, N_WAVE_TAB-1 DO BEGIN
        _log_C_Abs_Sil[*,i] = Interpol(_log_C_Abs_Sil_Tab[*,i], logaTab, loga)
        _log_C_Abs_Gra[*,i] = Interpol(_log_C_Abs_Gra_Tab[*,i], logaTab, loga)
        _log_C_Ext_Sil[*,i] = Interpol(_log_C_Ext_Sil_Tab[*,i], logaTab, loga)
        _log_C_Ext_Gra[*,i] = Interpol(_log_C_Ext_Gra_Tab[*,i], logaTab, loga)
     ENDFOR
     _log_C_Abs_Sil = Transpose(_log_C_Abs_Sil)
     _log_C_Abs_Gra = Transpose(_log_C_Abs_Gra)
     _log_C_Ext_Sil = Transpose(_log_C_Ext_Sil)
     _log_C_Ext_Gra = Transpose(_log_C_Ext_Gra)
  ENDIF $
  ELSE BEGIN
     _log_C_Abs_Sil = log_C_Abs_Sil_Tab
     _log_C_Abs_Gra = log_C_Abs_Gra_Tab
     _log_C_Ext_Sil = log_C_Ext_Sil_Tab
     _log_C_Ext_Gra = log_C_Ext_Gra_Tab
  ENDELSE
  IF (~Array_Equal(wave, waveTab)) THEN BEGIN
     log_C_Abs_Sil = DblArr(N_WAVE, N_A, /NoZero)
     log_C_Abs_Gra = DblArr(N_WAVE, N_A, /NoZero)
     log_C_Ext_Sil = DblArr(N_WAVE, N_A, /NoZero)
     log_C_Ext_Gra = DblArr(N_WAVE, N_A, /NoZero)
     FOR i=0, N_A-1 DO BEGIN
        log_C_Abs_Sil[*,i] = Spline(logWaveTab, _log_C_Abs_Sil[*,i], logWave)
        log_C_Abs_Gra[*,i] = Spline(logWaveTab, _log_C_Abs_Gra[*,i], logWave)
        log_C_Ext_Sil[*,i] = Spline(logWaveTab, _log_C_Ext_Sil[*,i], logWave)
        log_C_Ext_Gra[*,i] = Spline(logWaveTab, _log_C_Ext_Gra[*,i], logWave)
     ENDFOR
  ENDIF $
  ELSE BEGIN
     log_C_Abs_Sil = _log_C_Abs_Sil
     log_C_Abs_Gra = _log_C_Abs_Gra
     log_C_Ext_Sil = _log_C_Ext_Sil
     log_C_Ext_Gra = _log_C_Ext_Gra
  ENDELSE
  C_Abs_Sil = Exp(log_C_Abs_Sil)
  C_Abs_Gra = Exp(log_C_Abs_Gra)
  C_Ext_Sil = Exp(log_C_Ext_Sil)
  C_Ext_Gra = Exp(log_C_Ext_Gra)

  ;; Scale silicate opacities with scaling factor at each wavelength.
  IF (N_Elements(scaleSIL) NE 0) THEN BEGIN
     FOR i=0,N_A-1 DO BEGIN
        IF (N_Elements(scaleSIL.Wave) EQ N_Elements(wave)) THEN BEGIN
           C_Abs_Sil[*,i] *= scaleSIL.Scale
           C_Ext_Sil[*,i] *= scaleSIL.Scale
        ENDIF $
        ELSE BEGIN
           scale = Replicate(1.D, N_Elements(wave))
           idxW = Where((wave GT Min(scaleSIL.Wave)) AND $
                        (wave LT Max(scaleSIL.Wave)), cntW)
           scale[idxW] = Interpol(scaleSIL.Scale, scaleSIL.Wave, wave[idxW])
           C_Abs_Sil[*,i] *= scale
           C_Ext_Sil[*,i] *= scale
        ENDELSE
     ENDFOR
  ENDIF

  ;; Calculate PAH absorption cross sections.
  C_PAH = Jam_PAHCrossSection(wave, a, C_Abs_Gra, C_Cont=C_Cont, C_Feat=C_Feat)

  ;; Calculate carbonaceous grain weighting function.
  q_Gra = 0.01D
  a_Xi = 50.D ;; [A]
  xi_PAH = (1.D - q_Gra) * (1.D < ((a_Xi * 1D-4) / a)^3)
  xi_PAH = Rebin(Reform(xi_PAH, 1, N_A), N_WAVE, N_A)
  C_Abs_Gra_Carb = (1.D - xi_PAH) * C_Abs_Gra
  C_Ext_Gra_Carb = (1.D - xi_PAH) * C_Ext_Gra

  ;; Calculate carbonaceous cross sections.
  C_Abs_Neu_Cont = xi_PAH * C_Cont.Neu + C_Abs_Gra_Carb
  C_Abs_Ion_Cont = xi_PAH * C_Cont.Ion + C_Abs_Gra_Carb
  C_Ext_Neu_Cont = xi_PAH * C_Cont.Neu + C_Ext_Gra_Carb
  C_Ext_Ion_Cont = xi_PAH * C_Cont.Ion + C_Ext_Gra_Carb
  C_Neu_Feat = xi_PAH * C_Feat.Neu
  C_Ion_Feat = xi_PAH * C_Feat.Ion

  ;; Create final vectors.
  N_Tot = N_WAVE + N_A
  C_Abs_Sil = (N_Tot EQ 2) ? C_Abs_Sil[0] : Reform(C_Abs_Sil)
  C_Abs_Gra = (N_Tot EQ 2) ? C_Abs_Gra[0] : Reform(C_Abs_Gra)
  C_Ext_Sil = (N_Tot EQ 2) ? C_Ext_Sil[0] : Reform(C_Ext_Sil)
  C_Ext_Gra = (N_Tot EQ 2) ? C_Ext_Gra[0] : Reform(C_Ext_Gra)
  C_Abs_Neu_Cont = (N_Tot EQ 2) ? C_Abs_Neu_Cont[0] : Reform(C_Abs_Neu_Cont)
  C_Abs_Ion_Cont = (N_Tot EQ 2) ? C_Abs_Ion_Cont[0] : Reform(C_Abs_Ion_Cont)
  C_Ext_Neu_Cont = (N_Tot EQ 2) ? C_Ext_Neu_Cont[0] : Reform(C_Ext_Neu_Cont)
  C_Ext_Ion_Cont = (N_Tot EQ 2) ? C_Ext_Ion_Cont[0] : Reform(C_Ext_Ion_Cont)
  C_Neu_Feat = (N_Tot EQ 2) ? C_Neu_Feat[0] : Reform(C_Neu_Feat)
  C_Ion_Feat = (N_Tot EQ 2) ? C_Ion_Feat[0] : Reform(C_Ion_Feat)

  ;; Create C_Ext structure.
  C_Ext = { Wave:wave, $
            a:a, $
            Sil:C_Ext_Sil, $
            Gra:C_Ext_Gra, $
            Carb: { Neu: { Cont:C_Ext_Neu_Cont, $
                           Feat:C_Neu_Feat, $
                           Tot:C_Ext_Neu_Cont + C_Neu_Feat }, $
                    Ion: { Cont:C_Ext_Ion_Cont, $
                           Feat:C_Ion_Feat, $
                           Tot:C_Ext_Ion_Cont + C_Neu_Feat } } }

  IF (Keyword_Set(debug)) THEN !Except = except

  RETURN, { Wave:wave, $
            a:a, $
            Sil:C_Abs_Sil, $
            Gra:C_Abs_Gra, $
            Carb: { Neu: { Cont:C_Abs_Neu_Cont, $
                           Feat:C_Neu_Feat, $
                           Tot:C_Abs_Neu_Cont + C_Neu_Feat }, $
                    Ion: { Cont:C_Abs_Ion_Cont, $
                           Feat:C_Ion_Feat, $
                           Tot:C_Abs_Ion_Cont + C_Ion_Feat } } }

END
