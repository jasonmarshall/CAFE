;+ ====================================================================
; NAME:
; Jam_GrainCrossSection_Tab
;
; PURPOSE:
; This function tabulates absorption cross sections of spherical
; astronomical silicate and graphite dust grains as a function of
; grain radius and wavelength.
;
; CATEGORY:
; Astronomy, Physics.
;
; CALLING SEQUENCE:
; Jam_GrainCrossSection_Tab
;
; OUTPUTS:
; This function creates text files for silicate, graphite, and PAH
; cross sections in the !Jam.Path.Tables directory. These files are
; read with the Jam_GrainCrossSection function.
;
; REQUIREMENTS:
; JamLib - [Jam_Syntax, Jam_Message]
; JamLib - Jam_ReadArray, Jam_WriteData, Jam_Dist
;
; PROCEDURE:
; Absorption efficiencies, Q_Abs, of silicate and graphite grains
; are calculated according to the methods described in Laor and
; Draine (1993) (see http://www.astro.princeton.edu/~draine/dust/)
; and C_Abs(Wave,a) = Pi * a^2 * Q_Abs(Wave,a).
;
; REFERENCES:
; Laor & Draine, ApJ, 402, 1993.
;
; EXAMPLE:
; To tabulate absorption cross section of astronomical dust enter:
;   IDL> Jam_GrainCrossSection_Tab
;
; AUTHOR:
; Jason A. Marshall
; Department of Astronomy
; Cornell University
; Ithaca, NY 14853
; jam258@cornell.edu
;
; MODIFICATION HISTORY:
; 01 Oct 05 - JAM - Initial version.
;-
;; ....................................................................
; Copyright (C) 2005, 2006-, Jason A. Marshall
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or un-
; modified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;; ....................................................................

;;+ ===================================================================
; NAME:
; Jam_GrainCrossSection_Tab_Dust
;
; PURPOSE:
; This function calculates graphite and silicate efficiencies.
;
; MODIFICATION HISTORY:
; 01 Oct 05 - JAM - Initial version.
;;-

FUNCTION Jam_GrainCrossSection_Tab_Dust, type, wave, a

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get the file name and check the file.
  CASE (StrUpCase(type)) OF
    'GRA': path = !Jam.Path.Tables + 'draine_q.gra.txt'
    'SIL': path = !Jam.Path.Tables + 'draine_q.sil.txt'
  ENDCASE

  ;; Calculate wave and grain-size vectors.
  IF ((N_Elements(wave) EQ 0) OR (N_Elements(a) EQ 0)) THEN BEGIN

    ;; Get grain radii and wavelength information from file headers.
    aInfo = DblArr(3, /NoZero)
    wInfo = DblArr(3, /NoZero)
    OpenR, LUN, path, /Get_LUN
    Skip_LUN, LUN, 3, /Lines
    ReadF, LUN, aInfo
    ReadF, LUN, wInfo
    Free_LUN, LUN

    ;; Create vector of grain radii.
    N_A = Fix(aInfo[0])
    MIN_A = Double(aInfo[1])
    MAX_A = Double(aInfo[2])
    a = Jam_Dist(N_A, MIN_A, MAX_A)

    ;; Number of wavelengths.
    N_WAVE = Fix(wInfo[0])
    MAX_WAVE = Double(wInfo[1])
    MIN_WAVE = Double(wInfo[2])
    wave = Jam_Dist(N_WAVE, MIN_WAVE, MAX_WAVE)

  ENDIF $
  ELSE BEGIN
    N_A = N_Elements(a)
    N_Wave = N_Elements(wave)
  ENDELSE

  ;; Read table into N_A x N_WAVE x N_COLUMN array.
  data = Jam_ReadArray(path, /Block_First, N_Block=N_A, N_Row=N_WAVE, $
    Header_Lines=8, Break_Lines=3)
  Q_Abs = Reverse(Transpose(Reform(data[*,*,1])), 1) ;; [N_WAVE x N_A]
  Q_Sca = Reverse(Transpose(Reform(data[*,*,2])), 1) ;; [N_WAVE x N_A]
  Q_Ext = Q_Abs + Q_Sca

  RETURN, { Abs:Q_Abs, Ext:Q_Ext }

END

;; ====================================================================

PRO Jam_GrainCrossSection_Tab, Debug=debug

  Compile_Opt IDL2
  On_Error, 2

  ;; Setup debugging.
  IF (Keyword_Set(debug)) THEN BEGIN
    except = !Except
    !Except = 2
  ENDIF

  ;; Obtain silicate and graphite grain efficiencies.
  _Q_Sil = Jam_GrainCrossSection_Tab_Dust('SIL', _wave, _a)
  _Q_Gra = Jam_GrainCrossSection_Tab_Dust('GRA', _wave, _a)
  _N_WAVE = N_Elements(_wave)
  _N_A = N_Elements(_a)

  ;; Calculate silicate and graphite absorption cross sections.
  QtoC = Rebin(Reform(!DPi * (1D-4 * _a)^2, 1, _N_A), _N_WAVE, _N_A)
  _C_Abs_Sil = QtoC * _Q_Sil.Abs
  _C_Ext_Sil = QtoC * _Q_Sil.Ext
  _C_Abs_Gra = QtoC * _Q_Gra.Abs
  _C_Ext_Gra = QtoC * _Q_Gra.Ext

  ;; Create final grain radius and wavelength vectors.
  a = Jam_GrainRadii(NumRadii=N_A)
  wave = Jam_Wave(NumWave=N_WAVE)

  ;; Interpolate to find cross sections at final wavelengths and radii.
  _loga = ALog(_a)
  loga = ALog(a)
  _log_C_Abs_Sil = Transpose(ALog(_C_Abs_Sil)) ;; [_N_A x _N_Wave]
  _log_C_Ext_Sil = Transpose(ALog(_C_Ext_Sil))
  _log_C_Abs_Gra = Transpose(ALog(_C_Abs_Gra))
  _log_C_Ext_Gra = Transpose(ALog(_C_Ext_Gra))
  log_C_Abs_Sil = DblArr(N_A, _N_WAVE)
  log_C_Ext_Sil = DblArr(N_A, _N_WAVE)
  log_C_Abs_Gra = DblArr(N_A, _N_WAVE)
  log_C_Ext_Gra = DblArr(N_A, _N_WAVE)
  FOR i=0,_N_WAVE-1 DO BEGIN
     log_C_Abs_Sil[*,i] = Interpol(_log_C_Abs_Sil[*,i], _loga, loga)
     log_C_Ext_Sil[*,i] = Interpol(_log_C_Ext_Sil[*,i], _loga, loga)
     log_C_Abs_Gra[*,i] = Interpol(_log_C_Abs_Gra[*,i], _loga, loga)
     log_C_Ext_Gra[*,i] = Interpol(_log_C_Ext_Gra[*,i], _loga, loga)
  ENDFOR
  log_C_Abs_Sil = Transpose(log_C_Abs_Sil) ;; [_N_Wave x N_A]
  log_C_Ext_Sil = Transpose(log_C_Ext_Sil)
  log_C_Abs_Gra = Transpose(log_C_Abs_Gra)
  log_C_Ext_Gra = Transpose(log_C_Ext_Gra)
  IF (~Array_Equal(wave, _wave)) THEN BEGIN
     _log_C_Abs_Sil = DblArr(N_WAVE, N_A, /NoZero)
     _log_C_Ext_Sil = DblArr(N_WAVE, N_A, /NoZero)
     _log_C_Abs_Gra = DblArr(N_WAVE, N_A, /NoZero)
     _log_C_Ext_Gra = DblArr(N_WAVE, N_A, /NoZero)
     _logWave = ALog(_wave)
     logWave = ALog(wave)
     FOR i=0, N_A-1 DO BEGIN
        _log_C_Abs_Sil[*,i] = Spline(_logWave, log_C_Abs_Sil[*,i], logWave)
        _log_C_Ext_Sil[*,i] = Spline(_logWave, log_C_Ext_Sil[*,i], logWave)
        _log_C_Abs_Gra[*,i] = Spline(_logWave, log_C_Abs_Gra[*,i], logWave)
        _log_C_Ext_Gra[*,i] = Spline(_logWave, log_C_Ext_Gra[*,i], logWave)
     ENDFOR
  ENDIF $
  ELSE BEGIN
     _log_C_Abs_Sil = log_C_Abs_Sil
     _log_C_Ext_Sil = log_C_Ext_Sil
     _log_C_Abs_Gra = log_C_Abs_Gra
     _log_C_Ext_Gra = log_C_Ext_Gra
  ENDELSE
  C_Abs_Sil = Exp(_log_C_Abs_Sil) ;; [N_Wave x N_A]
  C_Ext_Sil = Exp(_log_C_Ext_Sil)
  C_Abs_Gra = Exp(_log_C_Abs_Gra)
  C_Ext_Gra = Exp(_log_C_Ext_Gra)

  ;; Create output filenames.
  pathAbsSil = !Jam.Path.Tables + 'c_abs.sil.txt'
  pathAbsGra = !Jam.Path.Tables + 'c_abs.gra.txt'
  pathExtSil = !Jam.Path.Tables + 'c_ext.sil.txt'
  pathExtGra = !Jam.Path.Tables + 'c_ext.gra.txt'

  ;; Create output data structure headers.
  data = { $
    DATE:SysTime(), $
    N_WAVE:N_Elements(wave), $
    MIN_WAVE:Min(wave), $
    MAX_WAVE:Max(wave), $
    N_A:N_Elements(a), $
    MIN_A:Min(a), $
    MAX_A:Max(a)  $
  }

  ;; Write data files.
  Jam_WriteArray, C_Abs_Sil, FileName=pathAbsSil, Header=data
  Jam_WriteArray, C_Ext_Sil, FileName=pathExtSil, Header=data
  Jam_WriteArray, C_Abs_Gra, FileName=pathAbsGra, Header=data
  Jam_WriteArray, C_Ext_Gra, FileName=pathExtGra, Header=data

  IF (Keyword_Set(debug)) THEN !Except = except

END
