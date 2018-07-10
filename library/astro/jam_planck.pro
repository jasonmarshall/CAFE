;+
; NAME:
; JAM_PLANCK
;
; PURPOSE:
; This function calculates the value of the Planck function as a function
; of wavelength and temperature for several choices of units.
;
; CATEGORY:
; Astronomy, Physics.
;
; CALLING SEQUENCE:
; Result = JAM_PLANCK(Wave, T)
;
; INPUTS:
; Wave: A scalar or vector of wavelengths in units of microns.
; T: A scalar or vector of temperatures in units of Kelvin.
;
; KEYWORD PARAMETERS:
; HZ: Set this keyword to output in units of ergs s-1 cm-2 Hz-1 sr-1.
; UM: Set this keyword to output in units of ergs s-1 cm-2 um-1 sr-1.
;
; OUTPUTS:
; This function returns the value of the Planck function for the specified
; wavelengths and temperatures in units of Jy sr^-1. If a single wavelength
; and temperature are input, the output will be a scalar. If a vector
; wavelength is input with a scalar temperature or vise versa, the output
; will be a vector. If both a vector wavelength and temperature are input,
; the output will have dimensions N_WAVE x N_TEMP.
;
; RESTRICTIONS:
; JamLib - JAM_SYNTAX, JAM_MESSAGE, JAM_CHECK
;
; NOTES:
; [1] 1 Jy = 1D-23 ergs s-1 cm-2 Hz-1.
; [2] B_Hz ~ ((wave/um)^2 / 3D14) * B_um
;
; EXAMPLE:
; To calculate the 300 K Planck function in Jy sr-1 at 10 microns enter:
;   IDL> Result = JAM_PLANCK(10.D, 300.D)
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, May 2004.
;-

;;+
; ===================================================================
;;-

FUNCTION Jam_Planck, wave, T, Hz=Hz, um=um, Debug=debug

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Setup debugging.
  IF (Keyword_Set(debug)) THEN BEGIN
    except = !Except
    !Except = 2
  ENDIF

  ;; Check input syntax.
  IF (N_Params() LT 2) THEN $
    Message, Jam_Message('Syntax', Jam_Syntax('Jam_Planck', /Is_Function))

  ;; Check input parameters.
  IF (Keyword_Set(debug)) THEN BEGIN
    IF (~Jam_Check(wave, TName='NUM', Max_N_Dimensions=1, Msg=msg)) THEN $
      Message, Jam_Message('Wave', msg)
    IF (~Jam_Check(T, TName='NUM', Max_N_Dimensions=1, Msg=msg)) THEN $
      Message, Jam_Message('T', msg)
    IF ((Keyword_Set(Hz)) AND (Keyword_Set(um))) THEN $
      Message, Jam_Message('Hz/um', 'May not be set simultaneously.')
  ENDIF

  ;; Number of temperatures and wavelengths.
  N_WAVE = N_Elements(wave)
  N_T = N_Elements(T)

  ;; Define constants.
  c1 = 3.972895D19 ;; = 2 * h * c = [Jy sr^-1 um^3]
  c2 = 1.4387731D4 ;; = h * c / k = [K um]

  ;; Convert to units of ergs s^-1 cm^-2 Hz^-1 sr^-1.
  IF (Keyword_Set(Hz)) THEN BEGIN
    c_Hz = 3.972895D-4 ;; = 1D-23 * c1
    arg1 = c_Hz / wave^3
  ENDIF

  ;; Convert to units of ergs s^-1 cm^-2 um^-1 sr^-1.
  IF (Keyword_Set(um)) THEN BEGIN
    c_um = 1.1918685D11 ;; = 1D-23 * 3D14 * c1
    arg1 = c_um / wave^5
  ENDIF

  ;; Calculate planck function.
  IF (N_Elements(arg1) EQ 0) THEN arg1 = c1 / wave^3
  arg2 = c2 / wave
  B = DblArr(N_WAVE, N_T, /NoZero)
  FOR i=0,N_T-1 DO B[0,i] = arg1 / (Exp((arg2 / T[i]) < !Jam.LogMax) - 1.D)

  IF (Keyword_Set(debug)) THEN !Except = except

  RETURN, (N_Elements(B) EQ 1) ? B[0] : B

END
