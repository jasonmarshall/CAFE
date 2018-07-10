;+
; NAME:
; JAM_PADSTRING
;
; PURPOSE:
; This function pads a string with blanks to make it a specified length.
; If the length of the input string is greater than the requested length,
; the input string is quietly returned.
;
; CATEGORY:
; General Programming.
;
; CALLING SEQUENCE:
; Result = JAM_PADSTRING(Input, Length)
;
; INPUTS:
; Input: A scalar or array of input strings to be padded.
; Length: A scalar or array of integers containing the length of the
;   output strings.
;
; KEYWORD PARAMETERS:
; BEFORE: Set this keyword to pad at the beginning of the strings.
;   The default is to pad the ends.
; PAD_CHAR: Set this keyword equal to a string of length one containing
;   the pad character. The default is a space.
;
; OUTPUTS:
; This function returns Input padded to the specified length.
;
; EXAMPLE:
; To pad the end of a string with blanck characters enter:
;   IDL> Result = JAM_PADSTRING('Hello', 10)
; To pad the beginning of a string with asterisks enter:
;   IDL> Result = JAM_PADSTRING('Hello', 10, /BEFORE, PAD_CHAR='*')
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, 11 June 2004.
;-

;/+
FUNCTION Jam_PadString, input, length, Before=before, $
  Pad_Char=pad_char, Debug=debug

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 2) THEN $
    Message, Jam_Message('Syntax', Jam_Syntax('Jam_PadString', /Is_Function))
  IF (~Jam_Check(input, TName='STRING', Msg=msg)) THEN $
    Message, Jam_Message('Input', msg)
  IF (~Jam_Check(length, TName='FIX', Max_N_Dimensions=1, Msg=msg)) THEN $
    Message, Jam_Message('Length', msg)
  IF (N_Elements(pad_char) NE 0) THEN BEGIN
    IF (~Jam_Check(pad_char, TName='STRING', N_Dimensions=0, Msg=msg)) THEN $
      Message, Jam_Message('PAD_CHAR', msg)
    IF (StrLen(pad_char) NE 1) THEN $
      Message, Jam_Message('PAD_CHAR', 'Must be a single character.')
  ENDIF

  ;; Calculate the number of blanks to pad with.
  nBlanks = (length - StrLen(input)) > 0

  ;; Return padded input string.
  padChar = (N_Elements(pad_char) EQ 0) ? ' ' : pad_char
  nInput = N_Elements(input)
  padLine = StrArr(nInput)
  FOR i=0,nInput-1 DO $
    padLine[i] = (nBlanks[i] EQ 0) ? $
      '' : StrJoin(Replicate(padChar, nBlanks[i]))
  
  str = (Keyword_Set(before)) ? padLine + input : input + padLine

  RETURN, (N_Elements(str) EQ 1) ? str[0] : Reform(str)

END
;\-
