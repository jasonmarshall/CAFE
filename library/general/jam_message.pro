;+
; NAME:
; JAM_MESSAGE
;
; PURPOSE:
; This function formats an error message string. It provides a simple and
; consistent way of changing the user issued error message syntax.
;
; CATEGORY:
; General Programming.
;
; CALLING SEQUENCE:
; Result = JAM_MESSAGE(Name, Msg)
;
; INPUTS:
; Name: A scalar string containing the name of the error causing variable.
; Msg: A scalar string containing the error message text.
;
; OUTPUTS:
; This function returns a scalar string containing the formatted error message.
; The error string may then be used with the IDL routine MESSAGE to throw an
; exception and output the message.
;
; EXAMPLE:
; To create the error message string enter:
;   IDL> Result = JAM_MESSAGE('Variable', 'Error!')
; To throw an exception and issue the formatted error message enter:
;   IDL> Message, Result
;
; MODIFICATION HISTORY:
;   Written by:  Jason Marshall, May 2004.
;-

;/+
FUNCTION Jam_Message, name, msg, Debug=debug

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 2) THEN $
    Message, '<Syntax> ' + Jam_Syntax('Jam_Message', /Is_Function)
  IF (~Jam_Check(name, TName='STRING', N_Dimensions=0, Msg=error)) THEN $
    Message, '<Name> ' + error
  IF (~Jam_Check(msg, TName='STRING', N_Dimensions=0, Msg=error)) THEN $
    Message, '<Msg> ' + error

  ;; Format and return error message.
  RETURN, StrCompress('<' + name + '> ' + msg)

END
;\-