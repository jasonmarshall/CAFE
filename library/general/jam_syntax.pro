;+
; NAME:
; JAM_SYNTAX
;
; PURPOSE:
; This function returns a string containing the calling syntax of an IDL
; routine (including all positional and keyword parameters).
;
; CATEGORY:
; General Programming.
;
; CALLING SEQUENCE:
; Result = JAM_SYNTAX(Routine)
;
; INPUTS:
; Routine: Scalar string containing the name of the IDL routine for which
;   the syntax is desired.
;
; KEYWORD PARAMETERS:
; IS_FUNCTION: Set this keyword if Routine is an IDL function rather than
;   a procedure.
;
; OUTPUTS:
; This function returns a scalar string containing the calling syntax of
; the input routine.
;
; PROCEDURE:
; This function uses the IDL function ROUTINE_INFO to obtain the names of
; positional and keyword parameters.
;
; EXAMPLE:
; To print the calling syntax of a procedure enter:
;   IDL> Print, JAM_SYNTAX('Jam_Help')
; To print the calling syntax of this function enter:
;   IDL> Print, JAM_SYNTAX('Jam_Syntax', /IS_FUNCTION)
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, July 2004.
;-

;/+
FUNCTION Jam_Syntax, routine, Is_Function=is_function, Debug=debug

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 1) THEN $
    Message, Jam_Message('Syntax', Jam_Syntax('Jam_Syntax', /Is_Function))

  ;; Check that Routine is a readable file.
  IF (~Jam_Check(routine, TName='STRING', N_Dimensions=0, Msg=msg)) THEN $
    Message, Jam_Message('Routine', msg)
  file = File_Which(routine + '.pro')
  IF (~File_Test(file, /Read, /Regular)) THEN $
    Message, Jam_Message(routine, 'Not a readable file.')

  ;; Compile Routine and get parameter information.
  IF (Keyword_Set(is_function)) THEN BEGIN
    Resolve_Routine, routine, /Is_Function, /No_Recompile
    info = Routine_Info(routine, /Functions, /Parameters)
  ENDIF $
  ELSE BEGIN
    Resolve_Routine, routine, /No_Recompile
    info = Routine_Info(routine, /Parameters)
  ENDELSE

  ;; Create syntax string.
  IF (Keyword_Set(is_function)) THEN BEGIN
    syntax = 'Result = ' + routine + '('
    FOR i=0,info.Num_Args-1 DO syntax += info.Args[i] + ', '
    FOR i=0,info.Num_KW_Args-1 DO syntax += info.KW_Args[i] + '=, '
    syntax = StrMid(syntax, 0, StrLen(syntax)-2) + ')'
  ENDIF $
  ELSE BEGIN
    syntax = routine
    IF (info.Num_Args + info.Num_KW_Args GT 0) THEN syntax += ', '
    FOR i=0,info.Num_Args-1 DO syntax += info.Args[i] + ', '
    FOR i=0,info.Num_KW_Args-1 DO syntax += info.KW_Args[i] + '=, '
    syntax = StrMid(syntax, 0, StrLen(syntax)-2)
  ENDELSE

  RETURN, syntax

END
;\-