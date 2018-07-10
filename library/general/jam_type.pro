;+
; NAME:
; JAM_TYPE
;
; PURPOSE:
; This function returns the type code corresponding to a given type name.
;
; CATEGORY:
; General Programming.
;
; CALLING SEQUENCE:
; Result = JAM_TYPE(TName)
;
; INPUTS:
; TName: A scalar string or string vector containing the type names for
;   which the type codes are desired.
;
; OUTPUTS:
; This function returns a scalar or vector of long integer containing the
; type codes corresponding to the given type names.
;
; EXAMPLE:
; To print the type code of a given type enter:
;   IDL> Print, JAM_TYPE('FLOAT')
;
; MODIFICATION HISTORY:
;   Written by:  Jason Marshall, 23 May 2004.
;-

;/+
FUNCTION Jam_Type, tName

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 1) THEN $
    Message, Jam_Message('Syntax', Jam_Syntax('Jam_Type', /Is_Function))
  IF (~Jam_Check(tName, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
    Message, Jam_Message('TName', msg)

  ;; Return type code.
  FOR i=0,N_Elements(tName)-1 DO BEGIN
    CASE (StrUpCase(tName[i])) OF
      'UNDEFINED': type = (N_Elements(type) EQ 0) ? 0 : [type, 0]
           'BYTE': type = (N_Elements(type) EQ 0) ? 1 : [type, 1]
            'INT': type = (N_Elements(type) EQ 0) ? 2 : [type, 2]
           'LONG': type = (N_Elements(type) EQ 0) ? 3 : [type, 3]
          'FLOAT': type = (N_Elements(type) EQ 0) ? 4 : [type, 4]
         'DOUBLE': type = (N_Elements(type) EQ 0) ? 5 : [type, 5]
        'COMPLEX': type = (N_Elements(type) EQ 0) ? 6 : [type, 6]
         'STRING': type = (N_Elements(type) EQ 0) ? 7 : [type, 7]
         'STRUCT': type = (N_Elements(type) EQ 0) ? 8 : [type, 8]
       'DCOMPLEX': type = (N_Elements(type) EQ 0) ? 9 : [type, 9]
        'POINTER': type = (N_Elements(type) EQ 0) ? 10 : [type, 10]
         'OBJREF': type = (N_Elements(type) EQ 0) ? 11 : [type, 11]
           'UINT': type = (N_Elements(type) EQ 0) ? 12 : [type, 12]
          'ULONG': type = (N_Elements(type) EQ 0) ? 13 : [type, 13]
         'LONG64': type = (N_Elements(type) EQ 0) ? 14 : [type, 14]
        'ULONG64': type = (N_Elements(type) EQ 0) ? 15 : [type, 15]
             ELSE: Message, 'Incorrect type name given.'
    ENDCASE
  ENDFOR

  RETURN, type

END
;\-