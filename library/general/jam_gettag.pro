;+
; ===================================================================
; NAME:
; JAM_GETTAG
;
; PURPOSE:
; This function is used to extract the value of a tag from a structure
; given the tag name.
;
; CALLING SEQUENCE:
; Result = JAM_GETTAG(Struct, TagName)
;
; INPUTS:
; Struct: The structure from which to extract tags.
; TagName: The names of the tags to extract from the structure.
;
; OUTPUT:
; The function returns a structure if more than one tag name is given or
; a scalar if a single tag name is given.
;
; EXAMPLE:
; To obtain a single tag from a structure enter:
;   IDL> Result = Jam_GetTag({ a:1, b:2, c:3 }, 'a')
; To obtain multiple tags from a structure enter:
;   IDL> Result = Jam_GetTag({ a:1, b:2, c:3 }, ['a', 'c'])
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, December 27, 2004.
;-

FUNCTION Jam_GetTag, struct, tagName

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Check syntax.
  IF (N_Params() NE 2) THEN $
    Message, Jam_Message('Syntax', Jam_Syntax('Jam_GetTag', /Is_Function))

  ;; Check input parameters.
  IF (~Jam_Check(struct, TName='STRUCT', N_Elements=1, Msg=msg)) THEN $
    Message, Jam_Message('Struct', msg)
  IF (~Jam_Check(tagName, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
    Message, Jam_Message('TagName', msg)

  ;; Get structure tags.
  names = Tag_Names(struct)

  ;; Loop over each requested tag name.
  FOR i=0,N_Elements(tagName)-1 DO BEGIN

    ;; Get index of current tag name.
    index = Where(StrUpCase(tagName[i]) EQ names, count)

    ;; Error if tag isn't found.
    IF (count NE 1) THEN $
      Message, Jam_Message(tagName[i], 'Not found in structure.')

    ;; Create output structure.
    output = (N_Elements(output) EQ 0) ? $
      Create_Struct(tagName[i], struct.(index)) : $
      Create_Struct(output, tagName[i], struct.(index))

  ENDFOR

  ;; Don't return a structure if only a single tag name requested.
  IF (N_Tags(output) EQ 1) THEN output = output.(0)

  RETURN, output

END