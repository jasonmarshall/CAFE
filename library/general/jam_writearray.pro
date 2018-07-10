;+ ====================================================================
; NAME:
; Jam_WriteArray
;
; PURPOSE:
; This procedure writes an ASCII text file containing 2D array data.
;
; CATEGORY:
; Utility.
;
; CALLING SEQUENCE:
; Jam_WriteArray, array
;
; INPUTS:
; array: Array of 2-dimensional data to write to file.
;
; KEYWORD PARAMETERS:
; FileName: Set to a scalar string containing the full path name
;   of the output file. The default is to output the array into a
;   file named 'array.txt' within the current working directory.
; Format: Set to a string format statement.
; Header: Set to a header structure, in the format defined in
;   Jam_ReadData, containing fields to write to the file header.
;
; OUTPUTS:
; The procedure outputs an ASCII text file containing the array.
;
; REQUIREMENTS:
; JamLib - [Jam_Syntax, Jam_Message, Jam_Check]
; JamLib - Jam_PadString
;
; EXAMPLE:
; To write an array to a disk file named 'array.txt' enter:
;   IDL> array = [[1,2],[3,4]]
;   IDL> Jam_WriteArray, array
;
; AUTHOR:
; Jason A. Marshall
; Department of Astronomy
; Cornell University
; Ithaca, NY 14853
; jam258@cornell.edu
;
; MODIFICATION HISTORY:
; 15 Dec 2005 - JAM - Initial version.
;-
;; ....................................................................
; Copyright (C) 2005-, Jason A. Marshall
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or un-
; modified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;; ....................................................................

PRO Jam_WriteArray, $
  array, $
  FileName=fileName, $
  Format=format, $
  Header=header, $
  Debug=debug

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input.
  IF (N_Params() NE 1) THEN $
    Message, Jam_Message('Syntax', $
      Jam_Syntax('Jam_WriteArray', Debug=debug))
  IF (~Jam_Check(array, N_Dimensions=2, Msg=msg, Debug=debug)) THEN $
    Message, Jam_Message('array', msg, Debug=debug)

  ;; Set default file name.
  IF (N_Elements(fileName) EQ 0) THEN fileName = 'array.txt'
  IF (~Jam_Check(fileName, $
    TName='STRING', N_Elements=1, Msg=msg, Debug=debug)) THEN $
      Message, Jam_Message(fileName, msg, Debug=debug)

  ;; Open output file.
  dims = Size(array, /Dimensions)
  N_COL = dims[0]
  N_ROW = dims[1]
  width = 16 * N_COL
  OpenW, LUN, fileName, /Get_LUN, Width=width

  ;; Create scalar field header.
  N_HEADER = (N_Elements(header) EQ 0) ? 0L : N_Tags(header)
  IF (N_HEADER GT 0) THEN BEGIN
    PrintF, LUN, ';+'
    names = Tag_Names(header)
    names = Jam_PadString(names, Max(StrLen(names)), Debug=debug)
    FOR i=0,N_HEADER-1 DO BEGIN
      thisValue = (Size(header.(i), /TName) EQ 'STRING') ? $
        "'" + header.(i) + "'" : String(header.(i))
      PrintF, LUN, ';> ' + names[i] + ' = ' + thisValue
    ENDFOR
    PrintF, LUN, ';-'
    PrintF, LUN, ''
  ENDIF

  ;; Write array
  PrintF, LUN, Transpose(array), Format=format

  ;; Close the file.
  Free_LUN, LUN

  RETURN

END
