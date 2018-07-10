;+ ====================================================================
; NAME:
;   JAM_WRITEDATA
;
; PURPOSE:
;
;   This procedure writes an ASCII text file containing data from a
;   structure formatted according to the rules described in the
;   PROCEDURE section below and returned by the routine JAM_READDATA.
;
; CATEGORY:
;
;   General Programming, I/O.
;
; CALLING SEQUENCE:
;
;   JAM_WRITEDATA, Data
;
; INPUTS:
;
;   Data: Structure of data in the format described in the PROCEDURE
;     section and returned by JAM_READDATA.
;
; KEYWORD PARAMETERS:
;
;   FILENAME: Set to a scalar string containing the full path name
;     of the output file. The default is to output the data into a
;     file named 'data.txt' within the current working directory.
;
;   FORMAT_SCL: 
;
;   FORMAT_VEC: Set to an array of strings, one for each vector of
;     data to output, containing format statements (e.g., 'A10', 'I5',
;     and 'E8.2' for a width 10 string, width 5 integer, and width
;     8 exponential notation float with two digits after the decimal,
;     respectively).
;
;   FORMAT_FIX: Set to a scalar string containing the format statement
;     for fixed point numbers. The default is 'I15'.
;
;   FORMAT_FLT: Set to a scalar string containing the format statement
;     for floating point numbers. The default is 'E15.7'.
;
; OUTPUTS:
;
;   The procedure outputs an ASCII text file containing the data.
;
; PROCEDURE:
;
;   The input structure must be of the following form:
;
;     { Data }.
;             |- Scalar_01
;             |- ...
;             |- Scalar_Ns
;             |- Vector_01
;             |- ...
;             |- Vector_Nv
;
;   where Ns and Nv are the number of scalar and vector fields in
;   the data structure. The scalar and vector fields may have any
;   string or numeric type.
;
;   Scalar fields are written in the ASCII file as 'header' values
;   while vector fields are written out as columns (see the function
;   JAM_READDATA for a description of the file format). The FORMAT_VEC
;   keyword should contain Nv string format statements if given.
;
; REQUIREMENTS:
;
;   <<JamLib>> - JAM_SYNTAX, JAM_MESSAGE, JAM_CHECK, JAM_PADSTRING
;
; EXAMPLE:
;
;   To write data to a disk file named 'data.txt' enter:
;
;     IDL> data = { Alphabet:['a','b','c'] }
;     IDL> JAM_WRITEDATA, data
;
; AUTHOR:
;
;   Jason A. Marshall
;   Department of Astronomy
;   Cornell University
;   Ithaca, NY 14853
;   jam258@cornell.edu
;
; MODIFICATION HISTORY:
;
;   Written by: Jason Marshall, Aug 2004.
;   2005 Dec 04 - JAM - Added FORMAT and FORMAT_xxx keywords.
;-
;; ....................................................................
; Copyright (C) 2005, Jason A. Marshall
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or un-
; modified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;; ....................................................................

PRO Jam_WriteData, data, $
                   FileName=fileName, $
                   Format_Vec=formatVec, $
                   Format_Scl=formatScl, $
                   Format_Fix=formatFix, $
                   Format_Flt=formatFlt, $
                   Help=help, $
                   Debug=debug

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Print help info.
  IF (Keyword_Set(help)) THEN BEGIN
     Print, Jam_Syntax('Jam_WriteData', Debug=debug)
     RETURN
  ENDIF

  ;; Check input.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', $
                          Jam_Syntax('Jam_WriteData', Debug=debug))
  IF (~Jam_Check(data, TName='STRUCT', N_Dimensions=1, $
                 N_Elements=1, Msg=msg, Debug=debug)) THEN $
                    Message, Jam_Message('Data', msg, Debug=debug)

  ;; Set default file name.
  IF (N_Elements(fileName) EQ 0) THEN fileName = 'data.txt'
  IF (~Jam_Check(fileName, $
                 TName='STRING', N_Elements=1, Msg=msg, Debug=debug)) THEN $
                    Message, Jam_Message(fileName, msg, Debug=debug)

  ;; Separate scalar and vector fields.
  N_FIELDS = N_Tags(data)
  fields = Tag_Names(data)
  FOR i=0,N_FIELDS-1 DO BEGIN
     CASE (N_Elements(data.(i))) OF
        1: BEGIN
           IF (Size(data.(i), /N_Dimensions) EQ 0) THEN BEGIN
              scalar = (N_Elements(scalar) EQ 0) ? $
                       Create_Struct(fields[i], data.(i)) : $
                       Create_Struct(scalar, fields[i], data.(i))
           ENDIF $
           ELSE BEGIN
              vector = (N_Elements(vector) EQ 0) ? $
                       Create_Struct(fields[i], data.(i)) : $
                       Create_Struct(vector, fields[i], data.(i))
           ENDELSE
        END
        ELSE: BEGIN
           vector = (N_Elements(vector) EQ 0) ? $
                    Create_Struct(fields[i], data.(i)) : $
                    Create_Struct(vector, fields[i], data.(i))
        END
     ENDCASE
  ENDFOR
  N_SCALAR = N_Tags(scalar)
  N_VECTOR = N_Tags(vector)

  ;; Calculate string format statments.
  formatStrLength = 5
  IF (N_VECTOR GT 0) THEN BEGIN
     formatStr = StrArr(N_VECTOR)
     FOR i=0,N_VECTOR-1 DO BEGIN
        IF (Size(vector.(i), /TName) EQ 'STRING') THEN $
           formatStr[i] = formatStrLength > Max(StrLen(vector.(i)))
     ENDFOR
     formatStr = StrCompress('A' + String(formatStr + 2), /Remove_All)
  ENDIF

  ;; Define format statements.
  IF (N_Elements(formatVec) EQ 0) THEN $
     formatVec = Replicate('', (N_VECTOR > 1))
  IF (N_Elements(formatFix) EQ 0) THEN formatFix = 'I15'
  IF (N_Elements(formatFlt) EQ 0) THEN formatFlt = 'E15.7'
  IF (~Jam_Check(formatVec, TName='STRING', $
                 N_Elements=(N_VECTOR > 1), Msg=msg, Debug=debug)) THEN $
                    Message, Jam_Message('Format_Vec', msg)
  IF (~Jam_Check(formatFix, $
                 TName='STRING', N_Elements=1, Msg=msg, Debug=debug)) THEN $
                    Message, Jam_Message('Format_Fix', msg)
  IF (~Jam_Check(formatFlt, $
                 TName='STRING', N_Elements=1, Msg=msg, Debug=debug)) THEN $
                    Message, Jam_Message('Format_Flt', msg)

  ;; Open output file.
  width = (N_VECTOR GT 0) ? 20 * N_VECTOR : 80
  OpenW, LUN, fileName, /Get_LUN, Width=width

  ;; Create scalar field header.
  IF (N_SCALAR GT 0) THEN BEGIN
     PrintF, LUN, ';+'
     names = Tag_Names(scalar)
     names = Jam_PadString(names, Max(StrLen(names)), Debug=debug)
     FOR i=0,N_SCALAR-1 DO BEGIN
        IF (N_Elements(formatScl) EQ 0) THEN BEGIN
           thisValue = (Size(scalar.(i), /TName) EQ 'STRING') ? $
                       "'" + scalar.(i) + "'" : String(scalar.(i))
        ENDIF $
        ELSE BEGIN
           thisFormat = '(' + formatScl[i] + ')'
           thisValue = (Size(scalar.(i), /TName) EQ 'STRING') ? $
                       "'" + scalar.(i) + "'" : $
                       String(scalar.(i), Format=thisFormat)
        ENDELSE
        PrintF, LUN, ';> ' + names[i] + ' = ' + thisValue
     ENDFOR
     PrintF, LUN, ';-'
     PrintF, LUN, ''
  ENDIF

  ;; Write vector data.
  IF (N_VECTOR GT 0) THEN BEGIN

     ;; Ensure that all vectors have the same length.
     length = N_Elements(vector.(0))
     FOR i=1,N_VECTOR-1 DO $
        IF (N_Elements(vector.(i)) NE length) THEN $
           Message, Jam_Message('Data', 'Contains non-uniform fields.')

     ;; Obtain field names and their string lengths.
     names = Tag_Names(vector)
     nameLengths = StrLen(names)

     ;; Gather format statements and field widths.
     formats = formatVec
     widths = LonArr(N_VECTOR)
     FOR i=0,N_VECTOR-1 DO BEGIN
        type = Size(vector.(i), /TName)
        CASE (type) OF
           'STRING': IF (formats[i] EQ '') THEN formats[i] = formatStr[i]
           'LONG': IF (formats[i] EQ '') THEN formats[i] = formatFix
           'INT': IF (formats[i] EQ '') THEN formats[i] = formatFix
           'BYTE': IF (formats[i] EQ '') THEN formats[i] = formatFix
           'FLOAT': IF (formats[i] EQ '') THEN formats[i] = formatFlt
           'DOUBLE': IF (formats[i] EQ '') THEN formats[i] = formatFlt
        ENDCASE
        thisFormat = '(' + formats[i] + ')'
        widths[i] = StrLen(String('', Format=thisFormat))
     ENDFOR
     widths = widths > nameLengths

     ;; Write the field names.
     thisFormat = '(A3, $)'
     PrintF, LUN, ';~ ', Format=thisFormat
     FOR i=0,N_VECTOR-1 DO BEGIN
        IF (i EQ (N_VECTOR-1)) THEN BEGIN
           thisFormat = '(A' + String(widths[i]) + ')'
           PrintF, LUN, names[i], Format=thisFormat
        ENDIF $
        ELSE BEGIN
           thisFormat = '(A' + String(widths[i]) + ', $, A2, $)'
           PrintF, LUN, names[i], '', Format=thisFormat
        ENDELSE
     ENDFOR

     ;; Write the field data.
     FOR i=0,length-1 DO BEGIN
        PrintF, LUN, '', Format='(A3, $)'
        FOR j=0,N_VECTOR-1 DO BEGIN
           thisVector = (Size(vector.(j), /TName) EQ 'STRING') ? $
                        "'" + (vector.(j))[i] + "'" : (vector.(j))[i]
           IF (j EQ (N_VECTOR-1)) THEN BEGIN
              thisFormat = '(' + formats[j] + ')'
              PrintF, LUN, thisVector, Format=thisFormat
           ENDIF $
           ELSE BEGIN
              thisFormat = '(' + formats[j] + ', $, A2, $)'
              PrintF, LUN, thisVector, '', Format=thisFormat
           ENDELSE
        ENDFOR
     ENDFOR

  ENDIF

  ;; Close the file.
  Free_LUN, LUN

  RETURN

END
