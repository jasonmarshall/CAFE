;+ ===========================================================================
; NAME:
; JAM_READDATA
;
; PURPOSE:
; This function reads a text file containing a 'Data' style text file. This
; file-type may contain both header data and column-formatted tabular data.
; The function returns a structure containing one element per header entry
; and column of data. Blank lines and those beginning with the IDL comment
; character are ignored.
;
; CATEGORY:
; General Programming, I/O.
;
; CALLING SEQUENCE:
; Result = JAM_READDATA(File)
;
; INPUTS:
; File: A scalar string containing the full path name of the 'Data' file.
;
; KEYWORD PARAMETERS:
; COMMENT: Set this keyword equal to a length one string containing the
;   character used to identify comment lines. The default character is ';'.
; IGNORE_COMMENTS: Ignores comments and thus includes commented-out lines of
;   array data in returned vectors.
; TNAME: Set this keyword equal to a scalar string or a string array containing
;   the type names for the columns of data in the text file. If a scalar string
;   is given, all columns are typed with a single type. If a string array is
;   given, each element of the array is used to type a column. Therefore,
;   the number of elements in this array must equal the number of columns in
;   the text file.  If this keyword is not set, the data type assigned to each
;   column is determined from the syntax used. If the column values are
;   enclosed within quotation marks, they are interpreted as type STRING. If
;   the values contain a decimal point or either of the exponentiation
;   characters 'e' or 'd', they are interpreted as type DOUBLE. Otherwise,
;   the value are interpreted as type LONG.
; LABELS: Set this keyword equal to a string array of labels to use for the
;   field names in the output structure.  The number of labels in this array
;   must equal the number of columns in the text file. If this keyword is not
;   given, the routine searches for the first commented line after the header.
;   If such a line exists and contains the same number of columns as there are
;   columns of data, the names of each column are taken from these entries. If
;   no commented line is found, columns are labeled 'Column[xx]' where 'xx'
;   indicates the column number.
; N_COLUMNS: Set this keyword equal to the number of columns to read in the
;   table. The default number of columns is taken from the number of columns
;   in the first line.
;
; OUTPUTS:
; This function returns an anonymous structure containing one element per
; header entry and column of data.
;
; EXAMPLE:
; The format of a data style file is as follows:
;
;   | ;+
;   | ;> HeaderName01 = HeaderValue01 | HeaderComment01
;   | ;>     ...      =     ...       |      ...
;   | ;> HeaderName0N = HeaderValue0N | HeaderComment0N
;   | ;-
;   | ;
;   | ;~ FirstColumn   SecondColumn   ThirdColumn
;   |    'A'           1.0            1
;   |    'B'           2.0            2
;   |    'C'           3.0            3
;
; To read this data file into a structure enter:
;
;   IDL> Result = JAM_READDATA('data.txt')
;
; The returned structure will contain N header fields and three fields of
; data. The first data field will be named 'FirstColumn' and will be an array
; with three STRING entries. The second will be named 'SecondColumn' and will
; be of type DOUBLE while the last will be named 'ThirdColumn' and will be of
; type LONG.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, May 2004.
; 2018 FEB 3 - JAM - Added IGNORE_COMMENTS keyword.
;-

FUNCTION Jam_ReadHeader, file, Data=data

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameter.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', Jam_Syntax('Jam_ReadHeader', /Is_Function))
  IF (~Jam_Check(file, /Is_File, Msg=msg)) THEN $
     Message, Jam_Message('File', msg)

  ;; Read entire file into text array.
  text = Jam_ReadText(file)
  data = text

  ;; Find the start of the header or return a null string if it doesn't exist.
  start = Min(Where(StrMid(text, 0, 2) EQ ';+'))
  IF (start EQ -1) THEN RETURN, ''

  ;; Find the end of the header or return a null string if it doesn't exist.
  finish = Min(Where(StrMid(text, 0, 2) EQ ';-'))
  IF (finish EQ -1) THEN RETURN, ''

  ;; Check that end is after start or return a null string.
  IF (finish LE start) THEN RETURN, ''

  ;; Return a null string if there are no header lines.
  IF (start EQ (finish - 1)) THEN BEGIN
     data = data[finish+1:*]
     RETURN, ''
  ENDIF

  ;; Extract the header and the data.
  header = text[start+1:finish-1]
  data = (finish LT (N_Elements(text) - 1)) ? text[finish+1:*] : ''

  ;; Obtain the header lines.
  index = Where(StrMid(header, 0, 2) EQ ';>', count)

  ;; Update the header array or return if no header lines.
  IF (count GT 0) THEN $
     header = header[index] $
  ELSE $
     RETURN, ''

  ;; Go through each header line creating arrays.
  nHeader = N_Elements(header)
  FOR i=0,nHeader-1 DO BEGIN

     ;; Split string at '='.
     parts = StrSplit(StrMid(header[i], 2), '=', /Extract)
     IF (N_Elements(parts) LT 2) THEN $
        Message, Jam_Message(file, "Field contains no '=' symbol.")

     ;; Fill in NAME array.
     newName = StrTrim(parts[0], 2)
     name = (N_Elements(name) EQ 0) ? [newName] : [name, newName]

     ;; Split string at '|'.
     parts = StrSplit(StrJoin(parts[1:*]), '|', /Extract)

     ;; Fill in VALUE array.
     newValue = StrTrim(parts[0], 2)
     value = (N_Elements(value) EQ 0) ? [newValue] : [value, newValue]

  ENDFOR

  ;; Create output structure.
  FOR i=0,nHeader-1 DO BEGIN
     IF ((StrPos(value[i], "'") NE -1) || $
         (StrPos(value[i], '"') NE -1)) THEN BEGIN
            thisValue = StrMid(value[i], 1, StrLen(value[i])-2)
     ENDIF $
     ELSE BEGIN
        IF ((StrPos(value[i], '.') NE -1) || $
            (StrPos(value[i], 'E') NE -1) || $
            (StrPos(value[i], 'e') NE -1) || $
            (StrPos(value[i], 'D') NE -1) || $
            (StrPos(value[i], 'd') NE -1)) THEN BEGIN
               thisValue = Double(value[i])
        ENDIF $
        ELSE BEGIN
           thisValue = Long(value[i])
        ENDELSE
     ENDELSE
     struct = (N_Elements(struct) EQ 0) ? $
              Create_Struct(name[i], thisValue) : $
              Create_Struct(struct, name[i], thisValue)
  ENDFOR

  RETURN, struct

END ;; -----------------------------------------------------------------------

FUNCTION Jam_ReadData, file, Comment=comment, TName=tName, Labels=labels, $
                       N_Columns=n_Columns, Ignore_Comments=ignore_comments

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', Jam_Syntax('Jam_ReadData', /Is_Function))
  IF (~Jam_Check(file, /Is_File, Msg=msg)) THEN $
     Message, Jam_Message(file, msg)
  IF (N_Elements(comment) NE 0) THEN BEGIN
     IF (~Jam_Check(comment, TName='STRING', N_Dimensions=0, Msg=msg)) THEN $
        Message, Jam_Message('COMMENT', msg)
     IF (StrLen(comment) NE 1) THEN $
        Message, Jam_Message('COMMENT', 'Must be one character long.')
  ENDIF

  ;; Read the header.
  header = Jam_ReadHeader(file, Data=data)

  ;; Check if there is a line giving the column labels.
  IF (~((N_Elements(data) EQ 1) && (data NE ''))) THEN BEGIN
     index = Where(StrMid(data, 0, 2) EQ ';~', count)
     IF (count GT 0) THEN labelLine = (data[index])[0]
  ENDIF

  ;; Obtain a text array containing all lines of data.
  IF (N_Elements(comment) EQ 0) THEN comment = ';'
  IF (Keyword_Set(ignore_comments)) THEN BEGIN
     data = JAM_ReadText(file, /Remove_Blanks)
     idx = Where(StrMid(StrTrim(data, 1), 0, 2) EQ ';~', Complement=cIdx)
     data = data[cIdx]
     idx = Where(StrMid(StrTrim(data, 1), 0, 1) EQ comment, cnt)
     IF (cnt GT 0) THEN data[idx] = StrTrim(StrMid(data[idx], 1), 1)
  ENDIF $
  ELSE BEGIN
	  data = Jam_ReadText(file, Comment=comment, /Remove_Blanks, /Remove_Comments)
  ENDELSE
  nLines = N_Elements(data)

  ;; Return header only if no data. Error if no header or data.
  IF ((N_Elements(data) EQ 1) && (data EQ '')) THEN BEGIN
     IF (Size(header, /TName) EQ 'STRUCT') THEN BEGIN
        RETURN, header
     ENDIF ELSE $
        Message, Jam_Message(file, 'No header or data.')
  ENDIF

  ;; Set the number of columns.
  IF (N_Elements(n_Columns) NE 0) THEN BEGIN
     IF (~Jam_Check(n_Columns, TName='FIX', N_Dimensions=0, Msg=msg)) THEN $
        Message, Jam_Message('NCOLUMNS', msg)
  ENDIF ELSE $
     n_Columns = N_Elements(StrSplit(data[0]))

  ;; Set type of each data column.
  CASE (N_Elements(tName)) OF
     0: $
        BEGIN

        ;; Get data from the first line.
        value = StrSplit(data[0], /Extract)

        ;; Check the type of each column from the syntax.
        _tName = StrArr(n_Columns)
        FOR i=0,n_Columns-1 DO BEGIN
           IF ((StrPos(value[i], "'") NE -1) || $
               (StrPos(value[i], '"') NE -1)) THEN $
                  _tName[i] = 'STRING' $
           ELSE $
              IF ((StrPos(value[i], '.') NE -1) || $
                  (StrPos(value[i], 'E') NE -1) || $
                  (StrPos(value[i], 'e') NE -1) || $
                  (StrPos(value[i], 'D') NE -1) || $
                  (StrPos(value[i], 'd') NE -1)) THEN $
                     _tName[i] = 'DOUBLE' $
              ELSE $
                 _tName[i] = 'LONG'
        ENDFOR
        idx = Where(StrUpCase(value) EQ 'NAN', cnt)
        IF (cnt GT 0) THEN _tName[idx] = 'NAN'

     END
     1: $
        BEGIN
        IF (~Jam_Check(tName, TName='STRING', N_Dimensions=0, Msg=msg)) THEN $
           Message, Jam_Message('TNAME', msg)
        _tName = Replicate(tName, n_Columns)
     END
     ELSE: $
        BEGIN
        IF (~Jam_Check(tName, TName='STRING', N_Dimensions=1, $
                       N_Elements=n_Columns, Msg=msg)) THEN $
                          Message, Jam_Message('TNAME', msg)
        _tName = tName
     END
  ENDCASE

  ;; Set label of each data column.
  IF (N_Elements(labels) NE 0) THEN BEGIN
     IF (~Jam_Check(labels, TName='STRING', N_Dimensions=1, $
                    N_Elements=n_Columns, Msg=msg)) THEN BEGIN
        Message, Jam_Message('LABELS', msg)
     ENDIF
  ENDIF $
  ELSE BEGIN

     ;; Get labels from commented line in the file.
     IF (N_Elements(labelLine) NE 0) THEN $
        labels = (StrSplit(labelLine, /Extract))[1:*]

     ;; Create number labels if the commented line isn't the right length.
     IF (N_Elements(labels) NE n_Columns) THEN BEGIN
        numbers = IndGen(n_Columns) + 1
        test = Where((numbers LT 10), count)
        numbers = String(numbers)
        IF (count GT 0) THEN $
           numbers[test] = StrCompress('0' + numbers[test])
        labels = StrCompress(('Column' + numbers), /Remove_All)
     ENDIF

  ENDELSE

  ;; Create output array.
  dataArray = StrArr(n_Columns, nLines)
  FOR i=0,nLines-1 DO BEGIN
     tmpArray = StrSplit(data[i], /Extract)
     IF (N_Elements(tmpArray) LT n_Columns) THEN BEGIN
        Message, Jam_Message('NCOLUMNS', $
                             'Data has incorrect number of columns.')
     ENDIF
     dataArray[*,i] = tmpArray[0:n_Columns-1]
  ENDFOR

  ;; Create output data structure.
  FOR i=0,n_Columns-1 DO BEGIN
     dataLabel = labels[i]
     dataValue = (_tName[i] EQ 'NAN') ? $
                 Replicate(!Values.D_NaN, N_Elements(dataArray[i,*])) : $
                 Reform(Fix(dataArray[i,*], Type=Jam_Type(_tName[i])))
     IF (_tName[i] EQ 'STRING') THEN BEGIN
        test = dataValue[0]
        IF ((StrPos(test, "'") NE -1) || (StrPos(test, '"') NE -1)) THEN $
           dataValue = Reform(StrMid(dataArray[i,*], 1, $
                                     StrLen(dataArray[i,*])-2))
     ENDIF
     struct = (N_Elements(struct) EQ 0) ? $
              Create_Struct(dataLabel, dataValue) : $
              Create_Struct(struct, dataLabel, dataValue)
  ENDFOR

  ;; Add header data.
  IF (Size(header, /TName) EQ 'STRUCT') THEN $
     struct = Create_Struct(header, struct)

  RETURN, struct

END ;; -----------------------------------------------------------------------
