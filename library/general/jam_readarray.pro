;+
; NAME:
; JAM_READARRAY
;
; PURPOSE:
; This function reads an ASCII text file into an IDL array. Text files
; containing a single column of data are read into one dimensional IDL
; arrays. Text files containing more than one column are read into two
; dimensional IDL arrays. Text files containing more than one block of
; rows and columns are read into three dimensional IDL arrays.
;
; CATEGORY:
; General Programming, I/O.
;
; CALLING SEQUENCE:
; Result = JAM_READARRAY(File)
;
; INPUTS:
; File: A scalar string containing the full path name of the data file.
;
; KEYWORD PARAMETERS:
; BLOCK_FIRST: Set this keyword to return an nBlock by nRow by nCol array
;   for three dimensional data. The default is to return an nRow by nCol
;   by nBlock array.
; BREAK_LINES: Set this keyword equal to a scalar integer containing the
;   number of lines to skip between each block of three dimensional data.
;   This keyword is ignored for one and two dimensional data.
; HEADER_LINES: Set this keyword equal to a scalar integer containing the
;   number of header lines to skip at the top of the ASCII data file.
; N_BLOCK: Set this keyword equal to a scalar integer containing the number
;   of three dimensional blocks of data to read.
; N_ROW: Set this keyword equal to a scalar integer containing the number
;   of rows in each two dimensional block of data.
;
; OUTPUTS:
; If File contains one dimensional data, this function will return an array
; containing nRow elements. If File contains two dimensional data, this
; function will return an array containing nRow by nCol elements. If File
; contains three dimensional data, this function will return an array
; containing nRow by nCol by nBlock elements.
;
; EXAMPLE:
; To read a two dimensional ASCII data file named 'Data2D.txt' containing
; no header lines enter:
;   IDL> Result = JAM_READARRAY('{Path}Data2D.txt')
; To read a three dimensional ASCII data file named 'Data3D.txt' containing
; 2 blocks of text, each of which have 5 rows enter:
;   IDL> Result = JAM_READARRAY('{Path}Data3D.txt', N_BLOCK=2, N_ROW=5)
;
; MODIFICATION HISTORY:
;   Written by:  Jason Marshall, May 2004.
;-

;/+
FUNCTION Jam_ReadArray, file, Block_First=block_first, $
  Break_Lines=break_lines, Header_Lines=header_lines, $
  N_Block=n_block, N_Row=n_row

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 1) THEN $
    Message, Jam_Message('Syntax', Jam_Syntax('Jam_ReadArray', /Is_Function))
  IF (~Jam_Check(file, /Is_File, Msg=msg)) THEN $
    Message, Jam_Message('File', msg)
  IF (N_Elements(n_row) NE 0) THEN $
    IF (~Jam_Check(n_row, TName='FIX', N_Dimensions=0, Msg=msg)) THEN $
      Message, Jam_Message('N_ROW', msg)
  IF (N_Elements(n_block) NE 0) THEN $
    IF (~Jam_Check(n_block, TName='FIX', N_Dimensions=0, Msg=msg)) THEN $
      Message, Jam_Message('N_DEEP', msg)
  IF (N_Elements(header_lines) NE 0) THEN $
    IF (~Jam_Check(header_lines, TName='FIX', N_Dimensions=0, Msg=msg)) THEN $
      Message, Jam_Message('HEADER_LINES', msg)
  IF (N_Elements(break_lines) NE 0) THEN $
    IF (~Jam_Check(break_lines, TName='FIX', N_Dimensions=0, Msg=msg)) THEN $
      Message, Jam_Message('BREAK_LINES', msg)

  ;; Check that number of rows is given for 3D arrays.
  IF (N_Elements(n_block) NE 0) THEN $
    IF (N_Elements(n_row) EQ 0) THEN $
      Message, Jam_Message('N_ROW', 'Required for 3D arrays.')

  ;; Count the number of columns.
  OpenR, LUN, file, /Get_LUN
  IF (N_Elements(header_lines) NE 0) THEN Skip_LUN, LUN, header_lines, /Lines
  line = ''
  ReadF, LUN, line
  nCol = N_Elements(StrSplit(line))
  Free_LUN, LUN

  ;; Count number of rows if not given.
  IF (N_Elements(n_row) EQ 0) THEN BEGIN
    nRow = File_Lines(file)
    IF (N_Elements(header_lines) NE 0) THEN nRow -= header_lines
  ENDIF ELSE $
    nRow = n_row

  ;; Set nBlock to 1 for 2D arrays.
  nBlock = (N_Elements(n_block) EQ 0) ? 1 : n_block

  ;; Set up data array.
  data = (Keyword_Set(block_first)) ? $
    DblArr(nBlock, nRow, nCol, /NoZero) : $
    DblArr(nRow, nCol, nBlock, /NoZero)

  ;; Open file for reading.
  OpenR, LUN, file, /Get_LUN

  ;; Skip the header lines.
  IF (N_Elements(header_lines) NE 0) THEN Skip_LUN, LUN, header_lines, /Lines

  ;; Read in data.
  FOR i=0,nBlock-1 DO BEGIN
    IF ((N_Elements(break_lines) NE 0) && (i NE 0)) THEN $
      Skip_LUN, LUN, break_lines, /Lines
    tempData = DblArr(nCol, nRow, /NoZero)
    ReadF, LUN, tempData
    IF (Keyword_Set(block_first)) THEN $
      data[i,*,*] = Transpose(tempData) $
    ELSE $
      data[0,0,i] = Transpose(tempData)
  ENDFOR

  ;; Close file.
  Free_LUN, LUN

  RETURN, Reform(data)

END
;\-