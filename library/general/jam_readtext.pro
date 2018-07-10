;+
; NAME:
; JAM_READTEXT
;
; PURPOSE:
; This function reads an ASCII text file into an IDL string array.
;
; CATEGORY:
; General Programming, I/O.
;
; CALLING SEQUENCE:
; Result = JAM_READTEXT(File)
;
; INPUTS:
; File: A scalar string containing the full path name of the ASCII file.
;
; KEYWORD PARAMETERS:
; COMMENT: Set this keyword equal to a length one string containing
;   the character used to identify comment lines. The default is ';'.
; REMOVE_BLANKS: Set this keyword to exclude blank lines from the array.
; REMOVE_COMMENTS: Set this keyword to exclude lines beginning with the
;   IDL comment character ';'. If the keyword COMMENT is set, the
;   character it contains is used to identify comment lines instead.
;
; OUTPUTS:
; This function returns an IDL string array containing one element per
; line of text in File.
;
; EXAMPLE:
; To read this file into a text array enter:
;   IDL> Result = JAM_READTEXT(File_Which('jam_readtext.pro'))
;
; MODIFICATION HISTORY:
;   Written by:  Jason Marshall, May 2004.
;-

;/+
FUNCTION Jam_ReadText, file, Comment=comment, Remove_Blanks=remove_blanks, $
  Remove_Comments=remove_comments, Debug=debug

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 1) THEN $
    Message, Jam_Message('Syntax', Jam_Syntax('Jam_ReadText', /Is_Function))
  IF (~Jam_Check(file, /Is_File, Msg=msg)) THEN $
    Message, Jam_Message(file, msg)

  ;; Obtain the number of lines in the file.
  nLines = File_Lines(file)

  ;; Return null string if number of lines is equal to zero.
  IF (nLines EQ 0) THEN RETURN, ''

  ;; Read the text file into a string array.
  text = StrArr(nLines)
  OpenR, LUN, file, /Get_LUN
  ReadF, LUN, text
  Free_LUN, LUN

  ;; Remove blank lines.
  IF (Keyword_Set(remove_blanks)) THEN BEGIN
    blank = Where(text EQ '', Complement=notBlank, NComplement=nNotBlank)
    IF (nNotBlank GT 0) THEN text = text[notBlank]
  ENDIF

  ;; Remove comment lines.
  IF (Keyword_Set(remove_comments)) THEN BEGIN

    ;; Check comment character input and set value.
    IF (N_Elements(comment) NE 0) THEN BEGIN
      IF (~Jam_Check(comment, TName='STRING', N_Dimensions=0, Msg=msg)) THEN $
          Message, Jam_Message('COMMENT', msg)
      IF (StrLen(comment) NE 1) THEN $
        Message, Jam_Message('COMMENT', 'Must be one character long.')
    ENDIF $
    ELSE $
      comment = ';'

    ;; Extract non-comment lines.
    trimText = StrMid(StrTrim(text,1),0,1)
    comments = Where(trimText EQ comment, Complement=notComment, $
      NComplement=nNotComment)
    IF (nNotComment GT 0) THEN $
      text = text[notComment] $
    ELSE $
      text = ''

  ENDIF

  RETURN, text

END
;\-
