
PRO JAM_PS, fileName, Close=close, EPS=EPS, Times=times, Portrait=portrait, $
            Landscape=landscape, _Extra=extra

  Compile_Opt IDL2
  On_Error, 2

  ;; Set default file name.
  IF (N_Elements(fileName) EQ 0) THEN fileName = 'idl.ps'

  ;; Check filename.
  IF (~Keyword_Set(close)) THEN $
    IF (~JAM_Check(fileName, TName='STRING', N_Elements=1, Msg=msg)) THEN $
      Message, JAM_Message('FileName', 'Valid name must be given.')

  ;; Create common block.
  Common JAM_PS, closed, thisFile, thisDevice, thisFont, thisDecomposed

  ;; ----- Set-up postscript -----
  IF (~Keyword_Set(close)) THEN BEGIN

    ;; Error if postscript file is currently open.
    IF (N_Elements(closed) NE 0) THEN $
      IF (closed EQ 0) THEN $
        Message, JAM_Message(thisFile, 'Postscript file already open.')

    ;; Set closed flag to indicate that a file is open.
    closed = 0

    ;; Save current settings.
    thisFile = fileName
    thisDevice = !D.Name
    thisFont = !P.Font
    Device, Get_Decomposed=thisDecomposed

    ;; Set color decomposition state.
    Device, Decomposed=0

    ;; Set PS device current.
    Set_Plot, 'PS', /Copy

    ;; Set-up device options.
    Device, Encapsulated=Keyword_Set(EPS), Bits_Per_Pixel=8, /Color, $
      /IsoLatin1, Language_Level=2, Times=times, Portrait=portrait, $
      Landscape=landscape, _Extra=extra
    Device, FileName=fileName

    ;; Use hardware fonts.
    !P.Font = 0

  ENDIF $

  ;; ----- Close postscript -----
  ELSE BEGIN

    ;; Obtain common block.
    Common JAM_PS, closed, thisFile, thisDevice, thisFont, thisDecomposed

    ;; Close the device.
    Device, /Close

    ;; Reset device to previous state.
    Device, /Helvetica
    !P.Font = thisFont
    Device, /Portrait

    ;; Reset to previous device.
    Set_Plot, thisDevice

    ;; Reset decomposed state.
    Device, Decomposed=thisDecomposed

    ;; Set closed flag.
    closed = 1
    thisFile = ''

  ENDELSE

  RETURN

END
