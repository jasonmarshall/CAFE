;+
; NAME:
; JAM_WINDOW
;
; PURPOSE:
; This procedure creates a new graphics window and returns the window index
; as an output keyword. The index is always obtained using the FREE keyword
; to the WINDOW procedure.
;
; CATEGORY:
; General Programming; Graphics.
;
; CALLING SEQUENCE:
; JAM_WINDOW
;
; KEYWORD PARAMETERS:
; WID: Set to obtain the window index number of the newly created window.
; TITLE: Set this keyword to the scalar string title for the new window.
; WINDOW_SCALE: Set this keyword to a decimal scalar used to calculate the
;   size of the window as a fraction of the screen size. This keyword is
;   ignored if the WINDOW_SIZE keyword is used.
; WINDOW_SIZE: Set this keyword to a two element vector containing the x
;   and y integer pixel size of the window.
; WINDOW_POS: Set this keyword to a two element vector containing the x
;   and y device coordinates of the window position.
;
;   If neither WINDOW_SCALE or WINDOW_SIZE are set, a default scaling of
;   0.7 is used.
;
; EXAMPLE:
; To create a new graphics window with the default window scaling and
; positioning enter:
;   IDL> JAM_WINDOW, Title='New Window'
;
; MODIFICATION HISTORY:
;   Written by:  Jason Marshall, 23 May 2004.
;-

;/+
PRO Jam_Window, WID=WID, Title=title, Window_Scale=window_scale, $
                Window_Size=window_size, Window_Pos=window_pos

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Define constants.
  DEFAULT_SCALE = 0.7

  ;; Check input parameters.
  IF (N_Elements(title) NE 0) THEN $
     IF (~Jam_Check(title, TName='STRING', N_Dimensions=0, Msg=msg)) THEN $
        Message, Jam_Message('TITLE', msg)
  IF (N_Elements(window_size) NE 0) THEN $
     IF (~Jam_Check(window_size, $
                    TName='NUM', N_Dimensions=1, N_Elements=2, Msg=msg)) THEN $
                       Message, Jam_Message('WINDOW_SIZE', msg)
  IF (N_Elements(window_scale) NE 0) THEN $
     IF (~Jam_Check(window_scale, TName='FLT', N_Dimensions=0, Msg=msg)) THEN $
        Message, Jam_Message('WINDOW_SCALE', msg)
  IF (N_Elements(window_pos) NE 0) THEN $
     IF (~Jam_Check(window_pos, $
                    TName='NUM', N_Dimensions=1, N_Elements=2, Msg=msg)) THEN $
                       Message, Jam_Message('WINDOW_POS', msg)

  ;; Calculate scaling.
  IF (N_Elements(window_size) EQ 0) THEN BEGIN
     IF (N_Elements(window_scale) EQ 0) THEN $
        scale = DEFAULT_SCALE $
     ELSE $
        scale = window_scale
  ENDIF

  ;; Get screen size.
  screenSize = Jam_GetScreenSize()

  ;; Calculate x and y size if not given.
  IF (N_Elements(window_size) EQ 0) THEN BEGIN
     xSize = scale * screenSize[0]
     ySize = scale * screenSize[1]
  ENDIF $
  ELSE BEGIN
     xSize = window_size[0]
     ySize = window_size[1]
  ENDELSE

  ;; Calculate the x and y positions.
  IF (N_Elements(window_pos) EQ 0) THEN BEGIN
     screenSizeTot = Get_Screen_Size()
     xPos = (screenSize[0] - xSize) / 2.0
     IF (screenSizeTot[0] GT (2.D * screenSizeTot[1])) THEN $
        xPos += screenSize[0]
     yPos = (screenSize[1] - ySize) / 2.0
  ENDIF

  ;; Create graphics window.
  IF (N_Elements(title) EQ 0) THEN BEGIN
     Window, XSize=xSize, YSize=ySize, XPos=xPos, YPos=yPos, /Free
     wid = !D.Window
  ENDIF $
  ELSE BEGIN
     Window, XSize=xSize, YSize=ySize, XPos=xPos, YPos=yPos, Title=title, /Free
     wid = !D.Window
  ENDELSE

  RETURN

END
