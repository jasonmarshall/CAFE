;+ ===========================================================================
; NAME:
;       Jam_Wave
;
; PURPOSE:
;       This function....
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_Wave()
;
; AUTHOR:
;       Jason A. Marshall
;       Department of Astronomy, Cornell University, Ithaca, NY 14853
;       jam258@cornell.edu
;
; MODIFICATION HISTORY:
;       Written by: Jason A. Marshall, 04 Mar 2006.
;-
;; ---------------------------------------------------------------------------
; Copyright (C) 2006, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ---------------------------------------------------------------------------

FUNCTION Jam_Wave, IR=IR, NumWave=numWave, MinWave=minWave, $
                   MaxWave=maxWave, Debug=debug

  ;; Set compiler options and error handling.
  Compile_Opt IDL2
  IF (~Keyword_Set(debug)) THEN On_Error, 2

  ;; Set debugging options.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  IF (Keyword_Set(IR)) THEN BEGIN
     numWave = 500
     minWave = 1.D  ;; [um]
     maxWave = 1D3  ;; [um]
  ENDIF $
  ELSE BEGIN
     numWave = 241
     minWave = 1D-3 ;; [um]
     maxWave = 1D+3 ;; [um]
  ENDELSE

  ;; Restore debugging options.
  !Except = except

  RETURN, Jam_Dist(numWave, minWave, maxWave)

END
