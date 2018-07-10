;+ ===========================================================================
; NAME:
;       Jam_T_bb
;
; PURPOSE:
;       This function....
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_T_bb()
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

FUNCTION Jam_T_bb, Num_T_bb=num_T_bb, Min_T_bb=min_T_bb, Max_T_bb=max_T_bb, $
                   Debug=debug

  ;; Set compiler options and error handling.
  Compile_Opt IDL2
  IF (~Keyword_Set(debug)) THEN On_Error, 2

  ;; Set debugging options.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  num_T_bb = 30
  min_T_bb = 3.D    ;; [K]
  max_T_bb = 1750.D ;; [K]
  T_bb = Jam_Dist(num_T_bb, min_T_bb, max_T_bb)

  ;; Restore debugging options.
  !Except = except

  RETURN, T_bb

END
