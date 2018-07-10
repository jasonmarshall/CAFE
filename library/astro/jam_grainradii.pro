;+ ===========================================================================
; NAME:
;       Jam_GrainRadii
;
; PURPOSE:
;       This function....
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_GrainRadii()
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

FUNCTION Jam_GrainRadii, NumRadii=numRadii, MinRadii=minRadii, $
                         MaxRadii=maxRadii, Debug=debug

  ;; Set compiler options and error handling.
  Compile_Opt IDL2
  IF (~Keyword_Set(debug)) THEN On_Error, 2

  ;; Set debugging options.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  numRadii = 81
  minRadii = 1D-3 ;; [um]
  maxRadii = 10.D ;; [um]
  radii = Jam_Dist(numRadii, minRadii, maxRadii)
  dlogRadii = ALog(radii[1] / radii[0])
  dlogVSG = ALog(minRadii / 3.5D-4)
  numRadiiVSG = Double(Ceil(dlogVSG / dlogRadii))
  numRadii += numRadiiVSG
  minRadii = Exp(ALog(minRadii) - numRadiiVSG * dlogRadii)
  radii = Jam_Dist(numRadii, minRadii, maxRadii)

  ;; Restore debugging options.
  !Except = except

  RETURN, radii

END
