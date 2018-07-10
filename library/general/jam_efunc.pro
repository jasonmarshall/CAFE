;+ ===========================================================================
; NAME:
;       Jam_EFunc
;
; PURPOSE:
;       This function calculates the uncertainty in a function.
;
; CATEGORY:
;       Mathematics
;
; CALLING SEQUENCE:
;       Result = Jam_EFunc()
;
; AUTHOR:
;       Jason A. Marshall
;       Department of Astronomy, Cornell University, Ithaca, NY 14853
;       jam258@cornell.edu
;
; MODIFICATION HISTORY:
;       Written by: Jason A. Marshall, 17 Nov 2006.
;-
;; ***************************************************************************
; Copyright (C) 2006, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ***************************************************************************

FUNCTION Jam_EFunc, _derivs, pError, Covar=covar, Index=index, Tag=tag

  Compile_Opt IDL2
  On_Error, 2

  ;; Get pError array if only the covariance matrix is given.
  IF ((N_Elements(pError) EQ 0) AND (N_Elements(covar) NE 0)) THEN $
     pError = Sqrt(Diag_Matrix(covar))

  ;; Ensure that pError is given.
  IF (N_Elements(pError) EQ 0) THEN $
     Message, 'Error: Either PError or COVAR must be given!'

  ;; Check which parameters have non-zero errors (i.e. weren't fixed).
  N_PAR = N_Elements(pError)
  nonZero = Replicate(1, N_PAR)
  IF (N_Elements(pError) NE 0) THEN BEGIN
     idx = Where(pError EQ 0.D, cnt)
     IF (cnt GT 0) THEN nonZero[idx] = 0
  ENDIF

  ;; Check if index of parameters to include in error sum is given.
  useParam = Replicate(0, N_PAR)
  IF (N_Elements(index) EQ 0) THEN index = LIndGen(N_PAR)
  useParam[index] = 1

  ;; Combine 'useParam' and 'nonZero' checks so that both must be satisfied
  ;; simultaneously to use each parameter.
  useParam *= nonZero

  ;; Get particular tag.
  IF (N_Elements(tag) NE 0) THEN BEGIN
     tags = Tag_Names(_derivs)
     idx = Where(tags EQ tag, cnt)
     IF (cnt NE 1) THEN Message, 'Tag is not found in Derivs!'
     derivs = _derivs.(idx)
  ENDIF ELSE $
     derivs = _derivs

  ;; Loop over parameter variance and covariances.
  covariance = (N_Elements(covar) NE 0)
  IF (Jam_Check(derivs, TName='STRUCT')) THEN BEGIN
     tags = Tag_Names(derivs)
     N_TAGS = N_Elements(tags)
     noX = (Size(derivs.(0), /N_Dimensions) EQ 1)
     N_X = (noX) ? 1 : N_Elements((derivs.(0))[*,0])
     FOR i=0,N_TAGS-1 DO BEGIN
        tot = DblArr(N_X)
        FOR j=0,N_PAR-1 DO BEGIN
           IF (useParam[j]) THEN BEGIN
              IF (noX) THEN BEGIN
                 tot += ((derivs.(i))[j] * pError[j])^2
              ENDIF ELSE $
                 tot += ((derivs.(i))[*,j] * pError[j])^2
           ENDIF
        ENDFOR
        IF (covariance) THEN BEGIN
           FOR j=0,N_PAR-1 DO BEGIN
              IF (useParam[j]) THEN BEGIN
                 FOR k=j+1,N_PAR-1 DO BEGIN
                    IF (useParam[k]) THEN BEGIN
                       IF (noX) THEN BEGIN
                          tot += (2.D * covar[j,k] * $
                                  Abs((derivs.(i))[j] * (derivs.(i))[k]))
                       ENDIF $
                       ELSE BEGIN
                          tot += (2.D * covar[j,k] * $
                                  Abs((derivs.(i))[*,j] * (derivs.(i))[*,k]))
                       ENDELSE
                    ENDIF
                 ENDFOR
              ENDIF
           ENDFOR
        ENDIF
        thisEFunc = (noX) ? Sqrt(tot[0] > 0.D) : Sqrt(tot > 0.D)
        eFunc = (N_Elements(eFunc) EQ 0) ? $
                Create_Struct(tags[i], thisEFunc) : $
                Create_Struct(eFunc, tags[i], thisEFunc)
     ENDFOR
  ENDIF $
  ELSE BEGIN
     noX = (Size(derivs, /N_Dimensions) EQ 1)
     N_X = (noX) ? 1 : N_Elements(derivs[*,0])
     tot = DblArr(N_X)
     FOR j=0,N_PAR-1 DO BEGIN
        IF (useParam[j]) THEN BEGIN
           IF (noX) THEN BEGIN
              tot += (derivs[j] * pError[j])^2
           ENDIF ELSE $
              tot += (derivs[*,j] * pError[j])^2
        ENDIF
     ENDFOR
     IF (covariance) THEN BEGIN
        FOR j=0,N_PAR-1 DO BEGIN
           IF (useParam[j]) THEN BEGIN
              FOR k=j+1,N_PAR-1 DO BEGIN
                 IF (useParam[k]) THEN BEGIN
                    IF (noX) THEN BEGIN
                       tot += (2.D * covar[j,k] * $
                               Abs(derivs[j] * derivs[k]))
                    ENDIF $
                    ELSE BEGIN
                       tot += (2.D * covar[j,k] * $
                               Abs(derivs[*,j] * derivs[*,k]))
                    ENDELSE
                 ENDIF
              ENDFOR
           ENDIF
        ENDFOR
     ENDIF
     eFunc = (noX) ? Sqrt(tot[0] > 0.D) : Sqrt(tot > 0.D)
  ENDELSE

  RETURN, (N_Elements(eFunc) EQ 1) ? eFunc[0] : Reform(eFunc)

END
