;+ ===========================================================================
; NAME:
;       Jam_Deriv
;
; PURPOSE:
;       This function returns the numerical derivatives of a user-defined
;       function.
;
; CATEGORY:
;       Mathematics
;
; CALLING SEQUENCE:
;       Result = Jam_Deriv()
;
; PROCEDURE:
;       df(x)/dp_i = 
;           {f[x;p_i*(1+HiEPS)] - f[x;p_i*(1-LoEPS)]} / (p_i*(LoEPS+HiEPS))
;
; AUTHOR:
;       Jason A. Marshall
;       Department of Astronomy, Cornell University, Ithaca, NY 14853
;       jam258@cornell.edu
;
; MODIFICATION HISTORY:
;       Written by: Jason A. Marshall, 15 Nov 2006.
;-
;; ***************************************************************************
; Copyright (C) 2006, 2007, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ***************************************************************************

FUNCTION Jam_Deriv, func, x, params, PError=pError, Covar=covar, $
                    LoEPS=loEPS, HiEPS=hiEPS, Side=side, Index=index, $
                    Debug=debug, _Extra=extra, Check=check

  Compile_Opt IDL2
  On_Error, 2

  ;; Check syntax.
  nParams = N_Params()
  IF (nParams LT 2) THEN $
     Message, Jam_Message('Syntax', Jam_Syntax('Jam_Deriv', /Is_Function))

  ;; Check if called with an 'x' vector.
  IF (nParams EQ 2) THEN params = x
  noX = Array_Equal(params, x)

  ;; Set hi and lo EPS values.
  mch = MachAr(/Double)
  N_PAR = N_Elements(params)
  IF (N_Elements(loEPS) EQ 0) THEN loEPS = Replicate(Sqrt(mch.EPS), N_PAR)
  IF (N_Elements(hiEPS) EQ 0) THEN hiEPS = Replicate(Sqrt(mch.EPS), N_PAR)
  IF (N_Elements(loEPS) EQ 1) THEN loEPS = Replicate(loEPS, N_PAR)
  IF (N_Elements(hiEPS) EQ 1) THEN hiEPS = Replicate(hiEPS, N_PAR)

  ;; Enforce "side-edness" of derivative.
  IF (N_Elements(side) NE 0) THEN BEGIN
     idxR = Where(side EQ -1, cntR)
     idxL = Where(side EQ  1, cntL)
     IF (cntR GT 0) THEN loEPS[idxR] = 0.D
     IF (cntL GT 0) THEN hiEPS[idxL] = 0.D
  ENDIF

  ;; Check input.
  IF (Keyword_Set(debug)) THEN BEGIN
     IF (~Jam_Check(func, /IS_FUNCTION, Msg=msg)) THEN $
        Message, Jam_Message(func, msg)
     IF ((~noX) AND (~Jam_Check(x, TName='NUM', Msg=msg))) THEN $
        Message, Jam_Message('x', msg)
     IF (~Jam_Check(params, TName='NUM', Max_N_Dim=1, Msg=msg)) THEN $
        Message, Jam_Message('Params', msg)
     IF (~Jam_Check(loEPS, TName='NUM', $
                    N_Elements=N_PAR, Min_Value=[0.D], Msg=msg)) THEN $
                       Message, Jam_Message('LoEPS', msg)
     IF (~Jam_Check(hiEPS, TName='NUM', $
                    N_Elements=N_PAR, Min_Value=[0.D], Msg=msg)) THEN $
                       Message, Jam_Message('HiEPS', msg)
  ENDIF
  
  ;; Get pError array if only the covariance matrix is given.
  IF ((N_Elements(pError) EQ 0) AND (N_Elements(covar) NE 0)) THEN $
     pError = Sqrt(Diag_Matrix(covar))

  ;; Check which parameters have non-zero errors (i.e. weren't fixed).
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

  ;; Loop over each parameter.
  N_X = N_Elements(x)
  derivs = (noX) ? DblArr(N_PAR) : DblArr(N_X, N_PAR)
  init = 0
  delta = Abs(params) * (loEPS + hiEPS)
  FOR i=0,N_PAR-1 DO BEGIN
     IF (useParam[i]) THEN BEGIN
        _params = params
        _params[i] *= (1.D + hiEPS[i])
        funcHi = (noX) ? $
                 Call_Function(func, _params, _Extra=extra) : $
                 Call_Function(func, x, _params, _Extra=extra)
        _params = params
        _params[i] *= (1.D - loEPS[i])
        funcLo = (noX) ? $
                 Call_Function(func, _params, _Extra=extra) : $
                 Call_Function(func, x, _params, _Extra=extra)
        IF (Jam_Check(funcLo, TName='STRUCT')) THEN BEGIN
           N_TAGS = N_Tags(funcLo)
           IF (init EQ 0) THEN BEGIN
              tags = Tag_Names(funcLo)
              array = (noX) ? $
                      DblArr(N_TAGS, N_PAR) : $
                      DblArr(N_X, N_TAGS, N_PAR)
              init = 1
           ENDIF
           FOR j=0,N_TAGS-1 DO BEGIN
              deriv = (funcHi.(j) - funcLo.(j)) / delta[i]
              thisIndex = Where(Finite(deriv, /NaN), thisCount)
              IF (thisCount GT 0) THEN deriv[thisIndex] = 0.D
              IF (noX) THEN BEGIN
                 array[j,i] = deriv
              ENDIF ELSE $
                 array[0,j,i] = deriv ;; NOTE: array[0,j,i] = array[0:*,j,i]
           ENDFOR
        ENDIF $
        ELSE BEGIN
           deriv = (funcHi - funcLo) / delta[i]
           thisIndex = Where(Finite(deriv, /NaN), thisCount)
           IF (thisCount GT 0) THEN deriv[thisIndex] = 0.D
           IF (noX) THEN BEGIN
              derivs[i] = Abs(deriv)
           ENDIF ELSE $
              derivs[0,i] = Abs(deriv) ;; NOTE: derivs[0,i] = derivs[0:*,i]
        ENDELSE
     ENDIF
  ENDFOR

  ;; Create output structure of derivs.
  IF (N_Elements(array) NE 0) THEN BEGIN
     array = Abs(array)
     derivs = (noX) ? $
              Create_Struct(tags[0], Reform(array[0,*])) : $
              Create_Struct(tags[0], Reform(array[*,0,*]))
     FOR i=1,N_TAGS-1 DO BEGIN
        derivs = (noX) ? $
                 Create_Struct(derivs, tags[i], Reform(array[i,*])) : $
                 Create_Struct(derivs, tags[i], Reform(array[*,i,*]))
     ENDFOR
  ENDIF

  RETURN, derivs

END
