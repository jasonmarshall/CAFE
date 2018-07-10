
;   { Result }.
;             |- [ <T_bb> ]
;             |- [[  Sil ]]
;             |- [[ Carb ]]

FUNCTION Jam_GrainMinSize, T_bb, T_Max_Carb=T_Max_Carb, T_Max_Sil=T_Max_Sil, $
                           C_Table=C_Table, T_Table=T_Table, T_EQ=_T_EQ, $
                           _Extra=extra, Debug=debug

  Compile_Opt IDL2
  On_Error, 2

  ;; Setup debug.
  IF (Keyword_Set(debug)) THEN BEGIN
     except = !Except
     !Except = 2
  ENDIF

  ;; Set default blackbody temperatures.
  IF (N_Elements(T_bb) EQ 0) THEN T_bb = Jam_T_bb()
  log_T_bb = ALog(T_bb)
  N_T_bb = N_Elements(T_bb)

  ;; Set default grain radius vector.
  a = Jam_GrainRadii()
  log_a = ALog(a)
  N_A = N_Elements(a)

  ;; Obtain the grain temperatures.
  IF (N_Elements(_T_EQ) EQ 0) THEN BEGIN
     T_EQ = Jam_GrainEQTemp(a, T_bb, Gra=gra, C_Table=C_Table, $
                            T_Table=T_Table, C_Abs=C_Abs, _Extra=extra, $
                            Debug=debug)
  ENDIF $
  ELSE BEGIN
     IF ((~Array_Equal(a, _T_EQ.a)) OR $
         (~Array_Equal(T_bb, _T_EQ.T_bb))) THEN BEGIN
        T_EQ = Jam_GrainEQTemp(a, T_bb, Gra=gra, C_Table=C_Table, $
                               T_Table=T_Table, C_Abs=C_Abs, _Extra=extra, $
                               Debug=debug)
     ENDIF ELSE $
        T_EQ = _T_EQ
  ENDELSE
  T_Sil = T_EQ.Sil
  T_Carb = T_EQ.Carb
  log_T_Sil = ALog(T_Sil)
  log_T_Carb = ALog(T_Carb)

  ;; Set default maximum survival temperature of each grain type equal
  ;; to the grain sublimation temperature.
  T_Sub_Carb = 1750.D
  T_Sub_Sil = 1400.D
  IF (N_Elements(T_Max_Carb) EQ 0) THEN T_Max_Carb = T_Sub_Carb
  IF (N_Elements(T_Max_Sil) EQ 0) THEN T_Max_Sil = T_Sub_Sil
  log_T_Max_Carb = ALog(T_Max_Carb)
  log_T_Max_Sil = ALog(T_Max_Sil)

  ;; Loop over each blackbody temperature.
  aMin_Sil = DblArr(N_T_bb)
  aMin_Carb = DblArr(N_T_bb)
  idx = Where(T_bb GT 100.D, cnt)
  FOR i=0,cnt-1 DO BEGIN
     IF (Max(log_T_Sil[*,idx[i]]) GE log_T_Max_Sil) THEN BEGIN
        IF (Min(log_T_Sil[*,idx[i]]) GE log_T_Max_Sil) THEN BEGIN
           aMin_Sil[idx[i]] = Exp(Max(log_a))
        ENDIF $
        ELSE BEGIN
           _log_a = [-100, log_a, 100]
           _log_T_Sil = [100.D, log_T_Sil[*,idx[i]], log_T_Max_Sil]
           aMin_Sil[idx[i]] = Exp(Interpol(_log_a, _log_T_Sil, $
                                           log_T_Max_Sil))
        ENDELSE
     ENDIF
     IF (Max(log_T_Carb[*,idx[i]]) GE log_T_Max_Carb) THEN BEGIN
        IF (Min(log_T_Carb[*,idx[i]]) GE log_T_Max_Carb) THEN BEGIN
           aMin_Carb[idx[i]] = Exp(Max(log_a))
        ENDIF $
        ELSE BEGIN
           _log_a = [-100, log_a, 100]
           _log_T_Carb = [100.D, log_T_Carb[*,idx[i]], log_T_Max_Carb]
           aMin_Carb[idx[i]] = Exp(Interpol(_log_a, _log_T_Carb, $
                                            log_T_Max_Carb))
        ENDELSE
     ENDIF
  ENDFOR

  IF (Keyword_Set(debug)) THEN !Except = except

  RETURN, { T_bb:T_bb, $
            Sil:(N_T_bb EQ 1) ? aMin_Sil[0] : Reform(aMin_Sil), $
            Carb:(N_T_bb EQ 1) ? aMin_Carb[0] : Reform(aMin_Carb) }

END
