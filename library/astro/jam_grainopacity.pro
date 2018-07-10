
;     { Result }.
;               |- [ <Wave> ]
;               |- [ <T_bb> ]
;               |- { Sil }.
;                         |- { Amo }.
;                                   |- [[  Tot ]]
;                                   |- [[ Cont ]]
;                                   |- [[ Feat ]]
;                         |- { Fst }.
;                                   |- [[  Tot ]]
;                                   |- [[ Cont ]]
;                                   |- [[ Feat ]]
;                         |- { Ens }.
;                                   |- [[  Tot ]]
;                                   |- [[ Cont ]]
;                                   |- [[ Feat ]]
;               |- [[ Carb ]]

FUNCTION Jam_GrainOpacity, wave, T_bb, K_Ext=K_Ext, IR=IR, Gra=gra, $
                           C_Table=C_Table, T_Table=T_Table, $
                           dnda_Table=dnda_Table, Fst_Table=Fst_Table, $
                           Ens_Table=Ens_Table, C_Abs=_C_Abs, C_Ext=_C_Ext, $
                           _dnda=_dnda, NoPAH=noPAH, _Extra=extra

  Compile_Opt IDL2
  On_Error, 2

  ;; Obtain wavelength vector.
  IF (N_Elements(wave) EQ 0) THEN wave = Jam_Wave(IR=IR)
  logWave = ALog(wave)
  N_Wave = N_Elements(wave)

  ;; Set default blackbody temperature vector.
  IF (N_Elements(T_bb) EQ 0) THEN T_bb = Jam_T_bb()
  N_T_bb = N_Elements(T_bb)

  ;; Set default grain radius vector.
  a = Jam_GrainRadii(NumRadii=N_A)
  loga = ALog(a)
  dloga = ALog(a[1] / a[0])
  big_a = Rebin(a, N_a, N_Wave, N_T_bb)

  ;; Calculate absorption cross sections.
  IF ((N_Elements(_C_Abs) EQ 0) OR (N_Elements(_C_Ext) EQ 0)) THEN BEGIN
     _C_Abs = Jam_GrainCrossSection(wave, a, C_Ext=_C_Ext, C_Table=C_Table, $
                                    _Extra=extra)
     C_Abs = _C_Abs
     C_Ext = _C_Ext
  ENDIF $
  ELSE BEGIN
     C_Abs = _C_Abs
     C_Ext = _C_Ext
  ENDELSE
  C_Abs_Carb = (Keyword_Set(gra)) ? C_Abs.Gra : $
               (Keyword_Set(noPAH)) ? $
               0.5D * (C_Abs.Carb.Neu.Cont + C_Abs.Carb.Ion.Cont) : $
               0.5D * (C_Abs.Carb.Neu.Tot + C_Abs.Carb.Ion.Tot)
  C_Ext_Carb = (Keyword_Set(gra)) ? C_Ext.Gra : $
               (Keyword_Set(noPAH)) ? $
               0.5D * (C_Ext.Carb.Neu.Cont + C_Ext.Carb.Ion.Cont) : $
               0.5D * (C_Ext.Carb.Neu.Tot + C_Ext.Carb.Ion.Tot)
  C_Abs_Sil = Transpose(C_Abs.Sil) ;; [N_a x N_Wave]
  C_Abs_Carb = Transpose(C_Abs_Carb)
  C_Ext_Sil = Transpose(C_Ext.Sil) ;; [N_a x N_Wave]
  C_Ext_Carb = Transpose(C_Ext_Carb)

  ;; Obtain the grain-size DF.
  IF (N_Elements(_dnda) EQ 0) THEN BEGIN
     _dnda = Jam_GrainSizeDF(a, T_bb, C_Table=C_Table, T_Table=T_Table, $
                             dnda_Table=dnda_Table, _Extra=extra)
     dnda = _dnda
  ENDIF ELSE $
     dnda = _dnda

  ;; Calculate total silicate opacities.
  big_dnda_Sil = Transpose(Rebin(dnda.Sil, N_a, N_T_bb, N_Wave), [0,2,1])
  int_Sil = big_a * big_dnda_Sil
  K_Abs_Sil_Tot = Jam_IntTab(int_Sil * $
                             Rebin(C_Abs_Sil, N_a, N_Wave, N_T_bb), dloga)
  IF (Arg_Present(K_Ext)) THEN BEGIN
     K_Ext_Sil_Tot = Jam_IntTab(int_Sil * $
                                Rebin(C_Ext_Sil, N_a, N_Wave, N_T_bb), dloga)
  ENDIF

  ;; Calculate carbonaceous opacities.
  big_dnda_Carb = Transpose(Rebin(dnda.Carb, N_a, N_T_bb, N_Wave), [0,2,1])
  int_Carb = big_a * big_dnda_Carb
  K_Abs_Carb = Jam_IntTab(int_Carb * $
                          Rebin(C_Abs_Carb, N_a, N_Wave, N_T_bb), dloga)
  IF (Arg_Present(K_Ext)) THEN BEGIN
     K_Ext_Carb = Jam_IntTab(int_Carb * $
                             Rebin(C_Ext_Carb, N_a, N_Wave, N_T_bb), dloga)
  ENDIF

  ;; Create index of wavelength points to fit silicate continuum to.
  WAVE_BELOW_MIN = 1.D
  WAVE_BELOW_MAX = 5.2D
  WAVE_MIDDLE_MIN = 13.8
  WAVE_MIDDLE_MAX = 14.5
  WAVE_ABOVE_MIN = 1D2
  WAVE_ABOVE_MAX = 1D3
  idx = Where((wave GE WAVE_BELOW_MIN) AND (wave LE WAVE_ABOVE_MAX))
  idxCon = Where(((wave GE WAVE_BELOW_MIN) AND $
                  (wave LE WAVE_BELOW_MAX)) OR $
                 ((wave GE WAVE_MIDDLE_MIN) AND $
                  (wave LE WAVE_MIDDLE_MAX)) OR $
                 ((wave GE WAVE_ABOVE_MIN) AND $
                  (wave LE WAVE_ABOVE_MAX)), cntCon)
  
  ;; Calculate silicate continuum and amorphous features.
  K_Sil_Cont = DblArr(N_Wave, N_T_bb, /NoZero)
  K_Amo_Feat = DblArr(N_Wave, N_T_bb, /NoZero)
 IF (cntCon GT 0) THEN BEGIN
  FOR i=0,N_T_bb-1 DO BEGIN
     
     IF (Total(K_Abs_Sil_Tot) GT 0.D) THEN BEGIN
        
        ;; Calculate silicate continuum.
        _K_Sil_Tot = K_Abs_Sil_Tot[*,i]
        _K_Sil_Cont = _K_Sil_Tot
        _K_Sil_Cont[idx] = $
           Exp(Spl_Interp(ALog(wave[idxCon]), $
                          ALog(_K_Sil_Tot[idxCon]), $
                          Spl_Init(ALog(wave[idxCon]), $
                                   ALog(_K_Sil_Tot[idxCon])), $
                          ALog(wave[idx])))
        
        ;; Subtract continuum to obtain 10/18 micron features.
        _K_Amo_Feat = (_K_Sil_Tot - _K_Sil_Cont) > 0.D
        idxMin = Where((_K_Amo_Feat / _K_Sil_Cont) LE 1D-3, cntMin)
        IF (cntMin GT 0) THEN _K_Amo_Feat[idxMin] = 0.D
        K_Amo_Feat[0,i] = _K_Amo_Feat
        
        ;; Subtract features from total to obtain final continuum.
        K_Sil_Cont[0,i] = _K_Sil_Tot - _K_Amo_Feat
        
     ENDIF $
     ELSE BEGIN
        K_Sil_Cont[0,i] = Replicate(0.D, N_Wave)
        K_Amo_Feat[0,i] = Replicate(0.D, N_Wave)
     ENDELSE
     
  ENDFOR
 ENDIF
  K_Abs_Sil_Cont = K_Sil_Cont
  K_Abs_Amo_Feat = K_Abs_Sil_Tot - K_Abs_Sil_Cont
  IF (Arg_Present(K_Ext)) THEN BEGIN
     K_Ext_Amo_Feat = K_Abs_Amo_Feat
     K_Ext_Sil_Cont = K_Ext_Sil_Tot - K_Ext_Amo_Feat
  ENDIF

  ;; Obtain crystalline silicate feature mass-opacities.
  IF (N_Elements(Fst_Table) EQ 0) THEN $
     Fst_Table = Jam_ReadData(!Jam.Path.Tables + 'k_abs.fst.txt')
  IF (N_Elements(Ens_Table) EQ 0) THEN $
     Ens_Table = Jam_ReadData(!Jam.Path.Tables + 'k_abs.ens.txt')
  K_PerMass_Fst = Replicate(0.D, N_Wave)
  K_PerMass_Ens = K_PerMass_Fst
  idx = Where((wave GE Min(Fst_Table.Wave)) AND $
              (wave LE Max(Fst_Table.Wave)), cnt)
  IF (cnt GT 0) THEN BEGIN
     K_PerMass_Fst[idx] = Exp(Interpol(ALog(Fst_Table.k_Abs), $
                                       ALog(Fst_Table.Wave), logWave[idx]))
     idx = Where((wave GE Min(Ens_Table.Wave)) AND $
                 (wave LE Max(Ens_Table.Wave)))
     K_PerMass_Ens[idx] = Exp(Interpol(ALog(Ens_Table.k_Abs), $
                                       ALog(Ens_Table.Wave), logWave[idx]))
  ENDIF

  ;; Calculate crystalline silicate feature opacities.
  densitySil = 3.5D-12 ;; [g um-3]
  big_Mass = Rebin(4.D / 3.D * !DPi * densitySil * a^3, N_a, N_Wave)
  C_Fst0 = Transpose(Rebin(K_PerMass_Fst, N_Wave, N_a))
  C_Ens0 = Transpose(Rebin(K_PerMass_Ens, N_Wave, N_a))
  C_Fst = big_Mass * C_Fst0
  C_Ens = big_Mass * C_Ens0
  K_Abs_Fst_Feat = Jam_IntTab(Rebin(C_Fst, N_a, N_Wave, N_T_bb), dloga)
  K_Abs_Ens_Feat = Jam_IntTab(Rebin(C_Ens, N_a, N_Wave, N_T_bb), dloga)
  IF (Arg_Present(K_Ext)) THEN BEGIN
     K_Ext_Fst_Feat = K_Abs_Fst_Feat
     K_Ext_Ens_Feat = K_Abs_Ens_Feat
  ENDIF

  ;; Create return vectors.
  K_Abs_Sil_Cont = (N_Elements(K_Abs_Sil_Cont) EQ 1) ? $
                   K_Abs_Sil_Cont[0] : Reform(K_Abs_Sil_Cont)
  K_Abs_Amo_Feat = (N_Elements(K_Abs_Amo_Feat) EQ 1) ? $
                   K_Abs_Amo_Feat[0] : Reform(K_Abs_Amo_Feat)
  K_Abs_Fst_Feat = (N_Elements(K_Abs_Fst_Feat) EQ 1) ? $
                   K_Abs_Fst_Feat[0] : Reform(K_Abs_Fst_Feat)
  K_Abs_Ens_Feat = (N_Elements(K_Abs_Ens_Feat) EQ 1) ? $
                   K_Abs_Ens_Feat[0] : Reform(K_Abs_Ens_Feat)
  K_Abs_Carb = (N_Elements(K_Abs_Carb) EQ 1) ? $
               K_Abs_Carb[0] : Reform(K_Abs_Carb)
  idx = Where(wave LT 1D-3, cnt)
  IF (cnt GT 0) THEN BEGIN
     K_Abs_Sil_Cont[idx] = 0.D
     K_Abs_Amo_Feat[idx] = 0.D
     K_Abs_Fst_Feat[idx] = 0.D
     K_Abs_Ens_Feat[idx] = 0.D
     K_Abs_Carb[idx] = 0.D
  ENDIF
  IF (Arg_Present(K_Ext)) THEN BEGIN
     K_Ext_Sil_Cont = (N_Elements(K_Ext_Sil_Cont) EQ 1) ? $
                      K_Ext_Sil_Cont[0] : Reform(K_Ext_Sil_Cont)
     K_Ext_Amo_Feat = (N_Elements(K_Ext_Amo_Feat) EQ 1) ? $
                      K_Ext_Amo_Feat[0] : Reform(K_Ext_Amo_Feat)
     K_Ext_Fst_Feat = (N_Elements(K_Ext_Fst_Feat) EQ 1) ? $
                      K_Ext_Fst_Feat[0] : Reform(K_Ext_Fst_Feat)
     K_Ext_Ens_Feat = (N_Elements(K_Ext_Ens_Feat) EQ 1) ? $
                      K_Ext_Ens_Feat[0] : Reform(K_Ext_Ens_Feat)
     K_Ext_Carb = (N_Elements(K_Ext_Carb) EQ 1) ? $
                  K_Ext_Carb[0] : Reform(K_Ext_Carb)
     IF (cnt GT 0) THEN BEGIN
        K_Ext_Sil_Cont[idx] = 0.D
        K_Ext_Amo_Feat[idx] = 0.D
        K_Ext_Fst_Feat[idx] = 0.D
        K_Ext_Ens_Feat[idx] = 0.D
        K_Ext_Carb[idx] = 0.D
     ENDIF
     K_Ext = { Wave:wave, $
               T_bb:T_bb, $
               Sil:{ Amo:{ Tot:K_Ext_Sil_Tot, $
                           Cont:K_Ext_Sil_Cont, $
                           Feat:K_Ext_Amo_Feat }, $
                     Fst:{ Tot:K_Ext_Sil_Cont + K_Ext_Fst_Feat, $
                           Cont:K_Ext_Sil_Cont, $
                           Feat:K_Ext_Fst_Feat }, $
                     Ens:{ Tot:K_Ext_Sil_Cont + K_Ext_Ens_Feat, $
                           Cont:K_Ext_Sil_Cont, $
                           Feat:K_Ext_Ens_Feat } }, $
               Carb:K_Ext_Carb }
  ENDIF

  RETURN, { Wave:wave, $
            T_bb:T_bb, $
            Sil:{ Amo:{ Tot:K_Abs_Sil_Tot, $
                        Cont:K_Abs_Sil_Cont, $
                        Feat:K_Abs_Amo_Feat }, $
                  Fst:{ Tot:K_Abs_Sil_Cont + K_Abs_Fst_Feat, $
                        Cont:K_Abs_Sil_Cont, $
                        Feat:K_Abs_Fst_Feat }, $
                  Ens:{ Tot:K_Abs_Sil_Cont + K_Abs_Ens_Feat, $
                        Cont:K_Abs_Sil_Cont, $
                        Feat:K_Abs_Ens_Feat } }, $
            Carb:K_Abs_Carb }

END
