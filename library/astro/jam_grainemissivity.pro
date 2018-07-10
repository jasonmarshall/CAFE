
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
;
;; Note: By default, emissivities are returned in [Jy sr-1 H-1 cm+2].

FUNCTION Jam_GrainEmissivity, wave, T_bb, um=um, Hz=Hz, Gra=gra, IR=IR, $
                              C_Table=C_Table, T_Table=T_Table, $
                              dnda_Table=dnda_Table, Fst_Table=Fst_Table, $
                              Ens_Table=Ens_Table, C_Abs=_C_Abs, _dnda=_dnda, $
                              T_EQ=_T_EQ, Big=big, Small=small, Model=model, $
                              aMaxValue=aMaxValue, X_Cry=X_Cry, $
                              _Extra=extra

  Compile_Opt IDL2
  On_Error, 2

  ;; Obtain wavelength vector.
  IF (N_Elements(wave) EQ 0) THEN wave = Jam_Wave(IR=IR)
  logWave = ALog(wave)
  N_Wave = N_Elements(wave)

  ;; Set default blackbody temperature vector.
  IF (N_Elements(T_bb) EQ 0) THEN T_bb = Jam_T_bb()
  log_T_bb = ALog(T_bb)
  N_T_bb = N_Elements(T_bb)

  ;; Set default grain radius vector.
  a = Jam_GrainRadii(NumRadii=N_A)
  loga = ALog(a)
  dloga = ALog(a[1] / a[0])
  big_a = Rebin(a, N_a, N_Wave, N_T_bb)

  ;; Calculate absorption cross sections.
  IF (N_Elements(_C_Abs) EQ 0) THEN BEGIN
     _C_Abs = Jam_GrainCrossSection(wave, a, C_Table=C_Table, _Extra=extra)
     C_Abs = _C_Abs
  ENDIF $
  ELSE BEGIN
     IF ((~Array_Equal(wave, _C_Abs.Wave)) OR $
         (~Array_Equal(a, _C_Abs.a))) THEN BEGIN
        C_Abs = Jam_GrainCrossSection(wave, a, C_Table=C_Table, _Extra=extra)
     ENDIF ELSE $
        C_Abs = _C_Abs
  ENDELSE
  C_Carb = (Keyword_Set(gra)) ? C_Abs.Gra : $
           0.5D * (C_Abs.Carb.Neu.Cont + C_Abs.Carb.Ion.Cont)
  C_Sil = Transpose(C_Abs.Sil) ;; [N_a x N_Wave]
  C_Carb = Transpose(C_Carb)

  ;; Obtain the grain temperatures.
  IF (N_Elements(_T_EQ) EQ 0) THEN BEGIN
     _T_EQ = Jam_GrainEQTemp(a, T_bb, Gra=gra, C_Table=C_Table, $
                             T_Table=T_Table, _Extra=extra)
     T_EQ = _T_EQ
  ENDIF $
  ELSE BEGIN
     IF ((~Array_Equal(a, _T_EQ.a)) OR $
         (~Array_Equal(T_bb, _T_EQ.T_bb))) THEN BEGIN
        T_EQ = Jam_GrainEQTemp(a, T_bb, Gra=gra, C_Table=C_Table, $
                               T_Table=T_Table, _Extra=extra)
     ENDIF ELSE $
        T_EQ = _T_EQ
  ENDELSE

  ;; Obtain the grain-size DF.
  IF (N_Elements(_dnda) EQ 0) THEN BEGIN
     _dnda = Jam_GrainSizeDF(a, T_bb, T_EQ=T_EQ, C_Table=C_Table, $
                             T_Table=T_Table, dnda_Table=dnda_Table, $
                             Big=big, Small=small, Model=model, $
                             aMaxValue=aMaxValue)
     dnda = _dnda
  ENDIF $
  ELSE BEGIN
     IF (~Array_Equal(a, _dnda.a)) THEN BEGIN
        dnda = Jam_GrainSizeDF(a, T_bb, T_EQ=T_EQ, C_Table=C_Table, $
                               T_Table=T_Table, dnda_Table=dnda_Table, $
                               Big=big, Small=small, Model=model, $
                               aMaxValue=aMaxValue)
     ENDIF ELSE $
        dnda = _dnda
  ENDELSE

  ;; Calculate Planck functions.
  planck_Sil = DblArr(N_Wave, N_a, N_T_bb, /NoZero)
  planck_Carb = DblArr(N_Wave, N_a, N_T_bb, /NoZero)
  FOR i=0,N_T_bb-1 DO BEGIN
     planck_Sil[*,*,i] = Jam_Planck(wave, T_EQ.Sil[*,i], um=um, Hz=Hz)
     planck_Carb[*,*,i] = Jam_Planck(wave, T_EQ.Carb[*,i], um=um, Hz=Hz)
  ENDFOR
  planck_Sil = (N_T_bb EQ 1) ? $
               Transpose(planck_Sil) : Transpose(planck_Sil, [1,0,2])
  planck_Carb = (N_T_bb EQ 1) ? $
                Transpose(planck_Carb) : Transpose(planck_Carb, [1,0,2])

  ;; Calculate total silicate emissivities.
  big_dnda_Sil = Transpose(Rebin(dnda.Sil, N_a, N_T_bb, N_Wave), [0,2,1])
  E_Sil_Tot = Jam_IntTab(big_a * big_dnda_Sil * planck_Sil * $
                         Rebin(C_Sil, N_a, N_Wave, N_T_bb), dloga)

  ;; Calculate carbonaceous emissivities.
  big_dnda_Carb = Transpose(Rebin(dnda.Carb, N_a, N_T_bb, N_Wave), [0,2,1])
  int_Carb = big_a * big_dnda_Carb * planck_Carb
  E_Carb = Jam_IntTab(int_Carb * Rebin(C_Carb, N_a, N_Wave, N_T_bb), dloga)

  ;; Create index of wavelength points to fit silicate continuum to.
  WAVE_BELOW_MIN = 1.D
  WAVE_BELOW_MAX = 5.2D
  WAVE_ABOVE_MIN = 1D2
  WAVE_ABOVE_MAX = 1D3
  idx = Where((wave GE WAVE_BELOW_MIN) AND (wave LE WAVE_ABOVE_MAX))
  idxCon = Where(((wave GE WAVE_BELOW_MIN) AND $
                  (wave LE WAVE_BELOW_MAX)) OR $
                 ((wave GE WAVE_ABOVE_MIN) AND $
                  (wave LE WAVE_ABOVE_MAX)), cntCon)
  idxConLo = Where((wave GE WAVE_BELOW_MIN) AND (wave LE WAVE_BELOW_MAX), cntLo)
  idxConHi = Where((wave GE WAVE_ABOVE_MIN) AND (wave LE WAVE_ABOVE_MAX), cntHi)

  ;; Calculate silicate continuum and amorphous features.
  E_Sil_Cont = DblArr(N_Wave, N_T_bb)
  E_Amo_Feat = DblArr(N_Wave, N_T_bb)
 IF ((cntLo GT 0) AND (cntHi GT 0)) THEN BEGIN

  FOR i=0,N_T_bb-1 DO BEGIN

     IF (Total(E_Sil_Tot) GT 0.D) THEN BEGIN
        
        ;; Calculate silicate continuum.
        _E_Sil_Tot = E_Sil_Tot[*,i]
        _E_Sil_Cont = _E_Sil_Tot
        _E_Sil_Cont[idx] = $
           Exp(Spl_Interp(ALog(wave[idxCon]), $
                          ALog(_E_Sil_Tot[idxCon]), $
                          Spl_Init(ALog(wave[idxCon]), $
                                   ALog(_E_Sil_Tot[idxCon])), $
                          ALog(wave[idx])))
        
        ;; Subtract continuum to obtain 10/18 micron features.
        _E_Amo_Feat = (_E_Sil_Tot - _E_Sil_Cont) > 0.D
        idxMin = Where((_E_Amo_Feat / _E_Sil_Cont) LE 1D-3, cntMin)
        IF (cntMin GT 0) THEN _E_Amo_Feat[idxMin] = 0.D
        E_Amo_Feat[0,i] = _E_Amo_Feat

        ;; Subtract features from total to obtain final continuum.
        E_Sil_Cont[0,i] = _E_Sil_Tot - _E_Amo_Feat

     ENDIF $
     ELSE BEGIN
        E_Sil_Cont[0,i] = Replicate(0.D, N_Wave)
        E_Amo_Feat[0,i] = Replicate(0.D, N_Wave)
     ENDELSE

  ENDFOR
 ENDIF

  ;; Obtain crystalline silicate feature mass-opacities.
  IF (N_Elements(Fst_Table) EQ 0) THEN $
     Fst_Table = Jam_ReadData(!Jam.Path.Tables + 'k_abs.fst.txt')
  IF (N_Elements(Ens_Table) EQ 0) THEN $
     Ens_Table = Jam_ReadData(!Jam.Path.Tables + 'k_abs.ens.txt')
  K_PerMass_Fst = Replicate(0.D, N_Wave)
  K_PerMass_Ens = Replicate(0.D, N_Wave)
  idx = Where((wave GE Min(Fst_Table.Wave)) AND $
              (wave LE Max(Fst_Table.Wave)))
  K_PerMass_Fst[idx] = Exp(Interpol(ALog(Fst_Table.k_Abs), $
                                    ALog(Fst_Table.Wave), logWave[idx]))
  idx = Where((wave GE Min(Ens_Table.Wave)) AND $
              (wave LE Max(Ens_Table.Wave)))
  K_PerMass_Ens[idx] = Exp(Interpol(ALog(Ens_Table.k_Abs), $
                                    ALog(Ens_Table.Wave), logWave[idx]))
     
  ;; Calculate crystalline silicate feature emissivities.
  densitySil = 3.5D-12 ;; [g um-3]
  big_Mass = Rebin(4.D / 3.D * !DPi * densitySil * a^3, N_a, N_Wave)
  C_Fst0 = Transpose(Rebin(K_PerMass_Fst, N_Wave, N_a))
  C_Ens0 = Transpose(Rebin(K_PerMass_Ens, N_Wave, N_a))
  C_Fst = big_Mass * C_Fst0
  C_Ens = big_Mass * C_Ens0
  E_Fst_Feat = Jam_IntTab(Rebin(C_Fst, N_a, N_Wave, N_T_bb), dloga)
  E_Ens_Feat = Jam_IntTab(Rebin(C_Ens, N_a, N_Wave, N_T_bb), dloga)
  X_Cry = Jam_IntTab(Rebin(big_Mass, N_a, N_Wave, N_T_bb), dloga)

  ;; Create return structure.
  E_Sil_Cont = (N_Elements(E_Sil_Cont) EQ 1) ? $
               E_Sil_Cont[0] : Reform(E_Sil_Cont)
  E_Amo_Feat = (N_Elements(E_Amo_Feat) EQ 1) ? $
               E_Amo_Feat[0] : Reform(E_Amo_Feat)
  E_Fst_Feat = (N_Elements(E_Fst_Feat) EQ 1) ? $
               E_Fst_Feat[0] : Reform(E_Fst_Feat)
  E_Ens_Feat = (N_Elements(E_Ens_Feat) EQ 1) ? $
               E_Ens_Feat[0] : Reform(E_Ens_Feat)
  E_Carb = (N_Elements(E_Carb) EQ 1) ? E_Carb[0] : Reform(E_Carb)

  RETURN, { Wave:wave, $
            T_bb:T_bb, $
            Sil:{ Amo:{ Tot:E_Sil_Tot, $
                        Cont:E_Sil_Cont, $
                        Feat:E_Amo_Feat }, $
                  Fst:{ Tot:E_Sil_Cont + E_Fst_Feat, $
                        Cont:E_Sil_Cont, $
                        Feat:E_Fst_Feat }, $
                  Ens:{ Tot:E_Sil_Cont + E_Ens_Feat, $
                        Cont:E_Sil_Cont, $
                        Feat:E_Ens_Feat } }, $
            Carb:E_Carb }
  
END
