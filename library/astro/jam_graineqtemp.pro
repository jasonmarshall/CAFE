;+ ===================================================================
; NAME:
; Jam_GrainEQTemp
;
; PURPOSE:
; This routine calculates the equilibrium temperatures of graphitic
; and silicate astronomical dust grains embedded in a given radiation
; field as a function of their size.
;
; CATEGORY:
; Astronomy, Physics.
;
; CALLING SEQUENCE:
; Result = Jam_GrainEQTemp([a, T_bb])
;
; OPTIONAL INPUTS:
; T_bb:
; a:
;
; KEYWORD PARAMETERS:
; C_ABS:
; L_WAVE:
; DLOGT:
;
; OUTPUT:
;
;   { Result }.
;             |- [ <a> ]
;             |- [ <T_bb> ]
;             |- [[  Sil ]]
;             |- [[ Carb ]]
;
; PROCEDURE:
;
;   Int[B(T) Q df] / Int[L Q df] = Int[B(T_bb) df] / Int[L df]
;
; EXAMPLE:
; To calculate grain temperatures enter:
;   IDL> Result = JAM_GRAINTEMPERATURE(...)
;
; REQUIREMENTS:
; JamLib - !JAM, JAM_CHECK, JAM_READDATA, JAM_GRAINCROSSSECTION, JAM_COMPARE
; Markwardt - CMREPLICATE
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, October 2005.
;-

FUNCTION Jam_GrainEQTemp, a, T_bb, Gra=gra, SourceSED=sourceSED, $
                          SourceType=sourceType, C_Table=C_Table, $
                          T_Table=T_Table, C_Abs=_C_Abs, _Extra=extra, $
                          DU_AGN=DU_AGN, DU_SB=DU_SB, DU_Model=DU_Model, $ ;; <- For DUSTY ULIRGs project
                          Debug=debug

  Compile_Opt IDL2
  On_Error, 2

  ;; Setup debugging.
  IF (Keyword_Set(debug)) THEN BEGIN
     except = !Except
     !Except = 2
  ENDIF

  ;; Create vector of grain temperatures.
  T = (N_Elements(T_Table) EQ 0) ? Jam_T_bb() : T_Table.T
  logT = ALog(T)
  N_T = N_Elements(T)

  ;; Create default blackbody temperature vector.
  IF (N_Elements(T_bb) EQ 0) THEN T_bb = T
  N_T_bb = N_Elements(T_bb)
  IF (Keyword_Set(debug)) THEN BEGIN
     IF (~Jam_Check(T_bb, TName='NUM', $
                    Max_N_Dimensions=1, Min_Value=[0.D], Msg=msg)) THEN $
                       Message, Jam_Message('T_bb', msg)
  ENDIF

  ;; Set default grain radius vector.
  IF (N_Elements(a) EQ 0) THEN a = Jam_GrainRadii()
  loga = ALog(a)
  N_A = N_Elements(a)
  IF (Keyword_Set(debug)) THEN BEGIN
     IF (~Jam_Check(a, TName='NUM', $
                    Max_N_Dimensions=1, Min_Value=[0.D], Msg=msg)) THEN $
                       Message, Jam_Message('a', msg)
  ENDIF

  ;; Set default wavelength vector.
  wave = Jam_Wave()
  logWave = ALog(wave)
  N_Wave = N_Elements(wave)

  ;; Code for DUSTY ULIRGs project:
  IF (N_Elements(DU_Model) NE 0) THEN BEGIN
     subDir = ['Dusty_ULIRGs', 'models', 'input']
     pathInput = FilePath('AGN-' + DU_AGN + '_SB-' + DU_SB, Root_Dir=!PROJECTS, SubDir=subDir)
     data = JAM_ReadData(FilePath(DU_Model + '.dat', Root_Dir=pathInput))
     f_Wave = Jam_Jy2um(data.Wave, data.Flux)
     sourceSED = { Wave: data.Wave, f_Wave: f_Wave }
  ENDIF
  IF (0) THEN BEGIN ;; <- Old method.
     f_N7714 = 0.D
     N7714 = JAM_ReadData(File_Which('ngc7714.txt'))
     wave_N7714 = N7714.Wave[Uniq(N7714.Wave)]
     flux_N7714 = JAM_Jy2um(wave_N7714, Interpol(N7714.Flux, N7714.Wave, wave_N7714))
     flux_N7714 /= Int_Tabulated(ALog(wave_N7714), wave_N7714 * flux_N7714)
     PG0804 = JAM_ReadData(File_Which('pg0804.txt'))
     wave_PG0804 = PG0804.Wave[Uniq(PG0804.Wave)]
     flux_PG0804 = JAM_Jy2um(wave_PG0804, Interpol(PG0804.Flux, PG0804.Wave, wave_PG0804))
     flux_PG0804 /= Int_Tabulated(ALog(wave_PG0804), wave_PG0804 * flux_PG0804)
     wave_Total = wave_N7714
     flux_Total = f_N7714 * flux_N7714 + $
                  Interpol((1.D - f_N7714) * flux_PG0804, wave_PG0804, wave_Total)
     sourceSED_B = { Wave:wave_Total, f_Wave: flux_Total }
  ENDIF

  ;; Obtain illuminating SED.
  IF (N_Elements(sourceSED) EQ 0) THEN BEGIN
     f_Wave = Jam_SourceSED(wave, sourceType, _Extra=extra)
  ENDIF $
  ELSE BEGIN
     f_Wave = (~Array_Equal(wave, sourceSED.Wave)) ? $
              Exp(Interpol(ALog(sourceSED.f_Wave), $
                           ALog(sourceSED.Wave), logWave)) : $
              sourceSED.f_Wave
  ENDELSE

  ;; Calculate absorption cross sections.
  IF (N_Elements(_C_Abs) EQ 0) THEN BEGIN
     _C_Abs = Jam_GrainCrossSection(wave, a, C_Table=C_Table, _Extra=extra, $
                                    Debug=debug)
     C_Abs = _C_Abs
  ENDIF $
  ELSE BEGIN
     IF ((~Array_Equal(wave, _C_Abs.Wave)) OR $
         (~Array_Equal(a, _C_Abs.a))) THEN BEGIN
        C_Abs = Jam_GrainCrossSection(wave, a, C_Table=C_Table, _Extra=extra, $
                                      Debug=debug)
     ENDIF ELSE $
        C_Abs = _C_Abs
  ENDELSE
  C_Sil = C_Abs.Sil
  gra = 1
  C_Carb = (Keyword_Set(gra)) ? C_Abs.Gra : $
           0.5D * (C_Abs.Carb.Neu.Tot + C_Abs.Carb.Ion.Tot)

  ;; Integrate over wavelength for each radius and temperature to calculate:
  ;; LHS = Int[C_Abs(wave,a) * B_Wave(wave,T), wave]
  dlogWave = ALog(wave[1] / wave[0])
  IF (N_Elements(T_Table) EQ 0) THEN BEGIN
     intSil = Transpose(Rebin(C_Sil, N_WAVE, N_A, N_T), [0,2,1])
     intCarb = Transpose(Rebin(C_Carb, N_WAVE, N_A, N_T), [0,2,1])
     bigPlanck = Rebin(Jam_Planck(wave, T, /um), N_Wave, N_T, N_A)
     intSil *= bigPlanck
     intCarb *= bigPlanck
     bigWave = Rebin(wave, N_Wave, N_T, N_A)
     LHS_Sil = Jam_IntTab(bigWave * intSil, dlogWave) ;; [N_T x N_A]
     LHS_Carb = Jam_IntTab(bigWave * intCarb, dlogWave)
     T_Table = { a:a, T:T, Sil:LHS_Sil, Carb:LHS_Carb }
  ENDIF $
  ELSE BEGIN
     LHS_Sil = T_Table.Sil
     LHS_Carb = T_Table.Carb
  ENDELSE

  ;; Integrate over wavelength for each grain size to calculate:
  ;; RHS = Int[f_Wave(wave) * C_Abs(wave,a), wave] / Int[f_Wave(wave), wave].
  big_f_Wave = Rebin(f_Wave, N_Wave, N_A)
  intSil = C_Sil * big_f_Wave
  intCarb = C_Carb * big_f_Wave
  bigWave = Rebin(wave, N_Wave, N_A)
  f_Wave_Tot = Jam_IntTab(wave * f_Wave, dlogWave)
  RHS_Sil = Jam_IntTab(bigWave * intSil, dlogWave) / f_Wave_Tot ;; [N_A]
  RHS_Carb = Jam_IntTab(bigWave * intCarb, dlogWave) / f_Wave_Tot

  ;; Create grain temperature arrays.
  log_T_Sil = DblArr(N_A, N_T_bb, /NoZero)
  log_T_Carb = DblArr(N_A, N_T_bb, /NoZero)

  ;; Loop over each blackbody temperature.
  sigmaSB = 5.67D-5 ;; Stefan-Boltzmann constant in [ergs s-1 cm-2 K-4]
  const = ALog(sigmaSB / !DPi * T_bb^4)
  log_RHS_Sil = ALog(RHS_Sil)
  log_RHS_Carb = ALog(RHS_Carb)
  log_LHS_Sil = ALog(LHS_Sil)
  log_LHS_Carb = ALog(LHS_Carb)
  FOR i=0,N_T_bb-1 DO BEGIN
     this_log_RHS_Sil = const[i] + log_RHS_Sil
     this_log_RHS_Carb = const[i] + log_RHS_Carb
     FOR j=0,N_A-1 DO BEGIN
        log_T_Sil[j,i] = Interpol(logT, log_LHS_Sil[*,j], $
                                  this_log_RHS_Sil[j])
        log_T_Carb[j,i] = Interpol(logT, log_LHS_Carb[*,j], $
                                   this_log_RHS_Carb[j])
     ENDFOR
  ENDFOR
  T_Sil = Exp(log_T_Sil)
  T_Carb = Exp(log_T_Carb)

  IF (Keyword_Set(debug)) THEN !Except = except

  RETURN, { a:a, $
            T_bb:T_bb, $
            Sil:(N_Elements(T_Sil) EQ 1) ? T_Sil[0] : Reform(T_Sil), $
            Carb:(N_Elements(T_Carb) EQ 1) ? T_Carb[0] : Reform(T_Carb) }

END
