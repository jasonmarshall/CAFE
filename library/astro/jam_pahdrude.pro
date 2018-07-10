;+ ===========================================================================
; NAME:
;       Jam_PAHDrude
;
; PURPOSE:
;       This function calculates the peak values of PAH features in the CNM
;       and PDR model components and returns the "Drude structure".
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_PAHDrude([N_CNM, N_PDR])
;
; AUTHOR:
;       Jason A. Marshall
;       Department of Astronomy, Cornell University, Ithaca, NY 14853
;       jam258@cornell.edu
;
; MODIFICATION HISTORY:
;       Written by: Jason A. Marshall, 21 Nov 2006.
;-
;; ***************************************************************************
; Copyright (C) 2006, 2007, 2008, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ***************************************************************************

FUNCTION Jam_PAHDrude, N_CNM, N_PDR, E_N_CNM=e_N_CNM, E_N_PDR=e_N_PDR, $
                       EDrude=eDrude, DrudeSB=drudeSB, Template=template, $
                       RatioTable=ratioTable, Debug=debug

  ;; Set compiler options and error handling.
  Compile_Opt IDL2
  IF (~Keyword_Set(debug)) THEN On_Error, 2

  ;; Set debugging options.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  ;; Set the default N_CNM & N_PDR values.
  IF (N_Elements(N_CNM) EQ 0) THEN N_CNM = 108
  IF (N_Elements(N_PDR) EQ 0) THEN N_PDR = N_CNM

  ;; Calculate PAH ratios for N_CNM & N_PDR values.
  ratios = (Arg_Present(eDrude)) ? $
           Jam_PAHRatios(N_CNM, N_PDR, E_N_CNM=e_N_CNM, E_N_PDR=e_N_PDR, $
                         ERatios=eRatios, RatioTable=ratioTable) : $
           Jam_PAHRatios(N_CNM, N_PDR, RatioTable=ratioTable)

  ;; Read PAH template file.
  IF (N_Elements(template) EQ 0) THEN BEGIN
     path = FilePath('pah_template.txt', Root_Dir=!Jam.Path.Tables)
     template = Jam_ReadData(path)
  ENDIF

  ;; Obtain wave0 and gamma vectors.
  wave0 = template.Wave0
  gamma = template.Gamma
  complex = template.Complex

  ;; Create index vectors for PAHs and complexes.
  idx3  = Where(complex EQ  0)
  idx6  = Where(complex EQ  3)
  idx7  = Where(complex EQ  5)
  idx11 = Where(complex EQ  9)
  idx12 = Where(complex EQ 11)
  idx17 = Where(complex EQ 16)

  ;; Set feature peaks to the values obtained using PAHFIT (Smith et
  ;; al. 2006) to fit the mean starburst spectrum of Brandl et al. (2006).
  ;;         PEAK           WAVE   N  COMPLEX
  peak = [0.00000000, $ ;;  3.30   0   0
          0.06159884, $ ;;  5.27   1   1
          0.07050930, $ ;;  5.70   2   2
          0.77360526, $ ;;  6.22   3   3
;;;          0.10000000, $ ;;  6.05   .   19
;;;          0.70000000, $ ;;  6.22   3   3
;;;          0.40000000, $ ;;  6.29   .   3
;;;          0.15000000, $ ;;  6.30   .   3
          0.09797205, $ ;;  6.69   4   4
          0.23897256, $ ;;  7.42   5   5
          0.89279697, $ ;;  7.60   6   5
          0.86156776, $ ;;  7.85   7   5
          0.25013986, $ ;;  8.33   8   6
          0.64566928, $ ;;  8.61   9   7
          0.02442993, $ ;; 10.68  10   8 ;;
;;;          0.20000000, $ ;; 11.00   .   9
          1.29167980, $ ;; 11.23  11   9
          1.04019180, $ ;; 11.33  12   9
          0.32429740, $ ;; 11.99  13  10
          0.84859132, $ ;; 12.62  14  11
          0.19136069, $ ;; 12.69  15  11
          0.21897772, $ ;; 13.48  16  12
          0.01000000, $ ;; 14.04  17  13 ;; <- Actually zero peak in fit. ;;
          0.17583134, $ ;; 14.19  18  14
          0.01376596, $ ;; 15.90  19  15 ;;
          0.42942265, $ ;; 16.45  20  16
          0.63617951, $ ;; 17.04  21  16
          0.29235229, $ ;; 17.375 22  16
          0.25877291, $ ;; 17.87  23  16
          0.31397021, $ ;; 18.92  24  17
          0.85513299]   ;; 33.10  25  18

  ;; Calculate the peak value of the 3.3 micron feature.
  ;; NOTES:
  ;;  * The 3.3/6.2 micron power ratio for IRAS 12112+0305 (w/ 3.3 data from
  ;;    Imanishi et al.) is used to calculate the peak 3.3 micron value.
  ;;  * PEAK(B) = [GAMMA(A)/GAMMA(B)]*[L(B)/L(A)]/[WAVE(A)/WAVE(B)]*PEAK(A)
  P_3_6_12112 = 0.064D
  peak[idx3] = (gamma[idx6] / gamma[idx3]) * P_3_6_12112 / $
               (wave0[idx6] / wave0[idx3]) * peak[idx6]

  ;; Create mean-SB Drude structure.
  IF (Arg_Present(drudeSB)) THEN BEGIN
     drudeSB = { Wave0:wave0, Gamma:gamma, Peak:peak, Complex:complex }
;;      wave = Jam_Dist(200, 3.D, 21.D)
;;      logWave = ALog(wave)
;;      fluxSB = Jam_DrudeFlux(wave, drudeSB)
;;      fluxSB0 = TSum(logWave, fluxSB / wave)
;;      drudeSB.Peak = peak / fluxSB0
  ENDIF

  ;; Calculate total power of PAH bands.
  P6 = Total(gamma[idx6] * peak[idx6] / wave0[idx6])
  P7 = Total(gamma[idx7] * peak[idx7] / wave0[idx7])
  P11 = Total(gamma[idx11] * peak[idx11] / wave0[idx11])
  P12 = Total(gamma[idx12] * peak[idx12] / wave0[idx12])
  P17 = Total(gamma[idx17] * peak[idx17] / wave0[idx17])

  ;; Calculate factor by which features must be scaled.
  scale7_CNM = (P11 / P7) / ratios.CNM.P11_P7
  scale7_PDR = (P11 / P7) / ratios.PDR.P11_P7
  ;; P11/P6 = P11/P7 * P7/P6 = P11/P7 / P6/P7
  P11_P6_CNM = ratios.CNM.P11_P7 / ratios.CNM.P6_P7
  P11_P6_PDR = ratios.PDR.P11_P7 / ratios.PDR.P6_P7
  scale6_CNM = (P11 / P6) / P11_P6_CNM
  scale6_PDR = (P11 / P6) / P11_P6_PDR

  ;; Scale features to create CNM and PDR peak models.
  peakCNM = peak
  peakPDR = peak
  idxScale6 = [0,3]
  idxScale7 = [1,2,4,5,6,7,8,9,10]
  peakCNM[idxScale7] *= scale7_CNM
  peakPDR[idxScale7] *= scale7_PDR
  peakCNM[idxScale6] *= scale6_CNM
  peakPDR[idxScale6] *= scale6_PDR

  ;; Create Drude structures.
  drudeCNM = { Wave0:wave0, Gamma:gamma, Peak:peakCNM, Complex:complex }
  drudePDR = { Wave0:wave0, Gamma:gamma, Peak:peakPDR, Complex:complex }

  ;; Scale peaks so that total PAH flux is normalized.
  wave = Jam_Dist(200, 3.D, 21.D)
  logWave = ALog(wave)
  fluxCNM = Jam_DrudeFlux(wave, drudeCNM)
  fluxPDR = Jam_DrudeFlux(wave, drudePDR)
  fluxCNM0 = TSum(logWave, fluxCNM / wave)
  fluxPDR0 = TSum(logWave, fluxPDR / wave)
  peakCNM /= fluxCNM0
  peakPDR /= fluxPDR0
  drudeCNM.Peak = peakCNM
  drudePDR.Peak = peakPDR

  ;; Calculate Drude errors.
  IF (Arg_Present(eDrude)) THEN BEGIN

     ;; Calculate scale factor errors.
     eScale7_CNM = scale7_CNM / ratios.CNM.P11_P7 * eRatios.CNM.P11_P7
     eScale7_PDR = scale7_PDR / ratios.PDR.P11_P7 * eRatios.PDR.P11_P7
     eP11_P6_CNM = Sqrt((eRatios.CNM.P11_P7 / ratios.CNM.P6_P7)^2 + $
                        (ratios.CNM.P11_P7 / (ratios.CNM.P6_P7)^2 * $
                         eRatios.CNM.P6_P7)^2)
     eP11_P6_PDR = Sqrt((eRatios.PDR.P11_P7 / ratios.PDR.P6_P7)^2 + $
                        (ratios.PDR.P11_P7 / (ratios.PDR.P6_P7)^2 * $
                         eRatios.PDR.P6_P7)^2)
     eScale6_CNM = scale6_CNM / P11_P6_CNM * eP11_P6_CNM
     eScale6_PDR = scale6_PDR / P11_P6_PDR * eP11_P6_PDR

     ;; Calculate peak errors.
     void = DblArr(N_Elements(peakCNM))
     ePeakCNM = void
     ePeakPDR = void
     ePeakCNM[idxScale7] = ePeakCNM[idxScale7] * eScale7_CNM
     ePeakPDR[idxScale7] = ePeakPDR[idxScale7] * eScale7_PDR
     ePeakCNM[idxScale6] = ePeakCNM[idxScale6] * eScale6_CNM
     ePeakPDR[idxScale6] = ePeakPDR[idxScale6] * eScale6_PDR

     ;; Create Drude error structure.
     eDrudeCNM = { Wave0:void, Gamma:void, Peak:ePeakCNM, Complex:complex }
     eDrudePDR = { Wave0:void, Gamma:void, Peak:ePeakPDR, Complex:complex }
     eDrude = { CNM:eDrudeCNM, PDR:eDrudePDR }

  ENDIF

  ;; Restore debugging options.
  !Except = except

  RETURN, { CNM:drudeCNM, PDR:drudePDR }

END ;; -----------------------------------------------------------------------
