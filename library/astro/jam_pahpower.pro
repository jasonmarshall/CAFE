;+ ===========================================================================
; NAME:
;       Jam_PAHPower
;
; PURPOSE:
;       This function calculates the power of the PAH features in the two 
;       component (CNM + PDR) emission model.
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_PAHPower(Drude)
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
; Copyright (C) 2006, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ***************************************************************************

FUNCTION Jam_PAHPower, drude, EDrude=eDrude, Covar=covar, EPower=ePower, $
                       CPower=cPower, ECPower=eCPower, Ratio=ratio, $
                       ERatio=eRatio, Debug=debug

  ;; Set compiler options and error handling.
  Compile_Opt IDL2
  IF (~Keyword_Set(debug)) THEN On_Error, 2

  ;; Set debugging options.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  ;; Check for minimum input parameters.
  nParams = N_Params()
  IF (nParams LT 1) THEN $
     Message, Jam_Message('Syntax', Jam_Syntax('Jam_PAHFlux', /Is_FUNCTION))

  ;; Calculate power and power error.
  drudeCNM = drude.CNM
  drudePDR = drude.PDR
  eDrudeCNM = eDrude.CNM
  eDrudePDR = eDrude.PDR
  covarCNM = covar.CNM
  covarPDR = covar.PDR
  IF ((Arg_Present(ePower)) OR (Arg_Present(eRatio))) THEN BEGIN
     powerCNM = Jam_DrudePower(drudeCNM, EDrude=eDrudeCNM, Covar=covarCNM, $
                               EPower=ePowerCNM)
     powerPDR = Jam_DrudePower(drudePDR, EDrude=eDrudePDR, Covar=covarPDR, $
                               EPower=ePowerPDR)
     ePower = { CNM:ePowerCNM, PDR:ePowerPDR, $
                TOT:Sqrt(ePowerCNM^2 + ePowerPDR^2) }
  ENDIF $
  ELSE BEGIN
     powerCNM = Jam_GaussianPower(drudeCNM)
     powerPDR = Jam_GaussianPower(drudePDR)
  ENDELSE
  power = { CNM:powerCNM, PDR:powerPDR, TOT:(powerCNM + powerPDR) }

  ;; Calculate power and power errors of PAH complexes.
  IF ((Arg_Present(eCPower)) OR (Arg_Present(eRatio))) THEN BEGIN
     powerCNM7 = Jam_DrudePower(drudeCNM, EDrude=eDrudeCNM, Covar=covarCNM, $
                                EPower=ePowerCNM7, Lines=[5,6,7])
     powerPDR7 = Jam_DrudePower(drudePDR, EDrude=eDrudePDR, Covar=covarPDR, $
                                EPower=ePowerPDR7, Lines=[5,6,7])
     powerCNM11 = Jam_DrudePower(drudeCNM, EDrude=eDrudeCNM, Covar=covarCNM, $
                                 EPower=ePowerCNM11, Lines=[11,12])
     powerPDR11 = Jam_DrudePower(drudePDR, EDrude=eDrudePDR, Covar=covarPDR, $
                                 EPower=ePowerPDR11, Lines=[11,12])
     powerCNM12 = Jam_DrudePower(drudeCNM, EDrude=eDrudeCNM, Covar=covarCNM, $
                                 EPower=ePowerCNM12, Lines=[14,15])
     powerPDR12 = Jam_DrudePower(drudePDR, EDrude=eDrudePDR, Covar=covarPDR, $
                                 EPower=ePowerPDR12, Lines=[14,15])
     powerCNM17 = Jam_DrudePower(drudeCNM, EDrude=eDrudeCNM, Covar=covarCNM, $
                                 EPower=ePowerCNM17, Lines=[20,21,22,23])
     powerPDR17 = Jam_DrudePower(drudePDR, EDrude=eDrudePDR, Covar=covarPDR, $
                                 EPower=ePowerPDR17, Lines=[20,21,22,23])
     cPowerCNM = { PAH7:powerCNM7, PAH11:powerCNM11, $
                   PAH12:powerCNM12, PAH17:powerCNM17 }
     cPowerPDR = { PAH7:powerPDR7, PAH11:powerPDR11, $
                   PAH12:powerPDR12, PAH17:powerPDR17 }
     cPower = { CNM:cPowerCNM, PDR:cPowerPDR }
     eCPowerCNM = { PAH7:ePowerCNM7, PAH11:ePowerCNM11, $
                    PAH12:ePowerCNM12, PAH17:ePowerCNM17 }
     eCPowerPDR = { PAH7:ePowerPDR7, PAH11:ePowerPDR11, $
                    PAH12:ePowerPDR12, PAH17:ePowerPDR17 }
     eCPower = { CNM:eCPowerCNM, PDR:eCPowerPDR }
  ENDIF $
  ELSE BEGIN
     IF ((Arg_Present(cPower)) OR (Arg_Present(ratio))) THEN BEGIN
        powerCNM7 = Jam_DrudePower(drudeCNM, Lines=[5,6,7])
        powerPDR7 = Jam_DrudePower(drudePDR, Lines=[5,6,7])
        powerCNM11 = Jam_DrudePower(drudeCNM, Lines=[11,12])
        powerPDR11 = Jam_DrudePower(drudePDR, Lines=[11,12])
        powerCNM12 = Jam_DrudePower(drudeCNM, Lines=[14,15])
        powerPDR12 = Jam_DrudePower(drudePDR, Lines=[14,15])
        powerCNM17 = Jam_DrudePower(drudeCNM, Lines=[20,21,22,23])
        powerPDR17 = Jam_DrudePower(drudePDR, Lines=[20,21,22,23])
        cPowerCNM = { PAH7:powerCNM7, PAH11:powerCNM11, $
                      PAH12:powerCNM12, PAH17:powerCNM17 }
        cPowerPDR = { PAH7:powerPDR7, PAH11:powerPDR11, $
                      PAH12:powerPDR12, PAH17:powerPDR17 }
        cPower = { CNM:cPowerCNM, PDR:cPowerPDR }
     ENDIF
  ENDELSE

  ;; Calculate PAH ratios.
  IF (Arg_Present(ratio)) THEN BEGIN
     ratioCNM6 = powerCNM[3] / powerCNM7
     ratioPDR6 = powerPDR[3] / powerPDR7
     ratioCNM11 = powerCNM11 / powerCNM7
     ratioPDR11 = powerPDR11 / powerPDR7
     ratioCNM17 = powerCNM17 / powerCNM7
     ratioPDR17 = powerPDR17 / powerPDR7
     ratioCNM = { PAH6:ratioCNM6, PAH11:ratioCNM11, PAH17:ratioCNM17 }
     ratioPDR = { PAH6:ratioPDR6, PAH11:ratioPDR11, PAH17:ratioPDR17 }
     ratio = { CNM:ratioCNM, PDR:ratioPDR }
  ENDIF
  IF (Arg_Present(eRatio)) THEN BEGIN
     eRatioCNM6 = Sqrt((ePowerCNM[3] / powerCNM7)^2 + $
                       (powerCNM[3] / powerCNM7^2 * ePowerCNM7)^2)
     eRatioPDR6 = Sqrt((ePowerPDR[3] / powerPDR7)^2 + $
                       (powerPDR[3] / powerPDR7^2 * ePowerPDR7)^2)
     eRatioCNM11 = Sqrt((ePowerCNM11 / powerCNM7)^2 + $
                        (powerCNM11 / powerCNM7^2 * ePowerCNM7)^2)
     eRatioPDR11 = Sqrt((ePowerPDR11 / powerPDR7)^2 + $
                        (powerPDR11 / powerPDR7^2 * ePowerPDR7)^2)
     eRatioCNM17 = Sqrt((ePowerCNM17 / powerCNM7)^2 + $
                        (powerCNM17 / powerCNM7^2 * ePowerCNM7)^2)
     eRatioPDR17 = Sqrt((ePowerPDR17 / powerPDR7)^2 + $
                        (powerPDR17 / powerPDR7^2 * ePowerPDR7)^2)
     eRatioCNM = { PAH6:eRatioCNM6, PAH11:eRatioCNM11, PAH17:eRatioCNM17 }
     eRatioPDR = { PAH6:eRatioPDR6, PAH11:eRatioPDR11, PAH17:eRatioPDR17 }
     eRatio = { CNM:eRatioCNM, PDR:eRatioPDR }
  ENDIF

  ;; Restore debugging options.
  !Except = except

  RETURN, power

END ;; -----------------------------------------------------------------------
