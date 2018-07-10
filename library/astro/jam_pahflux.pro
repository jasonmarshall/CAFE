;+ ===========================================================================
; NAME:
;       Jam_PAHFlux
;
; PURPOSE:
;       This function calculates the total PAH flux from the two component
;       (CNM + PDR) emission model.
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_PAHFlux(FracPDR[, N_CNM, N_PDR])
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
; Copyright (C) 2006, 2007, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ***************************************************************************

FUNCTION Jam_PAHFlux, wave, fracPDR, N_CNM, N_PDR, Drude=drude, $
                      ExtPDR=extPDR, EFracPDR=eFracPDR, E_N_CNM=e_N_CNM, $
                      E_N_PDR=e_N_PDR, EDrude=eDrude, EExtPDR=eExtPDR, $
                      Covar=covar, EFlux=eFlux, FCom=fCom, EFCom=eFCom, $
                      Template=template, RatioTable=ratioTable, Lines=lines, $
                      Debug=debug

  ;; Set compiler options and error handling.
  Compile_Opt IDL2
  IF (~Keyword_Set(debug)) THEN On_Error, 2

  ;; Set debugging options.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  ;; Check for minimum input parameters.
  nParams = N_Params()
  IF (nParams LT 2) THEN $
     Message, Jam_Message('Syntax', Jam_Syntax('Jam_PAHFlux', /Is_FUNCTION))

  ;; Read PAH template file.
  IF (N_Elements(template) EQ 0) THEN BEGIN
     path = FilePath('pah_template.txt', Root_Dir=!Jam.Path.Tables)
     template = Jam_ReadData(path)
  ENDIF

  ;; Calculate Drude structures.
  IF (N_Elements(drude) EQ 0) THEN BEGIN
     IF (Arg_Present(eFlux)) THEN BEGIN
        drude = Jam_PAHDrude(N_CNM, N_PDR, E_N_CNM=e_N_CNM, E_N_PDR=e_N_PDR, $
                             EDrude=eDrude, Template=template, $
                             RatioTable=ratioTable, Debug=debug)
     ENDIF $
     ELSE BEGIN
        drude = Jam_PAHDrude(N_CNM, N_PDR, Template=template, $
                             RatioTable=ratioTable, Debug=debug)
     ENDELSE
  ENDIF

  ;; Calculate CNM and PDR flux vectors.
  IF ((Arg_Present(eFlux)) AND $
      ((N_Elements(eDrude) NE 0) OR (N_Elements(covar) NE 0))) THEN BEGIN
     IF (N_Elements(eDrude) NE 0) THEN BEGIN
        eDrudeCNM = eDrude.CNM
        eDrudePDR = eDrude.PDR
     ENDIF
     IF (N_Elements(covar) EQ 0) THEN BEGIN
        N_P = N_Elements(eDrudeCNM)
        covar = { CNM:DblArr(N_P, N_P), PDR:DblArr(N_P, N_P) }
     ENDIF
     fluxCNM = Jam_DrudeFlux(wave, drude.CNM, EDrude=eDrudeCNM, $
                             Covar=covar.CNM, EFlux=eFluxCNM, Lines=lines)
     fluxPDR = Jam_DrudeFlux(wave, drude.PDR, EDrude=eDrudePDR, $
                             Covar=covar.PDR, EFlux=eFluxPDR, Lines=lines)
  ENDIF $
  ELSE BEGIN
     fluxCNM = Jam_DrudeFlux(wave, drude.CNM, Lines=lines)
     fluxPDR = Jam_DrudeFlux(wave, drude.PDR, Lines=lines)
     eFluxCNM = DblArr(N_Elements(fluxCNM))
     eFluxPDR = eFluxCNM
  ENDELSE

  ;; Calculate total flux.
  IF (N_Elements(extPDR) EQ 0) THEN extPDR = 1.D
  fCNM = (1.D - fracPDR) * fluxCNM
  fPDR0 = fracPDR * fluxPDR
  fPDR = extPDR * fPDR0
  flux = fCNM + fPDR
  fCom = { CNM:fCNM, PDR:fPDR, PDR0:fPDR0 }

  ;; Calculate total flux error.
  IF ((Arg_Present(eFlux)) OR (Arg_Present(eFCom))) THEN BEGIN
     IF (N_Elements(eFracPDR) EQ 0) THEN eFracPDR = 0.D
     IF (N_Elements(eExtPDR) EQ 0) THEN eExtPDR = 0.D
     eFluxCNM2 = (eFracPDR * fluxCNM)^2 + ((1.D - fracPDR) * eFluxCNM)^2
     eFluxPDR2 = (eFracPDR * extPDR * fluxPDR)^2 + $
                 (fracPDR * eExtPDR * fluxPDR)^2 + $
                 (fracPDR * extPDR * eFluxPDR)^2
     eFlux = Sqrt(eFluxCNM2 + eFluxPDR2)
     eFCom = { CNM:Sqrt(eFluxCNM2), PDR:Sqrt(eFluxPDR2), $
               PDR0:Sqrt((fracPDR * eFluxPDR)^2 + (eFracPDR * fluxPDR)^2) }
  ENDIF

  ;; Restore debugging options.
  !Except = except

  RETURN, flux

END ;; -----------------------------------------------------------------------
