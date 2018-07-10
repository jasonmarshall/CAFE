;+ ===========================================================================
; NAME:
;       Jam_PAHRatios
;
; PURPOSE:
;
;       This function returns the 6.2/7.7 and 11.3/7.7 power ratios of PAH
;       features in PDRs and the CNM.
;
; CATEGORY:
;       Astrophysics
;
; CALLING SEQUENCE:
;       Result = Jam_PAHRatios(LogN_CNM, LogN_PDR)
;
; INPUTS:
;
;       N_CNM - A scalar or vector giving the number of carbon atoms in the
;               cold-neutral-medium (CNM) PAH molecule(s).
;
;       N_PDR - A scalar or vector giving the number of carbon atoms in the
;               photodissociation region (PDR) PAH molecule(s).
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
; Copyright (C) 2006, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ***************************************************************************

FUNCTION Jam_PAHRatios, N_CNM, N_PDR, E_N_CNM=E_N_CNM, E_N_PDR=E_N_PDR, $
                        ERatios=eRatios, RatioTable=ratioTable, Debug=debug

  ;; Set compiler options and error handling.
  Compile_Opt IDL2
  IF (~Keyword_Set(debug)) THEN On_Error, 2

  ;; Set debugging options.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  ;; Check input parameters.
  IF (N_Params() LT 2) THEN $
     Message, Jam_Message('Syntax', Jam_Syntax('Jam_PAHRatios', /Is_Function))

  ;; Read the PAH ratio table.
  IF (N_Elements(ratioTable) EQ 0) THEN BEGIN
     file = 'pah_ratios.txt'
     path = FilePath(file, Root_Dir=!Jam.Path.Tables)
     ratioTable = Jam_ReadData(path)
  ENDIF

  ;; Interpolate to find the PAH power ratios at each input N_C.
  CNM_6_7 = Interpol(ratioTable.P6_P7_CNM, ratioTable.N_C, N_CNM)
  CNM_11_7 = Interpol(ratioTable.P11_P7_CNM, ratioTable.N_C, N_CNM)
  PDR_6_7 = Interpol(ratioTable.P6_P7_PDR, ratioTable.N_C, N_PDR)
  PDR_11_7 = Interpol(ratioTable.P11_P7_PDR, ratioTable.N_C, N_PDR)

  ;; Calculate PAH power ratio errors.
  IF (Arg_Present(eRatios)) THEN BEGIN
     eCNM_6_7 = 0.D
     eCNM_11_7 = 0.D
     ePDR_6_7 = 0.D
     ePDR_11_7 = 0.D
     EPS = 1D-5
     N_CNM_Lo = N_CNM * (1.D - EPS)
     N_CNM_Hi = N_CNM * (1.D + EPS)
     N_PDR_Lo = N_PDR * (1.D - EPS)
     N_PDR_Hi = N_PDR * (1.D + EPS)
     dN_CNM = N_CNM_Hi - N_CNM_Lo
     dN_PDR = N_PDR_Hi - N_PDR_Lo
     CNM_6_7_Lo = Interpol(ratioTable.P6_P7_CNM, ratioTable.N_C, N_CNM_Lo)
     CNM_11_7_Lo = Interpol(ratioTable.P11_P7_CNM, ratioTable.N_C, N_CNM_Lo)
     PDR_6_7_Lo = Interpol(ratioTable.P6_P7_PDR, ratioTable.N_C, N_PDR_Lo)
     PDR_11_7_Lo = Interpol(ratioTable.P11_P7_PDR, ratioTable.N_C, N_PDR_Lo)
     CNM_6_7_Hi = Interpol(ratioTable.P6_P7_CNM, ratioTable.N_C, N_CNM_Hi)
     CNM_11_7_Hi = Interpol(ratioTable.P11_P7_CNM, ratioTable.N_C, N_CNM_Hi)
     PDR_6_7_Hi = Interpol(ratioTable.P6_P7_PDR, ratioTable.N_C, N_PDR_Hi)
     PDR_11_7_Hi = Interpol(ratioTable.P11_P7_PDR, ratioTable.N_C, N_PDR_Hi)
     dCNM_6_7 = Abs((CNM_6_7_Hi - CNM_6_7_Lo) / dN_CNM)
     dCNM_11_7 = Abs((CNM_11_7_Hi - CNM_11_7_Lo) / dN_CNM)
     dPDR_6_7 = Abs((PDR_6_7_Hi - PDR_6_7_Lo) / dN_PDR)
     dPDR_11_7 = Abs((PDR_11_7_Hi - PDR_11_7_Lo) / dN_PDR)
     IF (N_Elements(E_N_CNM) NE 0) THEN BEGIN
        eCNM_6_7 = dCNM_6_7 * E_N_CNM
        eCNM_11_7 = dCNM_11_7 * E_N_CNM
     ENDIF ELSE $
        E_N_CNM = 0.D
     IF (N_Elements(E_N_PDR) NE 0) THEN BEGIN
        ePDR_6_7 = dPDR_6_7 * E_N_PDR
        ePDR_11_7 = dPDR_11_7 * E_N_PDR
     ENDIF ELSE $
        E_N_PDR = 0.D
     eRatios = { CNM:{ N_C:N_CNM, E_N_C:E_N_CNM, $
                       P6_P7:eCNM_6_7, P11_P7:eCNM_11_7 }, $
                 PDR:{ N_C:N_PDR, E_N_C:E_N_PDR, $
                       P6_P7:ePDR_6_7, P11_P7:ePDR_11_7 } }
  ENDIF

  ;; Restore debugging options.
  !Except = except

  RETURN, { CNM:{ N_C:N_CNM, P6_P7:CNM_6_7, P11_P7:CNM_11_7 }, $
            PDR:{ N_C:N_PDR, P6_P7:PDR_6_7, P11_P7:PDR_11_7 } }

END ;; -----------------------------------------------------------------------
