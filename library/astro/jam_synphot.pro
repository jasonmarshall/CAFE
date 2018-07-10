;+ ===========================================================================
; NAME:
;       Jam_SynPhot
;
; PURPOSE:
;       This function calculate synthetic aperture photometry for a number
;       of near- to far-infrared bands.
;
; CATEGORY:
;       Astrophysics.
;
; CALLING SEQUENCE:
;       Result = Jam_SynPhot()
;
; AUTHOR:
;       Jason A. Marshall
;       Department of Astronomy, Cornell University, Ithaca, NY 14853
;       jam258@cornell.edu
;
; MODIFICATION HISTORY:
;       Written by: Jason A. Marshall, 13 Jul 2006.
;-
;; ---------------------------------------------------------------------------
; Copyright (C) 2006, 2007, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ---------------------------------------------------------------------------

FUNCTION JAM_SynPhot, wave, _flux, _eFlux, z=z, Filters=filters, $
                      FilterPath=filterPath, FilterList=filterList, $
                      Initialize=initialize, Debug=debug

  ;; Set compiler options and error handling.
  Compile_Opt IDL2
  IF (~Keyword_Set(debug)) THEN On_Error, 2

  ;; Set debugging options.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  ;; Get filter list.
  readFilters = (N_Elements(filters) EQ 0)
  IF (readFilters) THEN BEGIN
     IF (N_Elements(filterPath) EQ 0) THEN $
        filterPath = !JAM.Path.Tables
     IF (N_Elements(filterList) EQ 0) THEN BEGIN
        filterFiles = File_Search(filterPath, 'filter.*.txt')
     ENDIF $
     ELSE BEGIN
        N_Filter = N_Elements(filterList)
        FOR i=0,N_Filter-1 DO BEGIN
           thisFilter = File_Search(filterPath, $
                                    'filter.' + filterList[i] + '.txt')
           filterFiles = (N_Elements(filterFiles) EQ 0) ? $
                         thisFilter : [filterFiles, thisFilter]
        ENDFOR
     ENDELSE
  ENDIF
  N_Filter = (readFilters) ? N_Elements(filterFiles) : N_Tags(filters)

  ;; Get filter names.
  IF (readFilters) THEN BEGIN
     filterNames = File_BaseName(filterFiles, '.txt')
     filterNames = StrMid(filterNames, 7)
  ENDIF ELSE $
     filterNames = Tag_Names(filters)

  ;; Create filters structure.
  IF (readFilters) THEN BEGIN
     FOR i=0,N_Filter-1 DO BEGIN
        _filter = Jam_ReadData(filterFiles[i])
        filters = (N_Elements(filters) EQ 0) ? $
                  Create_Struct(filterNames[i], _filter) : $
                  Create_Struct(filters, filterNames[i], _filter)
     ENDFOR
  ENDIF

  ;; Return filters structure if initializing.
  IF (Keyword_Set(initialize)) THEN RETURN, filters

  ;; Convert flux f_Jy -> f_um.
  flux = 3D14 / wave^2 * _flux
  eFlux = (N_Elements(_eFlux) EQ 0) ? 0.D * wave : 3D14 / wave^2 * _eFlux

  ;; Set default redshift for filter...this can be used to "scale-down" a filter
  ;; to get photometry for a rest-frame source at some redshift.
  IF (N_Elements(z) EQ 0) THEN z = 0.D

  ;; Loop over each filter.
  logWave = ALog(wave)
  FOR i=0,N_Filter-1 DO BEGIN
     data = filters.(i)
     wave0 = data.Wave / (1.D + z)
     logWave0 = ALog(wave0)
     flux0 = Interpol(flux, logWave, logWave0)
     eFlux0 = Interpol(eFlux, logWave, logWave0)
     T = data.T / Int_Tabulated(logWave0, wave0 * data.T)
     fluxTot = Int_Tabulated(logWave0, wave0 * T * flux0)
     eFluxTot = Int_Tabulated(logWave0, wave0 * T * eFlux0)
     waveTot = Int_Tabulated(logWave0, wave0 * T * wave0)
     widthTot = 1.D / Max(T)
     scale = waveTot^2 / 3D14
     fluxTot *= scale
     eFluxTot *= scale
     CASE (filterNames[i]) OF
        'J': fluxZero = 1594.D
        'H': fluxZero = 1024.D
        'K': fluxZero = 666.7D
        'L': fluxZero = 278.D
        'M': fluxZero = 155.D
        ELSE: fluxZero = 1.D
     ENDCASE
     magTot = -2.5D * ALog10(fluxTot / fluxZero)
     eMagTot = -2.5D / ALog(10.D) * eFluxTot / fluxTot
     waveSyn = (N_Elements(waveSyn) EQ 0) ? $
               Create_Struct(filterNames[i], waveTot) : $
               Create_Struct(waveSyn, filterNames[i], waveTot)
     widthSyn = (N_Elements(widthSyn) EQ 0) ? $
                Create_Struct(filterNames[i], widthTot) : $
                Create_Struct(widthSyn, filterNames[i], widthTot)
     fluxSyn = (N_Elements(fluxSyn) EQ 0) ? $
               Create_Struct(filterNames[i], fluxTot) : $
               Create_Struct(fluxSyn, filterNames[i], fluxTot)
     eFluxSyn = (N_Elements(eFluxSyn) EQ 0) ? $
                Create_Struct(filterNames[i], eFluxTot) : $
                Create_Struct(eFluxSyn, filterNames[i], eFluxTot)
     magSyn = (N_Elements(magSyn) EQ 0) ? $
              Create_Struct(filterNames[i], magTot) : $
              Create_Struct(magSyn, filterNames[i], magTot)
     eMagSyn = (N_Elements(eMagSyn) EQ 0) ? $
               Create_Struct(filterNames[i], eMagTot) : $
               Create_Struct(eMagSyn, filterNames[i], eMagTot)
  ENDFOR

  ;; Restore debugging options.
  !Except = except

  RETURN, { Wave:waveSyn, $
            Width:widthSyn, $
            Flux:fluxSyn, $
            EFlux:eFluxSyn, $
            Mag:magSyn, $
            EMag:eMagSyn }

END
