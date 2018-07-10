;+ ====================================================================
; NAME:
; SED_IMPORT
;
; PURPOSE:
; This procedure imports redshift, wavelength and flux data and
; outputs an SED structure as an XDR file.
;
; CATEGORY:
; Spitzer/IRS, Astronomy.
;
; CALLING SEQUENCE:
; SED_IMPORT, Object, z, Wave, Flux, [EFlux]
;
; INPUTS:
; Object: Set equal to a scalar string containing the object name.
; z: Set equal to a scalar containing the redshift of the object.
; Wave: Set equal to a vector of observed wavelengths in microns.
; Flux: Set equal to a vector of observed flux values in Jy.
;
; OPTIONAL INPUTS:
; EFlux: Set equal to a vector of observed flux error values in Jy.
;
; KEYWORD PARAMETERS:
; PATH: Set this keyword equal to a scalar string containing the
;   path to write the file to. The default path is the current
;   working directory.
;
;   *** NOTE ***
;   Any other variable passed to the procedure as a keyword will be
;   included as an element of the SED structure.
;
; REQUIREMENTS:
; <JAM> - JAM_CHECK
;
; EXAMPLE:
; To create an SED file in the current working directory enter:
;   IDL> wave = ... (enter wavelength vector)
;   IDL> flux = ... (enter flux vector)
;   IDL> SED_Import, 'SourceName', 0, wave, flux
;
; AUTHOR:
; Jason A. Marshall, Astronomy Dept., Cornell Univ., Ithaca, NY 14853
; jam258@cornell.edu
;
; MODIFICATION HISTORY:
; 2004 Dec 13 - JAM - Initial version.
;-
;; ....................................................................
; Copyright (C) 2005, 2006, Jason A. Marshall
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or un-
; modified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;; ....................................................................

PRO SED_Import, object, $
                z, $
                wave, $
                flux, $
                eFlux, $
                Nod1Flux=nod1Flux, $
                Nod2Flux=nod2Flux, $
                Path=path, $
                _Extra=extra, $
                Debug=debug

  Compile_Opt IDL2
  On_Error, 2

  ;; Setup debugging.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  ;; Check syntax.
  IF (N_Params() LT 4) THEN Message, 'Incorrect calling syntax.'

  ;; Check input parameters.
  Jam_Check, object, 'Object', TName='STRING', N_Dim=0
  Jam_Check, z, 'z', TName='NUM', N_Dim=0
  Jam_Check, wave, 'Wave', TName='NUM', N_Dim=1
  N_WAVE = N_Elements(wave)
  Jam_Check, flux, 'Flux', TName='NUM', N_Dim=1, N_Elements=N_WAVE

  ;; Check flux errors vector.
  IF (N_Elements(eFlux) NE 0) THEN $
     Jam_Check, eFlux, 'EFlux', TName='NUM', N_Dim=1, N_Elements=N_WAVE

  ;; Convert to rest frame.
  _wave = wave / (1.D + z)
  _flux = flux / (1.D + z)

  ;; Calculate default flux errors.
  IF (N_Elements(eFlux) EQ 0) THEN BEGIN
     IF ((N_Elements(nod1Flux) EQ 0) OR (N_Elements(nod2Flux) EQ 0)) THEN BEGIN
        _eFlux = 0.05D * _flux
     ENDIF ELSE $
        _eFlux = EFluxIRS(wave, nod1Flux, nod2Flux) / (1.D + z)
  ENDIF ELSE $
     _eFlux = eFlux / (1.D + z)

  ;; Create default nod1 and nod2 flux density vectors.
  _nod1Flux = (N_Elements(nod1Flux) EQ 0) ? $
              _flux + 0.5D * _eFlux : nod1Flux / (1.D + z)
  _nod2Flux = (N_Elements(nod2Flux) EQ 0) ? $
              _flux - 0.5D * _eFlux : nod2Flux / (1.D + z)

  ;; Create the SED structure.
  SED = { $
        Object:object, $
        CZ:(3D5 * z), $
        Frame:'Rest', $
        Wave:_wave, $
        Flux:{ Jy:_flux }, $
        Nod1Flux:{ Jy:_nod1Flux }, $
        Nod2Flux:{ Jy:_nod2Flux }, $
        EFlux:{ Jy:_eFlux } $
        }

  ;; Add EXTRA tags to SED structure.
  IF (N_Elements(extra) NE 0) THEN BEGIN
     tags = Tag_Names(extra)
     FOR i=0,N_Elements(tags)-1 DO $
        SED = Create_Struct(SED, tags[i], extra.(i))
  ENDIF

  ;; Create the name for the file.
  CD, Current=dir
  IF (N_Elements(path) EQ 0) THEN path = dir
  name = object + '.irslow.xdr'
  fileXDR = FilePath(name, Root_Dir=path)

  ;; Save the SED structure as an XDR file.
  Save, SED, FileName=fileXDR

  !Except = except
  RETURN

END
