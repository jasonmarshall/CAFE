;+ ===========================================================================
; NAME:
; Jam_GrainRefractiveIndex
;
; PURPOSE: 
; This function tabulates indices of refraction (n and k) for silicate and
; carbonaceous dust grains as a function of wavelength.
;
; CATEGORY:
; Astronomy, Physics.
;
; CALLING SEQUENCE:
; Jam_GrainRefractionIndex
;
; OUTPUTS:
; This function creates text files for silicate and carbonaceous grains in
; the !Jam.Path.Tables directory.
;
; REQUIREMENTS:
;
; PROCEDURE:
;
; REFERENCES:
;
; EXAMPLE:
; To tabulate indices of refraction for astronomical dust enter:
;   IDL> Jam_GrainRefractionIndex
;
; AUTHOR:
; Jason A. Marshall
; Department of Astronomy
; Cornell University
; Ithaca, NY 14853
; jam258@cornell.edu
;
; MODIFICATION HISTORY:
; 18 Sep 06 - JAM - Initial version.
; 24 Sep 06 - JAM - Modified to produce separate 0.01 and 0.1 um Gra files.
;-
;; ***************************************************************************
; Copyright (C) 2006, Jason A. Marshall
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or un-
; modified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;; ***************************************************************************

PRO Jam_GrainRefractiveIndex, Debug=debug

  Compile_Opt IDL2
  On_Error, 2

  ;; Setup debugging.
  IF (Keyword_Set(debug)) THEN BEGIN
    except = !Except
    !Except = 2
  ENDIF

  ;; Get the path names for the various files from Draine.
  root = !Jam.Path.Tables
  path_Sil = FilePath('draine_nk.sil.txt', Root_Dir=root)
  path_Carb_01_par = FilePath('draine_nk.carb-0.01-par.txt', Root_Dir=root)
  path_Carb_10_par = FilePath('draine_nk.carb-0.10-par.txt', Root_Dir=root)
  path_Carb_01_perp = FilePath('draine_nk.carb-0.01-perp.txt', Root_Dir=root)
  path_Carb_10_perp = FilePath('draine_nk.carb-0.10-perp.txt', Root_Dir=root)

  ;; Read various files.
  data_Sil = Jam_ReadArray(path_Sil, Header_Lines=5)
  data_Carb_01_par = Jam_ReadArray(path_Carb_01_par, Header_Lines=5)
  data_Carb_10_par = Jam_ReadArray(path_Carb_10_par, Header_Lines=5)
  data_Carb_01_perp = Jam_ReadArray(path_Carb_01_perp, Header_Lines=5)
  data_Carb_10_perp = Jam_ReadArray(path_Carb_10_perp, Header_Lines=5)

  ;; Calculate n and k.
  wave_Sil = data_Sil[*,0]
  wave_Carb_par = data_Carb_01_par[*,0]
  wave_Carb_perp = data_Carb_01_perp[*,0]
  n_Sil = 1.D + data_Sil[*,3]
  n_Carb_01_par = 1.D + data_Carb_01_par[*,3]
  n_Carb_10_par = 1.D + data_Carb_10_par[*,3]
  n_Carb_01_perp = 1.D + data_Carb_01_perp[*,3]
  n_Carb_10_perp = 1.D + data_Carb_10_perp[*,3]
  k_Sil = data_Sil[*,4]
  k_Carb_01_par = data_Carb_01_par[*,4]
  k_Carb_10_par = data_Carb_10_par[*,4]
  k_Carb_01_perp = data_Carb_01_perp[*,4]
  k_Carb_10_perp = data_Carb_10_perp[*,4]

  ;; Average par and perp for two grain-sizes.
  n_Carb_par = 0.5D * (n_Carb_01_par + n_Carb_10_par)
  n_Carb_perp = 0.5D * (n_Carb_01_perp + n_Carb_10_perp)
  k_Carb_par = 0.5D * (k_Carb_01_par + k_Carb_10_par)
  k_Carb_perp = 0.5D * (k_Carb_01_perp + k_Carb_10_perp)

  ;; Create output filenames.
  root = !Astro
  path_Sil = FilePath('sil-draine.nk', Root_Dir=root)
  path_Carb_01_par = FilePath('gra-par-0.01-draine.nk', Root_Dir=root)
  path_Carb_10_par = FilePath('gra-par-0.10-draine.nk', Root_Dir=root)
  path_Carb_01_perp = FilePath('gra-perp-0.01-draine.nk', Root_Dir=root)
  path_Carb_10_perp = FilePath('gra-perp-0.10-draine.nk', Root_Dir=root)
  path_Carb_par = FilePath('gra-par-draine.nk', Root_Dir=root)
  path_Carb_perp = FilePath('gra-perp-draine.nk', Root_Dir=root)

  ;; Create data structures.
  data_Sil = { N_Wave:N_Elements(wave_Sil), $
               Min_Wave:Min(wave_Sil), $
               Max_Wave:Max(wave_Sil), $
               Wave:wave_Sil, $
               n:n_Sil, $
               k:k_Sil }
  data_Carb_01_par = { N_Wave:N_Elements(wave_Carb_par), $
                       Min_Wave:Min(wave_Carb_par), $
                       Max_Wave:Max(wave_Carb_par), $
                       Wave:wave_Carb_par, $
                       n:n_Carb_01_par, $
                       k:k_Carb_01_par }
  data_Carb_10_par = { N_Wave:N_Elements(wave_Carb_par), $
                       Min_Wave:Min(wave_Carb_par), $
                       Max_Wave:Max(wave_Carb_par), $
                       Wave:wave_Carb_par, $
                       n:n_Carb_10_par, $
                       k:k_Carb_10_par }
  data_Carb_01_perp = { N_Wave:N_Elements(wave_Carb_perp), $
                        Min_Wave:Min(wave_Carb_perp), $
                        Max_Wave:Max(wave_Carb_perp), $
                        Wave:wave_Carb_perp, $
                        n:n_Carb_01_perp, $
                        k:k_Carb_01_perp }
  data_Carb_10_perp = { N_Wave:N_Elements(wave_Carb_perp), $
                        Min_Wave:Min(wave_Carb_perp), $
                        Max_Wave:Max(wave_Carb_perp), $
                        Wave:wave_Carb_perp, $
                        n:n_Carb_10_perp, $
                        k:k_Carb_10_perp }
  data_Carb_par = { N_Wave:N_Elements(wave_Carb_par), $
                    Min_Wave:Min(wave_Carb_par), $
                    Max_Wave:Max(wave_Carb_par), $
                    Wave:wave_Carb_par, $
                    n:n_Carb_par, $
                    k:k_Carb_par }
  data_Carb_perp = { N_Wave:N_Elements(wave_Carb_perp), $
                     Min_Wave:Min(wave_Carb_perp), $
                     Max_Wave:Max(wave_Carb_perp), $
                     Wave:wave_Carb_perp, $
                     n:n_Carb_perp, $
                     k:k_Carb_perp }

  ;; Write files.
  Jam_WriteData, data_Sil, FileName=path_Sil
  Jam_WriteData, data_Carb_01_par, FileName=path_Carb_01_par
  Jam_WriteData, data_Carb_10_par, FileName=path_Carb_10_par
  Jam_WriteData, data_Carb_01_perp, FileName=path_Carb_01_perp
  Jam_WriteData, data_Carb_10_perp, FileName=path_Carb_10_perp
  Jam_WriteData, data_Carb_par, FileName=path_Carb_par
  Jam_WriteData, data_Carb_perp, FileName=path_Carb_perp

  IF (Keyword_Set(debug)) THEN !Except = except

END ;; -----------------------------------------------------------------------
