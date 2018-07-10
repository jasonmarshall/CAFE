;+
; NAME:
;     JAM_TABLES
;
; PURPOSE:
;     This procedure exists so that the Jam/Tables directory is added to the
;     IDL system variable !PATH when the Jam library is searched using
;     EXPAND_PATH, so that table files may be searched for with FILE_WHICH.
;-

PRO Jam_Tables

  Compile_Opt IDL2, Hidden
  On_Error, 2

  RETURN

END
