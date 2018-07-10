;+ ===========================================================================
; NAME:
;       JAM_INIT
;
; PURPOSE:
;
;       This routine initializes the JAM library. It must be called before
;       any JAM library procedures or functions are used, and is therefore
;       best called from the IDL startup file.
;
; CALLING SEQUENCE:
;
;       JAM_Init
;
; KEYWORD PARAMETERS:
;
;       BROWSER - Set this keyword to a scalar string containing the command
;          to open a browser from a terminal window in your particular OS.
;          For example, in Mac OS X 'open -a safari' will launch Safari.
;
; PROCEDURE:
;
;       The non-standard system variable '!JAM' is added using DEFSYSV.
;       This system variable contains the following structure:
;
;       !JAM =|
;             |- LogMin: Natural log of minimum double-precision number.
;             |- LogMax: Natural log of maximum double-precesion number.
;             |- Log10Min: Base-10 log of minimum double-precision number.
;             |- Log10Max: Base-10 log of maximum double-precision number.
;             |- Browser: Command used to launch a web browser.
;             |- Path: Structure for paths.
;
; AUTHOR:
;
;       Jason A. Marshall
;       jason.a.marshall@gmail.com
;       http://jasonmarshall.org
;
; MODIFICATION HISTORY:
;
;       Written by: Jason Marshall on Oct 01, 2005.
;-
;; ...........................................................................
; Copyright (C) 2005, Jason Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ...........................................................................

PRO JAM, Browser=browser

   COMPILE_OPT IDL2, Hidden
   ON_ERROR, 2

   ;; Define system variable structure for JAM library (if it doesn't exist).
   pathJAM = File_DirName(File_Which('jam.pro'))
   DEFSYSV, '!JAM', Exist=exist
   IF (~exist) THEN BEGIN

      ;; Initialize browser invocation string.
      IF (N_Elements(browser) NE 0) THEN BEGIN
         IF ((Size(browser, /Dimensions) GT 1) OR $
             (Size(browser, /TName) NE 'STRING')) THEN BEGIN
             Message, 'BROWSER must be a scalar string.'
         ENDIF
      ENDIF ELSE $
         browser = ''

      DEFSYSV, '!JAM', { $

         ;; Path to root directory of JAM library:
         Path: { Tables: FilePath('Tables', Root_Dir=pathJAM) + Path_Sep() }, $

         ;; Minimum and maximum double precision numbers:
         LogMin: 0.95D * ALOG((MACHAR(/Double)).XMin), $
         LogMax: 0.95D * ALOG((MACHAR(/Double)).XMax), $
         Log10Min: 0.95D * ALOG10((MACHAR(/Double)).XMin), $
         Log10Max: 0.95D * ALOG10((MACHAR(/Double)).XMax), $

         ;; Default command to launch a browser window:
         Browser: browser $

      }, 1

      MESSAGE, 'Library has been initialized.', /Inf

   ENDIF

   Resolve_Routine, 'Jam_Check', /Either, /Compile_Full_File

   RETURN

END ;; .......................................................................