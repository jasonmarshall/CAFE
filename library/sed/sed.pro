;+ ====================================================================
; NAME:
; SED
;
; PURPOSE:
; This procedure is used to obtain and view spectroscopic and
; photometric data contained in SED structures and PHOT files.
;
; CATEGORY:
; Spitzer/IRS, Astronomy.
;
; CALLING SEQUENCE:
; SED, [Object]
;
; OPTIONAL INPUTS:
; Object: Set this equal to a scalar string containing the name of the
;   object to analyze. If not given, a dialog widget is produced to
;   interactively select an object.
;
; KEYWORD PARAMETERS:
; PATH_SED: Set this keyword equal to a scalar string containing the
;   path to the SED files. By default, PATH is set equal to the
;   value of the system variable '!MIRBASE' if it exists, and the
;   current working directory if it doesn't exist.
; PATH_PHOT: Set this keyword equal to a scalar string containing the
;   path to the photometry files. By default, the file is assumed to
;   be in the 'phot' sub-directory of the PATH_SED directory.
; PLOT_IRS: Set this keyword to create a plot of the spectroscopic
;   data contained in the SED structure. If the PLOTPHOT keyword
;   is also set, the spectra will be overplotted on the PHOT plot.
; PLOT_PHOT: Set this keyword to create a plot of the photometric
;   data contained in the PHOT file.
; WINDOW: Set this keyword to create a new window for plots.
; XRANGE: Set this keyword equal to a two element vector containing
;   the lower and upper limits of the wavelength vector to plot.
; YRANGE: Set this keyword equal to a two element vector containing
;   the lower and upper limits of the flux vector to plot.
; XLINEAR: Set this keyword to make plots linear in wavelength.
; YLINEAR: Set this keyword to make plots linear in flux.
; CURSOR: Set this keyword to output the current cursor location.
; PAH: Set this keyword to overplot lines indicating the locations
;   of PAH features.
; LINES: Set this keyword to overplot lines indicating the locations
;   of atomic and molecular fine-structure lines.
;
; OUTPUT KEYWORDS:
; SED: Use this keyword to obtain the SED structure.
; PHOT: Use this keyword to obtaine the PHOT structure.
; FILE: Use this keyword to obtain the full path names of the selected SED
;   files.
;
; REQUIREMENTS:
;      <JAM> - JAM_CHECK, JAM_READDATA, EFLUX
;      <FSC> - ERROR_MESSAGE, FSC_COLOR
; <ASTROLIB> - PLOTSYM, OPLOTERROR, RDPLOT
; <TEXTOIDL> - TEXTOIDL
;
; EXAMPLE:
; To produce a dialog widget to select objects and obtain their
; SED and PHOT structures enter:
;   IDL> SED, SED=SED, Phot=phot
; To obtain the SED and PHOT structures of a given object enter:
;   IDL> SED, 'OBJECT', SED=SED, Phot=phot
; To plot the spectroscopic data enter:
;   IDL> SED, /Plot_IRS
; To plot both the spectroscopic and photometric data enter:
;   IDL> SED, /Plot_IRS, /Plot_PHOT
;
; AUTHOR:
; Jason A. Marshall, Astronomy Dept., Cornell Univ., Ithaca, NY 14853
; jam258@cornell.edu
;
; MODIFICATION HISTORY:
; 2005 Dec 05 - JAM - Initial SED procedure created by merging
;                     routines SED_RESTORE, SED_PHOT, and SED_PLOT.
;-
;; ....................................................................
; Copyright (C) 2005, Jason A. Marshall
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or un-
; modified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;; ....................................................................


;;+ ===================================================================
; NAME:
; SED_PLOT
;
; PURPOSE:
; This procedure creates the SED plots.
;
; MODIFICATION HISTORY:
; 2005 Dec 05 - JAM - Initial version.
;;-

PRO SED_Plot, SED, $
              phot, $
              PlotIRS=plotIRS, $
              PlotPhot=plotPhot, $
              Window=window, $
              XRange=xRange, $
              YRange=yRange, $
              XLinear=xLinear, $
              YLinear=yLinear, $
              Cursor=cursor, $
              PAH=PAH, $
              Lines=lines, $
              Title=_title, $
              BackColor=_backColor, $
              AxisColor=_axisColor, $
              _Extra=extra

  Compile_Opt IDL2
  On_Error, 2

  IF (!D.Name NE 'PS') THEN BEGIN
     IF ((Keyword_Set(window)) || (!D.Window EQ -1L)) THEN BEGIN
        RATIO = 0.7D
        screen = RATIO * Jam_GetScreenSize()
        Window, /Free, XSize=screen[0], YSize=screen[1]
     ENDIF
  ENDIF

  IF (Keyword_Set(plotPhot)) THEN BEGIN
     name = phot.Name
     wave = phot.WRest
     flux = phot.FRest
     fErr = phot.EFRest
     stat = phot.Stat
     isPhot = Replicate(1L, N_Elements(phot.WRest))
  ENDIF

  IF (Keyword_Set(plotIRS)) THEN BEGIN
     thisName = Replicate('IRS', N_Elements(SED.Wave))
     name = (N_Elements(name) EQ 0) ? thisName : [name, thisName]
     wave = (N_Elements(wave) EQ 0) ? SED.Wave : [wave, SED.Wave]
     flux = (N_Elements(flux) EQ 0) ? SED.Flux.Jy : [flux, SED.Flux.Jy]
     fErr = (N_Elements(fErr) EQ 0) ? $
            EFluxIRS(SED=SED) : [fErr, EFluxIRS(SED=SED)]
     thisStat = Replicate(0L, N_Elements(SED.Wave))
     thisPhot = Replicate(0L, N_Elements(SED.Wave))
     stat = (N_Elements(stat) EQ 0) ? thisStat : [stat, thisStat]
     isPhot = (N_Elements(isPhot) EQ 0) ? thisPhot : [isPhot, thisPhot]
  ENDIF

  idx = Sort(wave)
  name = name[idx]
  wave = wave[idx]
  flux = flux[idx]
  fErr = fErr[idx]
  stat = stat[idx]
  isPhot = isPhot[idx]

  IF (N_Elements(xRange) EQ 0) THEN BEGIN
     xRange = MinMax(wave)
     IF (~Keyword_Set(xLinear)) THEN $
        xRange = [0.8D * xRange[0], 1.2D * xRange[1]]
  ENDIF

  idx = Where((wave GE xRange[0]) AND (wave LE xRange[1]))
  name = name[idx]
  wave = wave[idx]
  flux = flux[idx]
  fErr = fErr[idx]
  stat = stat[idx]
  isPhot = isPhot[idx]

  idxPhot = Where(isPhot EQ 1, cntPhot)
  idxIRS  = Where(isPhot EQ 0, cntIRS)

  IF (N_Elements(yRange) EQ 0) THEN BEGIN
     thisFlux = flux
     idx = Where(flux EQ 0.D, cnt)
     IF (cnt GT 0) THEN thisFlux[idx] += (3.D * fErr[idx])
     yRange = MinMax(thisFlux)
     IF (~Keyword_Set(yLinear)) THEN $
        yRange = [0.3D * yRange[0], 1.3D * yRange[1]]
  ENDIF

  colors = FSC_Color(/AllColors, ColorStructure=colorStruct)
  IF (!D.Name EQ 'PS') THEN BEGIN
     backColor = (N_Elements(_backColor) EQ 0) ? $
                 colorStruct.White : Jam_GetTag(colorStruct, _backColor)
     axisColor = (N_Elements(_axisColor) EQ 0) ? $
                 colorStruct.Black : Jam_GetTag(colorStruct, _axisColor)
     dataColor = colorStruct.Black
     SL2Color = colorStruct.Blue
     SL3Color = colorStruct.Green
     SL1Color = colorStruct.Red
     LL2Color = SL2Color
     LL3Color = SL3Color
     LL1Color = SL1Color
     lineColor = colorStruct.MediumGray
     errColor = colorStruct.SlateGray
  ENDIF $
  ELSE BEGIN
     backColor = (N_Elements(_backColor) EQ 0) ? $
                 colorStruct.Black : Jam_GetTag(colorStruct, _backColor)
     axisColor = (N_Elements(_axisColor) EQ 0) ? $
                 colorStruct.White : Jam_GetTag(colorStruct, _axisColor)
     dataColor = colorStruct.Snow
     SL2Color = colorStruct.Blue
     SL3Color = colorStruct.Green
     SL1Color = colorStruct.Red
     LL2Color = SL2Color
     LL3Color = SL3Color
     LL1Color = SL1Color
     lineColor = colorStruct.MediumGray
     errColor = colorStruct.SlateGray
  ENDELSE

  z = SED.cz / 3D5
  IF (N_Elements(_title) EQ 0) THEN BEGIN
     title = SED.Object + ' [z=' + String(z, Format='(F5.3)') + ']'
  ENDIF $
  ELSE BEGIN
     title = _title + ' [z=' + String(z, Format='(F5.3)') + ']'
  ENDELSE

  xLog = ~Keyword_Set(xLinear)
  yLog = ~Keyword_Set(yLinear)

  Plot, [1], [1], $
        XLog=xLog, XStyle=1, XRange=xRange, $
        YLog=yLog, YStyle=1, YRange=yRange, $
        XTitle=TeXtoIDL('\lambda_{rest} [\mum]'), $
        YTitle=TeXtoIDL('f_\nu [Jy]'), $
        Background=backColor, Color=axisColor, $
        /NoData, Title=title, CharSize=1.3, _Extra=extra

  IF (cntIRS GT 0) THEN BEGIN
     waveErr = [wave[idxIRS], Reverse(wave[idxIRS])]
     fluxErr = [(flux + fErr)[idxIRS] < yRange[1], $
                Reverse((flux - fErr)[idxIRS] > yRange[0])]
;     PolyFill, waveErr, fluxErr, Color=errColor
  ENDIF

  tags = Tag_Names(SED)
  idxNods = Where(tags EQ 'NOD1FLUX', cntNods)
  IF ((cntIRS GT 0) && (cntNods EQ 1)) THEN BEGIN
     OPlot, SED.Wave, SED.Nod1Flux.Jy, Thick=1, Color=colorStruct.Gray
     OPlot, SED.Wave, SED.Nod2Flux.Jy, Thick=1, COlor=colorStruct.Gray
  ENDIF

  IF (cntIRS GT 0) THEN $
     OPlot, wave[idxIRS], flux[idxIRS], Color=dataColor, Thick=2

  IF (cntPhot GT 0) THEN BEGIN

     idx = Where(stat[idxPhot] EQ 0L, cnt)
     IF (cnt GT 0) THEN BEGIN
        PlotSym, 0, /Fill, Color=dataColor
        OPlotError, (wave[idxPhot])[idx], (flux[idxPhot])[idx], $
                    (fErr[idxPhot])[idx], /NoHat, ErrColor=dataColor, PSym=8
     ENDIF

     idx = Where(stat[idxPhot] EQ 1L, cnt)
     IF (cnt GT 0) THEN BEGIN
        PlotSym, 5, /Fill, Color=dataColor
        OPlot, (wave[idxPhot])[idx], 3.D * (fErr[idxPhot])[idx], PSym=8
     ENDIF

     idxMIPS24 = Where(name EQ 'MIPS24', cntMIPS24)
     idxMIPS70 = Where(name EQ 'MIPS70', cntMIPS70)
     idxMIPS160 = Where(name EQ 'MIPS160', cntMIPS160)
     PlotSym, 0, 1.5, Thick=thick, Color=dataColor
     IF (cntMIPS24 EQ 1) THEN BEGIN
        fluxMIPS24 = (stat[idxMIPS24] EQ 0L) ? $
                     flux[idxMIPS24] : 3.D * fErr[idxMIPS24]
        OPlot, wave[idxMIPS24], fluxMIPS24, PSym=8
     ENDIF
     IF (cntMIPS70 EQ 1) THEN BEGIN
        fluxMIPS70 = (stat[idxMIPS70] EQ 0L) ? $
                     flux[idxMIPS70] : 3.D * fErr[idxMIPS70]
        OPlot, wave[idxMIPS70], fluxMIPS70, PSym=8
     ENDIF
     IF (cntMIPS160 EQ 1) THEN BEGIN
        fluxMIPS160 = (stat[idxMIPS160] EQ 0L) ? $
                      flux[idxMIPS160] : 3.D * fErr[idxMIPS160]
        OPlot, wave[idxMIPS160], fluxMIPS160, PSym=8
     ENDIF

  ENDIF

  idxMode = Where(tags EQ 'MODE', cntMode)
  IF ((cntIRS GT 0) && (cntMode EQ 1)) THEN BEGIN
     mode = SED.Mode
     order = SED.Order
     waveSED = wave[idxIRS]
     fluxSED = flux[idxIRS]
     idxSL2 = Where((mode EQ 'SL') AND (order EQ 2), cntSL2)
     idxSL3 = Where((mode EQ 'SL') AND (order EQ 3), cntSL3)
     idxSL1 = Where((mode EQ 'SL') AND (order EQ 1), cntSL1)
     idxLL2 = Where((mode EQ 'LL') AND (order EQ 2), cntLL2)
     idxLL3 = Where((mode EQ 'LL') AND (order EQ 3), cntLL3)
     idxLL1 = Where((mode EQ 'LL') AND (order EQ 1), cntLL1)
;     IF (cntSL2 GT 0) THEN $
;        OPlot, waveSED[idxSL2], fluxSED[idxSL2], Thick=2, Color=SL2Color
;     IF (cntSL3 GT 0) THEN $
;        OPlot, waveSED[idxSL3], fluxSED[idxSL3], Thick=2, Color=SL3Color
;     IF (cntSL1 GT 0) THEN $
;        OPlot, waveSED[idxSL1], fluxSED[idxSL1], Thick=2, Color=SL1Color
;     IF (cntLL2 GT 0) THEN $
;        OPlot, waveSED[idxLL2], fluxSED[idxLL2], Thick=2, Color=LL2Color
;     IF (cntLL3 GT 0) THEN $
;        OPlot, waveSED[idxLL3], fluxSED[idxLL3], Thick=2, Color=LL3Color
;     IF (cntLL1 GT 0) THEN $
;        OPlot, waveSED[idxLL1], fluxSED[idxLL1], Thick=2, Color=LL1Color
  ENDIF

  IF (Keyword_Set(PAH)) THEN BEGIN
     filePAH = !Jam.Path.Tables + 'pah_template.txt'
     dataPAH = Jam_ReadData(filePAH)
     FOR i=0,N_Elements(dataPAH.Wave0)-1 DO BEGIN
        wave0 = (dataPAH.Wave0)[i]
        IF ((wave0 GE xRange[0]) AND (wave0 LE xRange[1])) THEN BEGIN
           PlotS, [wave0, wave0], [1.05 * yRange[0], 0.95 * yRange[1]], $
                  Color=lineColor, LineStyle=1
        ENDIF
     ENDFOR
  ENDIF

  IF (Keyword_Set(lines)) THEN BEGIN

     fileATO = !Jam.Path.Tables + 'lines.atomic.txt'
     dataATO = Jam_ReadData(fileATO)
     FOR i=0,N_Elements(dataATO.Wave0)-1 DO BEGIN
        wave0 = (dataATO.Wave0)[i]
        IF ((wave0 GE xRange[0]) AND (wave0 LE xRange[1])) THEN BEGIN
           PlotS, [wave0, wave0], [yRange[0], yRange[1]], $
                  Color=lineColor, LineStyle=1
           text = StrCompress((dataATO.Name)[i] + ' (' + $
                              String(wave0, Format='(F5.2)') + ')', $
                              /Remove_All)
           XYOutS, wave0, 1.1D * yRange[0], text, $
                   Orientation=90.D, Color=lineColor
        ENDIF
     ENDFOR

     fileMOL = !Jam.Path.Tables + 'lines.molecular.txt'
     dataMOL = Jam_ReadData(fileMOL)
     FOR i=0,N_Elements(dataMOL.Wave0)-1 DO BEGIN
        wave0 = (dataMOL.Wave0)[i]
        IF ((wave0 GE xRange[0]) AND (wave0 LE xRange[1])) THEN BEGIN
           PlotS, [wave0, wave0], [yRange[0], yRange[1]], $
                  Color=lineColor, LineStyle=1
           text = StrCompress((dataMOL.Name)[i] + ' (' + $
                              String(wave0, Format='(F5.2)') + ')', $
                              /Remove_All)
           XYOutS, wave0, 1.1D * yRange[0], text, $
                   Orientation=90.D, Color=lineColor
        ENDIF
     ENDFOR

  ENDIF

  IF (Keyword_Set(cursor)) THEN $
     RDPlot, /Print, /FullCursor, XTitle='Wave = ', YTitle='Flux = '

  RETURN

END

;;~ ===================================================================

PRO SED, object, $
         Path_SED=pathSED, $
         Path_Phot=pathPhot, $
         Plot_IRS=plotIRS, $
         Plot_Phot=plotPhot, $
         Window=window, $
         XRange=xRange, $
         YRange=yRange, $
         XLinear=xLinear, $
         YLinear=yLinear, $
         Cursor=cursor, $
         PAH=PAH, $
         Lines=lines, $
         SED=SED, $
         Phot=phot, $
         IRAS=IRAS, $
         Debug=debug, $
         NoCatch=noCatch, $
         _Extra=extra

  Compile_Opt IDL2
  IF (~Keyword_Set(noCatch)) THEN BEGIN
     Catch, theError
     IF (theError NE 0) THEN BEGIN
        Catch, /Cancel
        ok = Error_Message(/Error)
        RETURN
     ENDIF
  ENDIF ELSE $
     On_Error, 2

  ;; Setup debugging.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  ;; Check input.
  IF (N_Elements(object) NE 0) THEN $
     Jam_Check, object, 'Object', TName='STRING', N_Elements=1

  ;; Set default SED path.
  IF (N_Elements(pathSED) EQ 0) THEN BEGIN
     DefSysV, '!MIRBASE', Exists=exists
     IF (~exists) THEN BEGIN
        CD, Current=pathSED
     ENDIF ELSE $
        pathSED = !MIRBASE
  ENDIF
  Jam_Check, pathSED, 'PATH_SED', /Is_Dir

  ;; Handle case when objects are passed to the program.
  IF (N_Elements(object) NE 0) THEN BEGIN
     file = FilePath(object + '.xdr', Root_Dir=pathSED)
     IF (~Jam_Check(file, /Is_File, Msg=msg)) THEN Message, file + ' is ' + msg
  ENDIF $
  ELSE BEGIN

     ;; Create dialog widget to pick the SED file.
     file = Dialog_PickFile(Filter='*.xdr', /Fix_Filter, $
                             /Must_Exist, Path=pathSED, /Read, $
                             Title='Select SED...')
     
     ;; Return if cancel is pressed.
     IF (file EQ '') THEN BEGIN
        SED = ''
        RETURN
     ENDIF
     
  ENDELSE

  ;; Restore the SED.
  Restore, file

  ;; Restore the PHOT structure.
  IF ((Keyword_Set(plotPhot)) || (Arg_Present(phot))) THEN BEGIN

     ;; Set default PHOT path.
     IF (N_Elements(pathPhot) EQ 0) THEN BEGIN
        DefSysV, '!SEDBASE', Exists=exists
        IF (~exists) THEN BEGIN
           CD, Current=pathPhot
        ENDIF ELSE $
           pathPhot = !SEDBASE
     ENDIF
     Jam_Check, pathPhot, 'PATH_PHOT', /Is_Dir

     ;; Read PHOT file.
     thisName = (Keyword_Set(IRAS)) ? 'IRAS' + SED.Object : SED.Object
     IF (StrUpCase(StrMid(thisName, 2, /Reverse_Offset)) EQ 'NUC') THEN $
        thisName = StrMid(thisName, 0, StrLen(thisName)-3)
     IF (thisName EQ 'NGC0520') THEN thisName = 'NGC520'
     photFile = FilePath(thisName + '.sed.txt', Root_Dir=pathPhot)
     z = SED.cz / 3D5
     IF (Jam_Check(photFile, /Is_File)) THEN BEGIN

        thisPhot = Jam_ReadData(photFile)

        idx = Where(StrMid(thisPhot.Band, 0, 4) NE 'IRS-', cnt)
        IF (cnt EQ 0) THEN idx = IndGen(N_Elements(thisPhot.Band))

        band = (thisPhot.Band)[idx]
        N_Band = N_Elements(band)
        filter = (Jam_Check(thisPhot, Tag_Name='Filter')) ? $
                 (thisPhot.Filter)[idx] : Replicate('', N_Band)
        wave = (thisPhot.Wave)[idx]
        flux = (thisPhot.Flux)[idx]
        sigma = (thisPhot.Sigma)[idx]
        limit = (thisPhot.Limit)[idx]
        fit = (thisPhot.Fit)[idx]
        plot = (thisPhot.Plot)[idx]
        reference = (thisPhot.Reference)[idx]

        ;; Create PHOT structure.
        phot = { Object:thisPhot.Object, $
                 Redshift:thisPhot.Redshift, $
                 Class:thisPhot.Class, $
                 ClassRef:thisPhot.ClassRef, $
                 Name:band, $
                 Filter:filter, $
                 WObs:wave, $
                 FObs:flux, $
                 EFObs:sigma, $
                 WRest:(wave / (1.D + z)), $
                 FRest:(flux / (1.D + z)), $
                 EFRest:(sigma / (1.D + z)), $
                 Stat:limit, $
                 Fit:fit, $
                 Plot:plot, $
                 Reference:reference }

     ENDIF $
     ELSE BEGIN
        phot = { Object:'', $
                 Redshift:SED.cz/3D5, $
                 Class:'', $
                 ClassRef:'', $
                 Name:'', $
                 Filter:'', $
                 WObs:0.D, $
                 FObs:0.D, $
                 EFObs:0.D, $
                 WRest:0.D, $
                 FRest:0.D, $
                 EFRest:0.D, $
                 Stat:0, $
                 Fit:0, $
                 Plot:0, $
                 Reference:'' }
     ENDELSE

  ENDIF

  ;; Make plots.
  IF ((Keyword_Set(plotPhot)) || (Keyword_Set(plotIRS))) THEN BEGIN
     SED_Plot, SED, phot, PlotIRS=plotIRS, PlotPhot=plotPhot, $
               Window=window, XRange=xRange, YRange=yRange, XLinear=xLinear, $
               YLinear=yLinear, Cursor=cursor, PAH=PAH, Lines=lines, $
               _Extra=extra
  ENDIF

  !Except = except
  RETURN

END
