
FUNCTION Jam_Print, _arg, sigFigs, Scientific=scientific, Float=float, $
                    Dot=dot, Notrim=noTrim, Terminal=terminal, LaTeX=LaTeX, $
                    Debug=debug

  Compile_Opt IDL2
  On_Error, 2

  IF (Jam_Check(_arg, TName='STRING')) THEN RETURN, _arg

  isFix = Jam_Check(_arg, TName='FIX')
  isFlt = Jam_Check(_arg, TName='FLT')
  IF ((isFix + isFlt) EQ 0) THEN $
     Message, 'Cannot convert argument to a string!'

  IF (isFix) THEN BEGIN
     format = '(I)'
  ENDIF $
  ELSE BEGIN
     format = (N_Elements(sigFigs) EQ 0) ? $
              '(G16)' : '(G16.' + StrCompress(String(sigFigs), $
                                              /Remove_All) + ')'
     IF (Keyword_Set(scientific)) THEN BEGIN
        format = (N_Elements(sigFigs) EQ 0) ? $
                 '(E16)' : '(E16.' + StrCompress(String(sigFigs), $
                                                 /Remove_All) + ')'
     ENDIF
     IF (Keyword_Set(float)) THEN BEGIN
        format = (N_Elements(sigFigs) EQ 0) ? $
                 '(F16)' : '(F16.' + StrCompress(String(sigFigs), $
                                                 /Remove_All) + ')'
     ENDIF
  ENDELSE

  arg = (Jam_Check(_arg, TName='DOUBLE')) ? Float(_arg) : _arg
  strArg = StrCompress(String(arg, Format=format), /Remove_All)

  IF (~Keyword_Set(noTrim)) THEN BEGIN
     posD = StrPos(strArg, '.')
     IF (posD NE -1L) THEN BEGIN
        after = StrMid(strArg, posD+1)
        strArg = StrMid(strArg, 0, posD)
        posE = StrPos(after, 'E')
        IF (posE NE -1L) THEN BEGIN
           post = StrMid(after, posE)
           after = StrMid(after, 0, posE)
        ENDIF ELSE $
           post = ''
        length = StrLen(after)
        last = StrMid(after, length-1, length)
        WHILE (last EQ '0') DO BEGIN
           after = StrMid(after, 0, length-1)
           length--
           last = StrMid(after, length-1, length)
        ENDWHILE
        IF (after NE '') THEN strArg = strArg + '.' + after
        IF (post NE '') THEN strArg = strArg + post
     ENDIF
  ENDIF
  
  IF (Keyword_Set(terminal)) THEN Return, strArg

  posE = StrPos(strArg, 'E')
  IF (posE NE -1L) THEN str = StrMid(strArg, posE+1, 1)

  IF (posE NE -1L) THEN BEGIN
     strNew = StrMid(strArg, 0, posE)
     exponent = StrMid(strArg, posE+2)
     first = StrMid(exponent, 0, 1)
     IF (first EQ '0') THEN BEGIN
        exponent = StrMid(exponent, 1)
        first = StrMid(exponent, 0, 1)
     ENDIF
     IF (exponent NE '') THEN BEGIN
        IF (strNew EQ '1') THEN BEGIN
           strNew = ''
        ENDIF $
        ELSE BEGIN
           IF (exponent NE '0') THEN BEGIN
              symbol = (Keyword_Set(dot)) ? '\cdot' : '\times'
              IF (~Keyword_Set(LaTeX)) THEN symbol = TeXtoIDL(symbol)
              strNew += symbol
           ENDIF
        ENDELSE
        IF (exponent NE '0') THEN BEGIN
           symbol = (str EQ '+') ? $
                    '10^{' + String(exponent) + '}' : $
                    '10^{-' + String(exponent) + '}'
           IF (~Keyword_Set(LaTeX)) THEN symbol = TeXtoIDL(symbol)
           strNew += symbol
        ENDIF
        IF (strNew EQ '') THEN BEGIN
           symbol = '10^0'
           IF (~Keyword_Set(LaTeX)) THEN symbol = TeXtoIDL(symbol)
           strNew = symbol
        ENDIF
     ENDIF
     strArg = strNew
  ENDIF

  RETURN, strArg

END
