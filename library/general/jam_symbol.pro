
FUNCTION Jam_Symbol, symbol, Pad=pad

  Compile_Opt IDL2
  On_Error, 2

  IF (N_Elements(pad) EQ 0) THEN pad = ''

  names = ['angstrom', $
           'otilde', $
           'obar', $
           'ohat', $
           'slash', $
           'int', $
           'langle', $
           'rangle']

  symbolsPS = [String(197B), $
               '!S!A!9' + pad + '~!X!R', $
               '!S!A!11' + pad + '-!X!R', $
               '!S!A!3' + pad + '^!X!R', $
               '!9' + String(Byte(164)) + '!X', $
               '!9' + String(Byte(242)) + '!X', $
               '!9' + String(Byte(225)) + '!X', $
               '!9' + String(Byte(241)) + '!X']

  symbolsX = ['!6!sA!r!u!9 %!6!n', $
               '!S!A!9' + pad + 'A!X!R', $
               '!S!A!11' + pad + '-!X!R', $
               '!S!A!3' + pad + '^!X!R', $
              '/', $
              '!9i!X', $
              '!13<!X', $
              '!13>!X']

  symbols = (!D.Name EQ 'PS') ? symbolsPS : symbolsX

  IF (N_Elements(symbol) EQ 0) THEN BEGIN
     struct = Create_Struct(names[0], symbols[0])
     FOR i=1,N_Elements(symbols)-1 DO $
        struct = Create_Struct(struct, names[i], symbols[i])
     
     RETURN, struct
  ENDIF $
  ELSE BEGIN
     idx = Where(names EQ symbol, cnt)
     RETURN, (cnt EQ 1) ? symbols[idx] : ''
  ENDELSE

END
