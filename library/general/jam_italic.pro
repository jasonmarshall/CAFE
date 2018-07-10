
FUNCTION Jam_Italic, string

  Compile_Opt IDL2
  On_Error, 2

  RETURN, (!D.Name EQ 'PS') ? $
          '!16' + string + '!X' : '!8' + string + '!X'

END
