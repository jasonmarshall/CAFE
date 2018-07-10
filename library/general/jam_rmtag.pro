
FUNCTION Jam_rmTag, struct, tag
  
  Compile_Opt IDL2, Hidden
  On_Error, 2
  
  tags = Tag_Names(struct)
  index = Where(tags EQ StrUpCase(tag), count, Complement=index0, $
                NComplement=count0)
  IF (count NE 1) THEN RETURN, struct
  FOR i=0,count0-1 DO BEGIN
     thisTagName = (tags[index0])[i]
     thisTagData = struct.(index0[i])
     newStruct = (N_Elements(newStruct) EQ 0) ? $
                 Create_Struct(thisTagName, thisTagData) : $
                 Create_Struct(newStruct, thisTagName, thisTagData)
  ENDFOR
  
  RETURN, newStruct
  
END
