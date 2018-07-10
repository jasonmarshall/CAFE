;+
; NAME:
; JAM_DATA
;
; PURPOSE:
; A JAM_DATA object is a generic variable container. Data is stored in a single
; structure so a data object may contain many pieces of data of varying size,
; dimensionality and type.
;
; CATEGORY:
; General Programming. I/O.
;
; METHODS:
; ADD: Add variables to the data structure.
; REMOVE: Remove variables from the data structure.
; MERGE: Merge a new data structure with the existing one.
; SET: Set the values of data structure variables.
; GET: Obtain the values of data structure variables.
; READ: Read a Jam_Data format text file into the data structure.
; WRITE: Write the data structure into a Jam_Data format text file.
; SAVE: Save the data structure as an IDL binary file.
; CHECK: Check the properties of data structure variables.
; REQUIRE: Require data structure variables to satisfy given conditions.
; DIMENSIONS: Obtain the dimensions of data structure variables.
; MINMAX: Obtain the minimum and maximum values of data structure variables.
; N_ELEMENTS: Obtain the number of elements of data structure variables.
; N_DIMENSIONS: Obtain the number of dimensions of data structure variables.
; TNAME: Obtain the type names of data structure variables.
; TYPE: Obtain the type codes of data structure variables.
; INFO: Print information about data structure variables to the terminal.
; PRINT: Print the values of data structure variables to the terminal.
; DESTROY: Destroy the object.
;
; CREATION:
; Result = JAM_DATA(Data)
;
; OPTIONAL INPUTS:
; Data: Set this input parameter equal to the structure of variables.
;
; KEYWORD PARAMETERS:
; File: Set this keyword equal to a scalar string containing the name of a
;   'Jam_Data' format file to read.
;
; OUTPUTS:
; The JAM_DATA object reference.
;
; EXAMPLE:
; To create a JAM_DATA container enter:
;   IDL> objRef = JAM_DATA()
; To create a JAM_DATA container and initialize the data structure enter:
;   IDL> data = { C:3 }
;   IDL> objRef = JAM_DATA(data)
; To create a JAM_DATA container and initialize the data structure from a
; Jam_Data format input file named 'data.txt' enter:
;   IDL> objRef = JAM_DATA(File='data.txt')
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
FUNCTION Jam_Data::Init, data, FileName=fileName

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Read data from file.
  IF (N_Elements(fileName) NE 0) THEN self->Read, fileName

  ;; Merge data structure variables.
  IF (N_Elements(data) NE 0) THEN self->Merge, data

  RETURN, 1

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::ADD
;
; PURPOSE:
; This method is used to add data structure variables.
;
; CALLING SEQUENCE:
; objRef->JAM_DATA::ADD, Name, Value
;
; INPUTS:
; Name: A scalar string or string vector of the names of new variables.
; Value: The value to assign to the new variables. All variables are assigned
;   the same value if more than one is given.
;
; EXAMPLE:
; To add a variable enter:
;   IDL> objRef->ADD, 'A', 1
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
PRO Jam_Data::Add, name, value

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 2) THEN $
     Message, Jam_Message('Syntax', 'obj->Jam_Data::Add, Name, Value')
  IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
     Message, Jam_Message('Name', msg)

  ;; Add data field(s).
  start = 0
  IF (N_Elements(value) EQ 0) THEN value = '<UNDEFINED>'
  IF (~Ptr_Valid(self.Data)) THEN BEGIN
     start = 1
     self.Data = Ptr_New(Create_Struct(StrUpCase(name[0]), value))
  ENDIF
  tags = Tag_Names(*self.Data)
  FOR i=start,N_Elements(name)-1 DO BEGIN
     index = Where(tags EQ StrUpCase(name[i]), count)
     IF (count NE 0) THEN $
        Message, Jam_Message(name[i], 'Duplicate data name.')
     *self.Data = Create_Struct(*self.Data, StrUpCase(name[i]), value)
  ENDFOR

  RETURN

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::REMOVE
;
; PURPOSE:
; This method is used to remove data structure variables.
;
; CALLING SEQUENCE:
; objRef->JAM_DATA::REMOVE, Name
;
; INPUTS:
; Name: A scalar string or string vector containing the names of data
;   structure variables to remove.
;
; EXAMPLE:
; To remove a data structure variable enter:
;   IDL> objRef->REMOVE, 'A'
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

PRO Jam_Data::Remove, name

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'obj->Jam_Data::Remove, Name')
  IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
     Message, Jam_Message('Name', msg)

  ;; Ensure that data structure exists.
  IF (~Ptr_Valid(self.Data)) THEN $
     Message, Jam_Message('Data', 'No data in data structure.')

  ;; Create new structure without omitted data.
  tags = Tag_Names(*self.Data)
  FOR i=0,N_Elements(tags)-1 DO BEGIN
     index = Where(tags[i] EQ StrUpCase(name), count)
     IF (count EQ 0) THEN BEGIN
        newData = (N_Elements(newData) EQ 0) ? $
                  Create_Struct(tags[i], (*self.Data).(i)) : $
                  Create_Struct(newData, tags[i], (*self.Data).(i))
     ENDIF
  ENDFOR
  IF (N_Elements(newData) EQ 0) THEN $
     Ptr_Free, self.Data $
  ELSE $
     *self.Data = newData

  RETURN

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::MERGE
;
; PURPOSE:
; This method is used to merge a structure of variables with the existing
; data structure variables.
;
; CALLING SEQUENCE:
; objRef->JAM_DATA::MERGE, Data
;
; INPUTS:
; Data: A structure of data to merge with the existing data structure.
;
; EXAMPLE:
; To merge a new data structure into the existing one enter:
;   IDL> objRef->MERGE, { X:24, Y:25, Z:26 }
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

PRO Jam_Data::Merge, data

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'obj->Jam_Data::Merge, Data')
  IF (~Jam_Check(data, TName='STRUCT', Msg=msg)) THEN $
     Message, Jam_Message('Data', msg)

  ;; Merge new structure with existing one.
  newTags = Tag_Names(data)
  oldTags = (Ptr_Valid(self.Data)) ? Tag_Names(*self.Data) : ['']
  FOR i=0,N_Elements(newTags)-1 DO BEGIN
     index = Where(oldTags EQ newTags[i], count)
     IF (count NE 0) THEN $
        Message, Jam_Message(newTags[i], 'Duplicate variable name.')
     self->Add, newTags[i], data.(i)
  ENDFOR

  RETURN

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::SET
;
; PURPOSE:
; This method is used to set the values of data structure variables.
;
; CALLING SEQUENCE:
; objRef->JAM_DATA::SET, Name, Value
;
; INPUTS:
; Name: A scalar string or string vector containing the names of data
;   structure variables whose values should be set.
; Value: The value to assign to the data structure variables. All variables
;   are assigned the same value if more than one name is given.
;
; EXAMPLE:
; To add a new data structure variable and change its value enter:
;   IDL> objRef->ADD, 'B', 2
;   IDL> objRef->SET, 'B', 20
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
PRO Jam_Data::Set, name, value

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 2) THEN $
     Message, Jam_Message('Syntax', 'obj->Jam_Data::Set, Name, Value')
  IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
     Message, Jam_Message('Name', msg)

  ;; Ensure that data structure exists.
  IF (~Ptr_Valid(self.Data)) THEN $
     Message, Jam_Message('Data', 'No data in data structure.')

  ;; Set data value.
  data = *self.Data
  tags = Tag_Names(data)
  FOR i=0,N_Elements(tags)-1 DO BEGIN
     idx = Where(StrUpCase(name) EQ tags[i], cnt)
     thisData = (cnt EQ 1) ? value : data.(i)
     newData = (N_Elements(newData) EQ 0) ? $
               Create_Struct(tags[i], thisData) : $
               Create_Struct(newData, tags[i], thisData)
  ENDFOR
  *self.Data = newData

  RETURN

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::GET
;
; PURPOSE:
; This method is used to obtain the values of data structure variables.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_DATA::GET(Name)
;
; OPTIONAL INPUTS:
; Name: A scalar string or string vector containing the names of data
;   structure variables whose values should be returned.
;
; OUTPUT:
; The method returns the entire data structure if no name is given. If a
; single name is given, the method returns the value associated with that
; variable. If a vector of names is given, the method returns a structure
; containing the values of each variable.
;
; EXAMPLE:
; To obtain the value of a single data structure variable enter:
;   IDL> Result = objRef->GET('B')
; To obtain the entire data structure enter:
;   IDL> Result = objRef->GET()
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
FUNCTION Jam_Data::Get, name

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Elements(name) NE 0) THEN $
     IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
        Message, Jam_Message('Name', msg)

  ;; Ensure that data structure exists.
  IF (~Ptr_Valid(self.Data)) THEN $
     Message, Jam_Message('Data', 'No data in data structure.')

  ;; Return full data structure if no name is given.
  IF (N_Elements(name) EQ 0) THEN RETURN, *self.Data

  ;; Get data value of individuals data elements.
  tags = Tag_Names(*self.Data)
  nName = N_Elements(name)
  FOR i=0,nName-1 DO BEGIN
     index = Where(tags EQ StrUpCase(name[i]), count)
     IF (count NE 1) THEN $
        Message, Jam_Message(name[i], 'Does not exist in data structure.')
     IF (nName EQ 1) THEN RETURN, (*self.Data).(index)
     struct = (N_Elements(struct) EQ 0) ? $
              Create_Struct(StrUpCase(name[i]), (*self.Data).(index)) : $
              Create_Struct(struct, StrUpCase(name[i]), (*self.Data).(index))
  ENDFOR

  RETURN, struct

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::READ
;
; PURPOSE:
; This method is used to read a Jam_Data format text file into the data
; structure.
;
; CALLING SEQUENCE:
; objRef->JAM_DATA::READ, FileName
;
; INPUTS:
; FileName: A scalar string containing the name of the Jam_Data format text
;   file to import data from.
;
; EXAMPLE:
; To read the contents of a Jam_Data format text file named 'data.txt' enter:
;   IDL> objRef->READ, 'data.txt'
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
PRO Jam_Data::Read, fileName

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'obj->Jam_Data::Read, FileName')
  IF (~Jam_Check(fileName, /Is_File, Msg=msg)) THEN $
     Message, Jam_Message(fileName, msg)

  ;; Merge with the imported data structure.
  self->Merge, Jam_ReadData(fileName)

  RETURN

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::WRITE
;
; PURPOSE:
; This method is used to write the data structure to a Jam_Data format text
; file.
;
; CALLING SEQUENCE:
; objRef->JAM_DATA::WRITE, FileName
;
; INPUTS:
; FileName: A scalar string containing the name of the Jam_Data format text
;   file to write the data structure to.
;
; EXAMPLE:
; To write the contents of the data structure to a Jam_Data format text file
; named 'data.txt' enter:
;   IDL> objRef->WRITE, 'data.txt'
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
PRO Jam_Data::Write, fileName

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'obj->Jam_Data::Write, FileName')
  IF (~Jam_Check(fileName, TName='STRING', N_Elements=1, Msg=msg)) THEN $
     Message, Jam_Message(fileName, msg)

  ;; Write the data structure.
  Jam_WriteData, self.Data, fileName

  RETURN

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::SAVE
;
; PURPOSE:
; This method is used to save the data structure as an IDL binary file.
;
; CALLING SEQUENCE:
; objRef->JAM_DATA::SAVE, FileName
;
; INPUTS:
; FileName: A scalar string containing the name of the IDL binary file in
;   which to save the data structure.
;
; EXAMPLE:
; To save the data structure to an IDL binary file named 'data.sav' enter:
;   IDL> objRef->SAVE, 'data.sav'
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
PRO Jam_Data::Save, fileName

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'obj->Jam_Data::Save, FileName')
  IF (~Jam_Check(fileName, TName='STRING', N_Elements=1, Msg=msg)) THEN $
     Message, Jam_Message(fileName, msg)

  ;; Save the data structure.
  data = *self.Data
  Save, data, FileName=fileName

  RETURN

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::CHECK
;
; PURPOSE:
; This method is used to check if properties of data structure variables
; satisfy given requirements. If no requirements are given, the method checks
; that the data structure variables are valid members of the data structure.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_DATA::CHECK(Name)
;
; INPUTS:
; Name: A scalar string or string vector containing the names of the data
;   structure variables whose properties should be checked.
;
; KEYWORD PARAMETERS:
; DIMENSIONS: Set this keyword equal to a scalar or array of integers
;   containing the required dimensions of the selected variables.
; DOMAIN: Set this keyword equal to a scalar or array of the same type as
;   the selected variables, containing the values which the selected variables
;   must be a member of.
; IS_DIR: Set this keyword to require that the selected variables be strings
;   containing the full-path-names of valid directories.
; IS_FILE: Set this keyword to require that the selected variables be strings
;   containing the full-path-names of valid files.
; IS_FUNCTION: Set this keyword to require that the selected variables be
;   strings containing the names of compilable IDL functions.
; IS_PRO: Set this keyword to require that the selected variables be strings
;   containing the names of compilable IDL procedures.
; IS_ROUTINE: Set this keyword to require that the selected variables be
;   strings containing the names of compilable IDL routines.
; MAX_VALUE: Set this keyword equal to a scalar or single element array of
;   the same type as the selected variables. If a scalar value is given, all
;   elements of the selected variables are tested to check if they are LESS
;   THAN OR EQUAL TO the MAX_VALUE. If a single element array is given, all
;   elements of the selected variables are tested to check if they are LESS
;   THAN the MAX_VALUE.
; MIN_VALUE: Set this keyword equal to a scalar or single element array of
;   the same type as the selected variables. If a scalar value is given, all
;   elements of the selected variables are tested to check if they are GREATER
;   THAN OR EQUAL TO the MAX_VALUE. If a single element array is given, all
;   elements of the selected variables are tested to check if they are GREATER
;   THAN the MAX_VALUE.
; MAX_N_DIMENSIONS: Set this keyword equal to a scalar integer containing
;   the maximum number of allowed dimensions in the selected variables.
; N_DIMENSIONS: Set this keyword equal to a scalar or array of integers
;   containing the possible number of dimensions the selected variables must
;   have.
; N_ELEMENTS: Set this keyword equal to scalar or array of integers containing
;   the possible number of elements the selected variables must have.
; TNAME: Set this keyword equal to a scalar string containing the required
;   type name of the selected variables. In addition to the built in IDL type
;   names, the names 'FIX', 'FLT' and 'NUM' are also understood. 'FIX' includes
;   only non-decimal numeric types, 'FLT' includes only decimal numeric types,
;   while 'NUM' includes all numeric types.
;
; OUTPUTS:
; This function returns a 1 if the variables satisfy all specified conditions
; and 0 otherwise.
;
; OPTIONAL OUTPUTS:
; MSG: Set this keyword equal to a variable to contain a scalar string
;   indicating the error status. If all tests pass, the routine returns a
;   null string. If a test fails, the routine returns a string indicating
;   the nature of the error.
;
; SIDE EFFECTS:
; Testing that a variable contains a valid IDL routine name requires a call to
; the procedure RESOLVE_ROUTINE. Thus, use of the keywords IS_FUNCTION, IS_PRO
; and IS_ROUTINE will result in the compilation of the indicated routines.
;
; EXAMPLE:
; To check if a data structure variable satisfies a set of requirements and
; obtain any error messages enter:
;   IDL> ok = objRef->CHECK('B', TNAME='FIX', N_ELEMENTS=1, MSG=msg)
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
FUNCTION Jam_Data::Check, name, Dimensions=dimensions, Domain=domain, $
                          Is_Dir=is_dir, Is_File=is_file, $
                          Is_Function=is_function, Is_Pro=is_pro, $
                          Is_Routine=is_routine, Max_Value=max_value, $
                          Min_Value=min_value, $
                          Max_N_Dimensions=max_n_dimensions, Msg=msg, $
                          N_Dimensions=n_dimensions, $
                          N_Elements=n_elements, TName=tName

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'obj->Jam_Data::Check, Name')
  IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
     Message, Jam_Message('Name', msg)

  ;; Check if any data is defined.
  IF (~Ptr_Valid(self.Data)) THEN BEGIN
     msg = 'No data is defined.'
     RETURN, 0
  ENDIF

  ;; Check each variable for the indicated properties.
  FOR i=0,N_Elements(name)-1 DO BEGIN
     tags = Tag_Names(*self.Data)
     index = Where(tags EQ StrUpCase(name[i]), count)
     IF (count NE 1) THEN BEGIN
        msg = name[i] + ' does not exist.'
        RETURN, 0
     ENDIF
     ok = Jam_Check(self->Get(name[i]), Dimensions=dimensions, Domain=domain, $
                    Is_Dir=is_dir, Is_File=is_file, Is_Function=is_FUNCTION, $
                    Is_Pro=is_pro, Is_Routine=is_routine, $
                    Max_Value=max_value, Min_Value=min_value, $
                    Max_N_Dimensions=max_n_dimensions, Msg=msg, $
                    N_Dimensions=n_dimensions, N_Elements=n_elements, $
                    TName=tName)
     IF (ok EQ 0) THEN RETURN, 0
  ENDFOR

  RETURN, 1

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::REQUIRE
;
; PURPOSE:
; This method is used to require that properties of data structure variables
; satisfy given requirements. If no requirements are given, the method checks
; that the data structure variables are valid members of the data structure.
; If the requirements are not satisfied, the method throws an error via the
; IDL MESSAGE procedure, outputing an error message to the screen.
;
; CALLING SEQUENCE:
; objRef->JAM_DATA::REQUIRE(Name)
;
; INPUTS:
; Name: A scalar string or string vector containing the names of the data
;   structure variables whose properties should be checked.
;
; KEYWORD PARAMETERS:
; DATA: Set this keyword equal to the data to be checked. If it satisfies
;   the requirements the data is set in the appropriate field or the field
;   is added. If a vector of names is given, the data is assumed to be
;   associated with the first.
; DIMENSIONS: Set this keyword equal to a scalar or array of integers
;   containing the required dimensions of the selected variables.
; DOMAIN: Set this keyword equal to a scalar or array of the same type as
;   the selected variables, containing the values which the selected variables
;   must be a member of.
; IS_DIR: Set this keyword to require that the selected variables be strings
;   containing the full-path-names of valid directories.
; IS_FILE: Set this keyword to require that the selected variables be strings
;   containing the full-path-names of valid files.
; IS_FUNCTION: Set this keyword to require that the selected variables be
;   strings containing the names of compilable IDL functions.
; IS_PRO: Set this keyword to require that the selected variables be strings
;   containing the names of compilable IDL procedures.
; IS_ROUTINE: Set this keyword to require that the selected variables be
;   strings containing the names of compilable IDL routines.
; MAX_VALUE: Set this keyword equal to a scalar or single element array of
;   the same type as the selected variables. If a scalar value is given, all
;   elements of the selected variables are tested to check if they are LESS
;   THAN OR EQUAL TO the MAX_VALUE. If a single element array is given, all
;   elements of the selected variables are tested to check if they are LESS
;   THAN the MAX_VALUE.
; MIN_VALUE: Set this keyword equal to a scalar or single element array of
;   the same type as the selected variables. If a scalar value is given, all
;   elements of the selected variables are tested to check if they are GREATER
;   THAN OR EQUAL TO the MAX_VALUE. If a single element array is given, all
;   elements of the selected variables are tested to check if they are GREATER
;   THAN the MAX_VALUE.
; MAX_N_DIMENSIONS: Set this keyword equal to a scalar integer containing
;   the maximum number of allowed dimensions in the selected variables.
; N_DIMENSIONS: Set this keyword equal to a scalar or array of integers
;   containing the possible number of dimensions the selected variables must
;   have.
; N_ELEMENTS: Set this keyword equal to scalar or array of integers containing
;   the possible number of elements the selected variables must have.
; TNAME: Set this keyword equal to a scalar string containing the required
;   type name of the selected variables. In addition to the built in IDL type
;   names, the names 'FIX', 'FLT' and 'NUM' are also understood. 'FIX' includes
;   only non-decimal numeric types, 'FLT' includes only decimal numeric types,
;   while 'NUM' includes all numeric types.
;
; SIDE EFFECTS:
; Testing that a variable contains a valid IDL routine name requires a call to
; the procedure RESOLVE_ROUTINE. Thus, use of the keywords IS_FUNCTION, IS_PRO
; and IS_ROUTINE will result in the compilation of the indicated routines.
;
; EXAMPLE:
; To require a data structure variable to satisfy a set of requirements enter:
;   IDL> objRef->REQUIRE('B', TNAME='FIX', N_ELEMENTS=1)
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
PRO Jam_Data::Require, name, Data=data, Dimensions=dimensions, Domain=domain, $
                       Is_Dir=is_dir, Is_File=is_file, $
                       Is_Function=is_function, Is_Pro=is_pro, $
                       Is_Routine=is_routine, Max_Value=max_value, $
                       Min_Value=min_value, $
                       Max_N_Dimensions=max_n_dimensions, $
                       N_Dimensions=n_dimensions, $
                       N_Elements=n_elements, TName=tName

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'obj->Jam_Data::Check, Name')
  IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
     Message, Jam_Message('Name', msg)

  ;; Check keyword data for the indicated properties.
  IF (N_Elements(data) NE 0) THEN BEGIN
     ok = Jam_Check(data, Dimensions=dimensions, Domain=domain, $
                    Is_Dir=is_dir, Is_File=is_file, Is_Function=is_FUNCTION, $
                    Is_Pro=is_pro, Is_Routine=is_routine, $
                    Max_Value=max_value, Min_Value=min_value, $
                    Max_N_Dimensions=max_n_dimensions, Msg=msg, $
                    N_Dimensions=n_dimensions, N_Elements=n_elements, $
                    TName=tName)
     IF (ok EQ 0) THEN Message, Jam_Message(name[0], msg)
     IF (self->Check(name[0])) THEN $
        self->Set, name[0], data $
     ELSE $
        self->Add, name[0], data
  ENDIF

  ;; Check each variable for the indicated properties.
  FOR i=1,N_Elements(name)-1 DO BEGIN
     ok = Jam_Check(self->Get(name[i]), Dimensions=dimensions, Domain=domain, $
                    Is_Dir=is_dir, Is_File=is_file, Is_Function=is_FUNCTION, $
                    Is_Pro=is_pro, Is_Routine=is_routine, $
                    Max_Value=max_value, Min_Value=min_value, $
                    Max_N_Dimensions=max_n_dimensions, Msg=msg, $
                    N_Dimensions=n_dimensions, N_Elements=n_elements, $
                    TName=tName)
     IF (ok EQ 0) THEN Message, Jam_Message(name[i], msg)
  ENDFOR

  RETURN

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::DIMENSIONS
;
; PURPOSE:
; This method is used to obtain the dimensions of data structure variables.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_DATA::DIMENSIONS(Name)
;
; OPTIONAL INPUTS:
; Name: A scalar string or string vector containing the names of data
;   structure variables whose dimensions should be returned.
;
; OUTPUT:
; If no variable names are given, the method returns a structure containing
; the dimensions of all data structure variables. If a single variable name
; is given, the method returns an array containing the dimensions of that
; variable. If a vector of variable names is given, the method returns a
; structure containing the dimensions of each named variable.
;
; EXAMPLE:
; To obtain the dimensions of a single data structure variable enter:
;   IDL> Result = objRef->DIMENSIONS('B')
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
FUNCTION Jam_Data::Dimensions, name

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  nName = N_Elements(name)
  IF (nName NE 0) THEN $
     IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
        Message, Jam_Message('Name', msg)

  ;; Ensure that data structure exists.
  IF (~Ptr_Valid(self.Data)) THEN $
     Message, Jam_Message('Data', 'No data in data structure.')

  ;; Return structure of all dimensions if no name is given.
  tags = Tag_Names(*self.Data)
  nTags = N_Elements(tags)
  IF (nName EQ 0) THEN BEGIN
     IF (nTags EQ 1) THEN RETURN, Size((*self.Data).(0), /Dimensions)
     FOR i=0,nTags-1 DO BEGIN
        struct = (N_Elements(struct) EQ 0) ? $
                 Create_Struct(tags[i], $
                               Size((*self.Data).(i), /Dimensions)) : $
                 Create_Struct(struct, tags[i], $
                               Size((*self.Data).(i), /Dimensions))
     ENDFOR
  ENDIF $

     ;; Or get dimensions of specified variables.
  ELSE BEGIN
     FOR i=0,nName-1 DO BEGIN
        index = Where(tags EQ StrUpCase(name[i]), count)
        IF (count NE 1) THEN $
           Message, Jam_Message(name[i], 'Does not exist in data structure.')
        IF (nName EQ 1) THEN RETURN, Size((*self.Data).(index), /Dimensions)
        struct = (N_Elements(struct) EQ 0) ? $
                 Create_Struct(name[i], $
                               Size((*self.Data).(index), /Dimensions)) : $
                 Create_Struct(struct, name[i], $
                               Size((*self.Data).(index), /Dimensions))
     ENDFOR
  ENDELSE

  RETURN, struct

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::MINMAX
;
; PURPOSE:
; This method is used to obtain the minimum and maximum values of data
; structure variables.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_DATA::MINMAX(Name)
;
; OPTIONAL INPUTS:
; Name: A scalar string or string vector containing the names of data
;   structure variables whose minimum and maximum values should be returned.
;
; OUTPUT:
; Minimum and maximum values of variables are returned as two element
; vectors.  If no variable names are given, the method returns a structure
; containing the extremum vectors of all data structure variables. If a
; single variable name is given, the method returns the extremem vector for
; that variable. If a vector of variable names is given, the method returns
; a structure containing the extremem vectors of each named variable.
;
; EXAMPLE:
; To obtain the extremem array of a single data structure variable enter:
;   IDL> Result = objRef->MINMAX('B')
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
FUNCTION Jam_Data::MinMax, name

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  nName = N_Elements(name)
  IF (nName NE 0) THEN $
     IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
        Message, Jam_Message('Name', msg)

  ;; Ensure that data structure exists.
  IF (~Ptr_Valid(self.Data)) THEN $
     Message, Jam_Message('Data', 'No data in data structure.')

  ;; Return structure of all minmax values if no name is given.
  tags = Tag_Names(*self.Data)
  nTags = N_Elements(tags)
  IF (nName EQ 0) THEN BEGIN
     IF (nTags EQ 1) THEN RETURN, MinMax((*self.Data).(0))
     FOR i=0,nTags-1 DO BEGIN
        struct = (N_Elements(struct) EQ 0) ? $
                 Create_Struct(tags[i], MinMax((*self.Data).(i))) : $
                 Create_Struct(struct, tags[i], MinMax((*self.Data).(i)))
     ENDFOR
  ENDIF $

     ;; Or get minmax values of specified variables.
  ELSE BEGIN
     FOR i=0,nName-1 DO BEGIN
        index = Where(tags EQ StrUpCase(name[i]), count)
        IF (count NE 1) THEN $
           Message, Jam_Message(name[i], 'Does not exist in data structure.')
        IF (nName EQ 1) THEN RETURN, MinMax((*self.Data).(index))
        struct = (N_Elements(struct) EQ 0) ? $
                 Create_Struct(name[i], MinMax((*self.Data).(index))) : $
                 Create_Struct(struct, name[i], $
                               MinMax((*self.Data).(index)))
     ENDFOR
  ENDELSE

  RETURN, struct

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::N_ELEMENTS
;
; PURPOSE:
; This method is used to obtain the number of elements contained in data
; structure variables.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_DATA::N_ELEMENTS(Name)
;
; OPTIONAL INPUTS:
; Name: A scalar string or string vector containing the names of data
;   structure variables whose number of elements should be returned.
;
; OUTPUT:
; If no variable names are given, the method returns an integer vector
; containing the number of elements in all data structure variables. If a
; single variable name is given, the method returns a scalar integer
; containing the number of elements in that variable. If a vector of variable
; names is given, the method returns an integer vector containing the number
; of elements in each named variable.
;
; EXAMPLE:
; To obtain the number of elements in a single data structure variable enter:
;   IDL> Result = objRef->N_ELEMENTS('B')
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
FUNCTION Jam_Data::N_Elements, name

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  nName = N_Elements(name)
  IF (nName NE 0) THEN $
     IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
        Message, Jam_Message('Name', msg)

  ;; Ensure that data structure exists.
  IF (~Ptr_Valid(self.Data)) THEN $
     Message, Jam_Message('Data', 'No data in data structure.')

  ;; Return structure of all N_Elements if no name is given.
  tags = Tag_Names(*self.Data)
  nTags = N_Elements(tags)
  IF (nName EQ 0) THEN BEGIN
     IF (nTags EQ 1) THEN RETURN, N_Elements((*self.Data).(0))
     FOR i=0,nTags-1 DO BEGIN
        struct = (N_Elements(struct) EQ 0) ? $
                 Create_Struct(tags[i], N_Elements((*self.Data).(i))) : $
                 Create_Struct(struct, tags[i], N_Elements((*self.Data).(i)))
     ENDFOR
  ENDIF $

     ;; Or get minmax values of specified variables.
  ELSE BEGIN
     FOR i=0,nName-1 DO BEGIN
        index = Where(tags EQ StrUpCase(name[i]), count)
        IF (count NE 1) THEN $
           Message, Jam_Message(name[i], 'Does not exist in data structure.')
        IF (nName EQ 1) THEN RETURN, N_Elements((*self.Data).(index))
        struct = (N_Elements(struct) EQ 0) ? $
                 Create_Struct(name[i], N_Elements((*self.Data).(index))) : $
                 Create_Struct(struct, name[i], $
                               N_Elements((*self.Data).(index)))
     ENDFOR
  ENDELSE

  RETURN, struct

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::N_DIMENSIONS
;
; PURPOSE:
; This method is used to obtain the number of dimensions contained in data
; structure variables.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_DATA::N_DIMENSIONS(Name)
;
; OPTIONAL INPUTS:
; Name: A scalar string or string vector containing the names of data
;   structure variables whose number of dimensions should be returned.
;
; OUTPUT:
; If no variable names are given, the method returns an integer vector
; containing the number of dimensions in all data structure variables. If a
; single variable name is given, the method returns a scalar integer
; containing the number of dimensions in that variable. If a vector of variable
; names is given, the method returns an integer vector containing the number
; of dimensions in each named variable.
;
; EXAMPLE:
; To obtain the number of dimensions in a single data structure variable enter:
;   IDL> Result = objRef->N_DIMENSIONS('B')
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
FUNCTION Jam_Data::N_Dimensions, name

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  nName = N_Elements(name)
  IF (nName NE 0) THEN $
     IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
        Message, Jam_Message('Name', msg)

  ;; Ensure that data structure exists.
  IF (~Ptr_Valid(self.Data)) THEN $
     Message, Jam_Message('Data', 'No data in data structure.')

  ;; Return structure of all N_Elements if no name is given.
  tags = Tag_Names(*self.Data)
  nTags = N_Elements(tags)
  IF (nName EQ 0) THEN BEGIN
     IF (nTags EQ 1) THEN RETURN, Size((*self.Data).(0), /N_Dimensions)
     FOR i=0,nTags-1 DO BEGIN
        struct = (N_Elements(struct) EQ 0) ? $
                 Create_Struct(tags[i], $
                               Size((*self.Data).(i), /N_Dimensions)) : $
                 Create_Struct(struct, tags[i], $
                               Size((*self.Data).(i), /N_Dimensions))
     ENDFOR
  ENDIF $

     ;; Or get minmax values of specified variables.
  ELSE BEGIN
     FOR i=0,nName-1 DO BEGIN
        index = Where(tags EQ StrUpCase(name[i]), count)
        IF (count NE 1) THEN $
           Message, Jam_Message(name[i], 'Does not exist in data structure.')
        IF (nName EQ 1) THEN RETURN, Size((*self.Data).(index), /N_Dimensions)
        struct = (N_Elements(struct) EQ 0) ? $
                 Create_Struct(name[i], $
                               Size((*self.Data).(index), /N_Dimensions)) : $
                 Create_Struct(struct,name[i], $
                               Size((*self.Data).(index), /N_Dimensions))
     ENDFOR
  ENDELSE

  RETURN, struct

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::TNAME
;
; PURPOSE:
; This method is used to obtain the type names of data structure variables.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_DATA::TNAME(Name)
;
; OPTIONAL INPUTS:
; Name: A scalar string or string vector containing the names of data
;   structure variables whose type names should be returned.
;
; OUTPUT:
; If no variable names are given, the method returns a string vector containing
; the type names of all data structure variables. If a single variable name is
; given, the method returns a scalar string containing the type name of that
; variable. If a vector of variable names is given, the method returns a string
; vector containing the type names of each named variable.
;
; EXAMPLE:
; To obtain the type names of a single data structure variable enter:
;   IDL> Result = objRef->TNAME('B')
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
FUNCTION Jam_Data::TName, name

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  nName = N_Elements(name)
  IF (nName NE 0) THEN $
     IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
        Message, Jam_Message('Name', msg)

  ;; Ensure that data structure exists.
  IF (~Ptr_Valid(self.Data)) THEN $
     Message, Jam_Message('Data', 'No data in data structure.')

  ;; Return structure of all N_Elements if no name is given.
  tags = Tag_Names(*self.Data)
  nTags = N_Elements(tags)
  IF (nName EQ 0) THEN BEGIN
     IF (nTags EQ 1) THEN RETURN, Size((*self.Data).(0), /TName)
     FOR i=0,nTags-1 DO BEGIN
        struct = (N_Elements(struct) EQ 0) ? $
                 Create_Struct(tags[i], Size((*self.Data).(i), /TName)) : $
                 Create_Struct(struct, tags[i], Size((*self.Data).(i), /TName))
     ENDFOR
  ENDIF $

     ;; Or get minmax values of specified variables.
  ELSE BEGIN
     FOR i=0,nName-1 DO BEGIN
        index = Where(tags EQ StrUpCase(name[i]), count)
        IF (count NE 1) THEN $
           Message, Jam_Message(name[i], 'Does not exist in data structure.')
        IF (nName EQ 1) THEN RETURN, Size((*self.Data).(index), /TName)
        struct = (N_Elements(struct) EQ 0) ? $
                 Create_Struct(name[i], Size((*self.Data).(index), /TName)) : $
                 Create_Struct(struct, name[i], $
                               Size((*self.Data).(index), /TName))
     ENDFOR
  ENDELSE

  RETURN, struct

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::TYPE
;
; PURPOSE:
; This method is used to obtain the type codes of data structure variables.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_DATA::TYPE(Name)
;
; OPTIONAL INPUTS:
; Name: A scalar string or string vector containing the names of data
;   structure variables whose type codes should be returned.
;
; OUTPUT:
; If no variable names are given, the method returns an integer vector
; containing the type codes of all data structure variables. If a single
; variable name is given, the method returns a scalar integer containing the
; type code of that variable. If a vector of variable names is given, the
; method returns an integer vector containing the type codes of each named
; variable.
;
; EXAMPLE:
; To obtain the type names of a single data structure variable enter:
;   IDL> Result = objRef->TNAME('B')
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
FUNCTION Jam_Data::Type, name

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  nName = N_Elements(name)
  IF (nName NE 0) THEN $
     IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
        Message, Jam_Message('Name', msg)

  ;; Ensure that data structure exists.
  IF (~Ptr_Valid(self.Data)) THEN $
     Message, Jam_Message('Data', 'No data in data structure.')

  ;; Return structure of all N_Elements if no name is given.
  tags = Tag_Names(*self.Data)
  nTags = N_Elements(tags)
  IF (nName EQ 0) THEN BEGIN
     IF (nTags EQ 1) THEN RETURN, Size((*self.Data).(0), /Type)
     FOR i=0,nTags-1 DO BEGIN
        struct = (N_Elements(struct) EQ 0) ? $
                 Create_Struct(tags[i], Size((*self.Data).(i), /Type)) : $
                 Create_Struct(struct, tags[i], Size((*self.Data).(i), /Type))
     ENDFOR
  ENDIF $

     ;; Or get minmax values of specified variables.
  ELSE BEGIN
     FOR i=0,nName-1 DO BEGIN
        index = Where(tags EQ StrUpCase(name[i]), count)
        IF (count NE 1) THEN $
           Message, Jam_Message(name[i], 'Does not exist in data structure.')
        IF (nName EQ 1) THEN RETURN, Size((*self.Data).(index), /Type)
        struct = (N_Elements(struct) EQ 0) ? $
                 Create_Struct(name[i], Size((*self.Data).(index), /Type)) : $
                 Create_Struct(struct, name[i], $
                               Size((*self.Data).(index), /Type))
     ENDFOR
  ENDELSE

  RETURN, struct

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::INFO
;
; PURPOSE:
; This method is used to print information about data structure variables
; to the terminal.
;
; CALLING SEQUENCE:
; objRef->JAM_DATA::INFO, Name
;
; OPTIONAL INPUTS:
; Name: A scalar string or string vector containing the names of data
;   structure variables for which information should be printed.
;
; EXAMPLE:
; To obtain information about a single data structure variable enter:
;   IDL> objRef->INFO, 'B'
; To obtain information about all data structure variables enter:
;   IDL> objRef->INFO
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

PRO Jam_Data::Info, name

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Elements(name) NE 0) THEN $
     IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
        Message, Jam_Message('Name', msg)

  ;; Check that data structure is defined.
  IF (~Ptr_Valid(self.Data)) THEN $
     Message, Jam_Message('Data', 'No data in data structure.')

  ;; Create index indicating which variables to print information about.
  tags = Tag_Names(*self.Data)
  nTags = N_Elements(tags)
  nName = N_Elements(name)
  IF (nName EQ 0) THEN BEGIN
     all = IndGen(nTags)
  ENDIF $
  ELSE BEGIN
     FOR i=0,nName-1 DO BEGIN
        index = Where(tags EQ name[i], count)
        IF (count NE 1) THEN $
           Message, Jam_Message(name[i], 'Does not exist in data structure.')
        all = (N_Elements(all) EQ 0) ? [index] : [all, index]
     ENDFOR
  ENDELSE

  ;; Print info.
  FOR i=0,N_Elements(all)-1 DO BEGIN
     tag  = StrCompress('NAME = ' + tags[all[i]])
     type = StrCompress('TYPE = ' + Size((*self.Data).(all[i]), /TName))
     dims = StrCompress('N_DIMENSIONS = ' + $
                        String(Size((*self.Data).(all[i]), /N_Dimensions)))
     elms = StrCompress('N_ELEMENTS = ' + $
                        String(Size((*self.Data).(all[i]), /N_Elements)))
     Print, $
        Jam_PadString(tag,  30) + $
        Jam_PadString(type, 20) + $
        Jam_PadString(dims, 20) + $
        Jam_PadString(elms, 20)
  ENDFOR

  RETURN

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::PRINT
;
; PURPOSE:
; This method is used to print the values of data structure variables to the
; terminal.
;
; CALLING SEQUENCE:
; objRef->JAM_DATA::PRINT, Name
;
; INPUTS:
; Name: A scalar string or string vector containing the names of data
;   structure variables whose values should be printed.
;
; EXAMPLE:
; To print the value of a single data structure variable enter:
;   IDL> objRef->PRINT, 'B'
; To print the value of all data structure variables enter:
;   IDL> objRef->PRINT
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
PRO Jam_Data::Print, name

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Check input parameters.
  IF (N_Elements(name) NE 0) THEN $
     IF (~Jam_Check(name, TName='STRING', Max_N_Dimensions=1, Msg=msg)) THEN $
        Message, Jam_Message('Name', msg)

  ;; Check that data structure is defined.
  IF (~Ptr_Valid(self.Data)) THEN $
     Message, Jam_Message('Data', 'No data in data structure.')

  ;; Create index indicating which variables to print values for.
  tags = Tag_Names(*self.Data)
  nTags = N_Elements(tags)
  nName = N_Elements(name)
  IF (nName EQ 0) THEN BEGIN
     all = IndGen(nTags)
     name = tags
  ENDIF $
  ELSE BEGIN
     FOR i=0,nName-1 DO BEGIN
        index = Where(tags EQ name[i], count)
        IF (count NE 1) THEN $
           Message, Jam_Message(name[i], 'Does not exist in data structure.')
        all = (N_Elements(all) EQ 0) ? [index] : [all, index]
     ENDFOR
  ENDELSE

  ;; Print indicated data.
  FOR i=0,N_Elements(all)-1 DO BEGIN
     str = name[all[i]] + ' = '
     isString = (Size((*self.Data).(all[i]), /TName) EQ 'STRING')
     IF (isString) THEN str += "'"
     str += String((*self.Data).(all[i]))
     IF (isString) THEN str += "'"
     Print, StrCompress(str)
  ENDFOR

  RETURN

END
;\\

;+
; ===================================================================
; METHOD_NAME:
; JAM_DATA::DESTROY
;
; PURPOSE:
; This method is used to destroy a JAM_DATA object.
;
; CALLING SEQUENCE:
; objRef->JAM_DATA::DESTROY
;
; EXAMPLE:
; To destroy a JAM_DATA object enter:
;   IDL> objRef->DESTROY
;
; MODIFICATION HISTORY:
; Written by:  Jason Marshall, October 27, 2004.
;-

;//
PRO Jam_Data::Destroy

  Compile_Opt IDL2, Hidden
  On_Error, 2

  IF Obj_Valid(self) THEN Obj_Destroy, self

  RETURN

END
;\\

;//
PRO Jam_Data::CleanUp

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Free the data structure pointer.
  IF (Ptr_Valid(self.Data)) THEN Ptr_Free, self.Data

  RETURN

END
;\\

;//
PRO Jam_Data__Define

  Compile_Opt IDL2, Hidden
  On_Error, 2

  class = { JAM_DATA, Data:Ptr_New() }

  RETURN

END
;\\

;/+
FUNCTION Jam_Data, data, FileName=fileName

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  Catch, theError
  IF (theError NE 0) THEN BEGIN
     Catch, /Cancel
     ok = Error_Message(/Error, /Traceback)
     RETURN, Obj_New()
  ENDIF

  ;; Check input parameters.
  IF (N_Elements(data) NE 0) THEN $
     IF (~Jam_Check(data, TName='STRUCT', Msg=msg)) THEN $
        Message, Jam_Message('Data', msg)
  IF (N_Elements(fileName) NE 0) THEN $
     IF (~Jam_Check(fileName, /Is_File, Msg=msg)) THEN $
        Message, Jam_Message(fileName, msg)

  RETURN, Obj_New('Jam_Data', data, FileName=fileName)

END
;\-
