;+ ====================================================================
; NAME:
; JAM_CHECK
;
; PURPOSE:
; This function checks if an expression satisfies conditions specified
; by setting keyword parameters. If no conditions are specified, this
; function checks if the expression is a defined variable.
;
; CATEGORY:
; Utility.
;
; CALLING SEQUENCE:
; Result = JAM_CHECK(Expression)
;
; INPUTS:
; Expression: Expression to check for specified conditions.
;
; KEYWORD PARAMETERS:
; DIMENSIONS: Set to a scalar or array of integers containing the
;   required dimensions of Expression.
; DOMAIN: Set to a scalar or array of the same type as Expression,
;   containing the values which Expression must be a member of.
; IS_DIRECTORY: Set to require that Expression is a scalar string
;   containing the full-path-name of a valid directory.
; IS_FILE: Set to require that Expression is a scalar string
;   containing the full-path-name of a valid file.
; IS_FUNCTION: Set to require that Expression is a scalar string
;   containing the name of a compilable IDL function.
; IS_PROCEDURE: Set to require that Expression is a scalar string
;   containing the name of a compilable IDL procedure.
; IS_ROUTINE: Set to require that Expression is a scalar string
;   containing the name of a compilable IDL routine.
; MAX_VALUE: Set to a scalar or single element array of the same
;   type as Expression. If a scalar value is given, all elements of
;   Expression are tested to check if they are <= MAX_VALUE. If a
;   single element array is given, all elements of Expression are
;   tested to check if they are < MAX_VALUE.
; MIN_VALUE: Set to a scalar or single element array of the same
;   type as Expression. If a scalar value is given, all elements of
;   Expression are tested to check if they are >= MAX_VALUE. If a
;   single element array is given, all elements of Expression are
;   tested to check if they are > MAX_VALUE.
; MAX_N_DIMS: Set to a scalar integer containing the maximum
;   number of allowed dimensions in Expression.
; N_DIMENSIONS: Set to a scalar or array of integers containing
;   the possible number of dimensions Expression must have.
; N_ELEMENTS: Set to scalar or array of integers containing the
;   possible number of elements Expression must have.
; TAG_NAME: Set to a scalar string containing the tag name of the
;   element of the structure Expression upon which the tests should
;   be performed.
; TNAME: Set to a scalar or vector of strings containing the
;   required type name of Expression. In addition to the built in
;   IDL type names, the names 'FIX', 'FLT' and 'NUM' are also
;   understood. 'FIX' includes only non-decimal numeric types,
;   'FLT' includes only decimal numeric types, and 'NUM' includes
;   all numeric types.
;
; OUTPUTS:
; This function returns a 1 if Expression satisfies all specified
; conditions and 0 otherwise. If no tests are specified via keyword
; parameters, the return value indicates if Expression is defined.
;
; OPTIONAL OUTPUTS:
; MSG: Set to a variable to contain a scalar string indicating the
;   error status. If all tests pass, the routine returns a null
;   string. If a test fails, the string indicates the nature of
;   the error.
;
; SIDE EFFECTS:
; Testing if Expression is a valid IDL routine requires a call
; to the procedure RESOLVE_ROUTINE so that use of the keywords
; IS_FUNCTION, IS_PRO, or IS_ROUTINE results in the compilation of
; the routine indicated by Expression.
;
; REQUIREMENTS:
; <FSC> - ERROR_MESSAGE
;
; EXAMPLE:
; To test if a variable is defined enter:
;   IDL> check = Jam_Check(undefined)
; To test if this function is indeed a valid IDL function enter:
;   IDL> check = Jam_Check('Jam_Check', /Is_Function)
; To test if an array has the proper type and dimensionality enter:
;   IDL> check = Jam_Check(FltArr(2,2), TName='FLT', N_Dim=2)
;
; AUTHOR:
; Jason A. Marshall, Astronomy Dept., Cornell Univ., Ithaca, NY 14853
; jam258@cornell.edu
;
; MODIFICATION HISTORY:
; 2004 May 01 - JAM - Initial version.
; 2004 Jul 24 - JAM - Changed to allow scalars AND arrays of values
;                     for the N_DIMENSIONS and N_ELEMENTS keywords.
; 2004 Jul 29 - JAM - Added the TAG_NAME keyword.
;-
;; ....................................................................
; Copyright (C) 2004 by Jason A. Marshall
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or un-
; modified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;; ....................................................................

;;+ ===================================================================
; NAME:
; JAM_CHECK_DIMENSIONS
;
; PURPOSE:
; This function checks that the DIMENSIONS condition is satisfied.
;
; MODIFICATION HISTORY:
; 2004 May 01 - JAM - Initial version.
;;-

FUNCTION Jam_Check_Dimensions, dimensions, expression, types

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get information about Expression.
  expSize = Size(expression, /Structure)

  ;; Ensure that DIMENSIONS is of the correct type and format.
  dimSize = Size(dimensions, /Structure)
  IF (Where(dimSize.Type_Name EQ types.FixType) EQ -1L) THEN $
    Message, 'DIMENSIONS must be of type FIX.', Level=-2L
  IF (dimSize.N_Dimensions NE 1) THEN $
    Message, 'DIMENSIONS must be one dimensional.', Level=-2L

  ;; Test that the number of elements in each dimension are the same.
  testArray = expSize.Dimensions[0:dimSize.N_Elements-1]
  IF (~Array_Equal(dimensions, testArray)) THEN BEGIN
    RETURN, StrCompress('dimensions must be [' + $
      StrTrim(StrJoin(String(dimensions), ','), 2) + '].')
  ENDIF ELSE $
    RETURN, ''

END

;;+ ===================================================================
; NAME:
; JAM_CHECK_DOMAIN
;
; PURPOSE:
; This function checks that the DOMAIN condition is satisfied.
;
; MODIFICATION HISTORY:
; 2004 May 01 - JAM - Initial version.
;;-

FUNCTION Jam_Check_Domain, domain, expression, types
  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get information about Expression.
  expSize = Size(expression, /Structure)

  ;; Ensure that DOMAIN is of the correct type and format.
  domSize = Size(domain, /Structure)
  IF (expSize.Type_Name EQ 'STRING') THEN BEGIN
    IF (domSize.Type_Name NE 'STRING') THEN $
      Message, 'DOMAIN & Expression must have same type.', Level=-2L
  ENDIF $
  ELSE BEGIN
    expNumType = $
      (Where(expSize.Type_Name EQ types.NumType) EQ -1L) ? 0 : 1
    domNumType = $
      (Where(domSize.Type_Name EQ types.NumType) EQ -1L) ? 0 : 1
    IF (expNumType NE domNumType) THEN $
      Message, 'DOMAIN & Expression must have same type.', Level=-2L
  ENDELSE

  ;; Test that each element of Expression is in the correct domain.
  thisExp = (expSize.Type_Name EQ 'STRING') ? $
    StrUpCase(expression) : expression
  FOR i=0,expSize.N_Elements-1 DO $
    IF (Where(thisExp[i] EQ [domain]) EQ -1) THEN $
      RETURN, StrCompress('must be in the domain [' + $
        StrTrim(StrJoin(String(domain), ','), 2) + '].')

  RETURN, ''

END

;;+ ===================================================================
; NAME:
; JAM_CHECK_IS
;
; PURPOSE:
; This function checks that the IS_XXX conditions are satisfied.
;
; MODIFICATION HISTORY:
; 2004 May 01 - JAM - Initial version.
;;-

FUNCTION Jam_Check_Is, is_what, expression, types

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get information about Expression.
  expSize = Size(expression, /Structure)

  ;; Ensure that Expression is a scalar string.
  IF (expSize.Type_Name NE 'STRING') THEN $
    Message, 'Expression must be a string.', Level=-2L
  IF (expSize.N_Dimensions NE 0) THEN $
    Message, 'Expression must be a scalar.', Level=-2L

  ;; Check for appropriate 'Is' condition.
  CASE (is_what) OF
    'DIR': IF (~File_Test(expression, /Directory)) THEN $
        RETURN, 'not a valid directory.'
    'FILE': IF (~File_Test(expression, /Regular)) THEN $
        RETURN, 'not a valid file.'
    'FUNCTION': Resolve_Routine, expression, /Is_Function, $
      /No_Recompile
    'PRO': Resolve_Routine, expression, /No_Recompile
    'ROUTINE': Resolve_Routine, expression, /Either, /No_Recompile
  ENDCASE

  RETURN, ''

END

;;+ ===================================================================
; NAME:
; JAM_CHECK_MAX
;
; PURPOSE:
; This function checks that the MAX_VALUE condition is satisfied.
;
; MODIFICATION HISTORY:
; 2004 May 01 - JAM - Initial version.
;;-

FUNCTION Jam_Check_Max, max_value, expression, types

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get information about Expression.
  expSize = Size(expression, /Structure)

  ;; Ensure that MAX_VALUE is of the correct type and format.
  maxSize = Size(max_value, /Structure)
  IF (expSize.Type_Name EQ 'STRING') THEN BEGIN
    IF (maxSize.Type_Name NE 'STRING') THEN $
      Message, 'MAX_VALUE & Expression must have same type.', $
        Level=-2L
  ENDIF $
  ELSE BEGIN
    expNumType = $
      (Where(expSize.Type_Name EQ types.NumType) EQ -1L) ? 0 : 1
    maxNumType = $
      (Where(maxSize.Type_Name EQ types.NumType) EQ -1L) ? 0 : 1
    IF (expNumType NE maxNumType) THEN $
      Message, 'MAX_VALUE & Expression must have same type.', $
        Level=-2L
  ENDELSE

  ;; Check that MAX_VALUE is a scalar or one element array.
  IF (maxSize.N_Elements NE 1) THEN $
    Message, 'MAX_VALUE must be a scalar or one element array.', $
      Level=-2L

  ;; Test if Expression <= MAX_VALUE if passed as a scalar.
  ;; Test if Expression <  MAX_VALUE if passed as an array.
  ok = (maxSize.N_Dimensions EQ 0) ? $
    (Max(expression) LE max_value) : $
    (Max(expression) LT max_value)

  ;; Return message if not ok.
  IF (~ok) THEN $
    RETURN, (maxSize.N_Dimensions EQ 0) ? $
      StrCompress('must be less than or equal to ' + $
        String(max_value) + '.') : $
      StrCompress('must be less than ' + $
        String(max_value[0]) + '.') $
  ELSE $
    RETURN, ''

END

;;+ ===================================================================
; NAME:
; JAM_CHECK_MIN
;
; PURPOSE:
; This function checks that the MIN_VALUE condition is satisfied.
;
; MODIFICATION HISTORY:
; 2004 May 01 - JAM - Initial version.
;;-

FUNCTION Jam_Check_Min, min_value, expression, types

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get information about Expression.
  expSize = Size(expression, /Structure)

  ;; Ensure that MIN_VALUE is of the correct type and format.
  minSize = Size(min_value, /Structure)
  IF (expSize.Type_Name EQ 'STRING') THEN BEGIN
    IF (minSize.Type_Name NE 'STRING') THEN $
      Message, 'MIN_VALUE & Expression must have the same type.', $
        Level=-2L
  ENDIF $
  ELSE BEGIN
    expNumType = $
      (Where(expSize.Type_Name EQ types.NumType) EQ -1) ? 0 : 1
    minNumType = $
      (Where(minSize.Type_Name EQ types.NumType) EQ -1) ? 0 : 1
    IF (expNumType NE minNumType) THEN $
      Message, 'MIN_VALUE & Expression must have the same type.', $
        Level=-2L
  ENDELSE

  ;; Check that MIN_VALUE is a scalar or one element array.
  IF (minSize.N_Elements NE 1) THEN $
    Message, 'MIN_VALUE must be a scalar or one element array.', $
      Level=-2L

  ;; Test if Expression >= MIN_VALUE if passed as a scalar.
  ;; Test if Expression >  MIN_VALUE if passed as an array.
  ok = (minSize.N_Dimensions EQ 0) ? $
    (Min(expression) GE min_value) : $
    (Min(expression) GT min_value)

  ;; Return message if not ok.
  IF (~ok) THEN $
    RETURN, (minSize.N_Dimensions EQ 0) ? $
      StrCompress('must be greater than or equal to ' + $
        String(min_value) + '.') : $
      StrCompress('must be greater than ' + $
        String(min_value[0]) + '.') $
  ELSE $
    RETURN, ''

END

;;+ ===================================================================
; NAME:
; JAM_CHECK_MAX_N_DIMENSIONS
;
; PURPOSE:
; This function checks that MAX_N_DIMENSIONS condition is satisfied.
;
; MODIFICATION HISTORY:
; 2004 May 01 - JAM - Initial version.
;;-

FUNCTION Jam_Check_Max_N_Dimensions, max_n_dim, expression, types

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get information about Expression.
  expSize = Size(expression, /Structure)

  ;; Ensure that MAX_N_DIMENSIONS is a scalar FIX.
  dimSize = Size(max_n_dim, /Structure)
  IF (Where(dimSize.Type_Name EQ types.FixType) EQ -1L) THEN $
    Message, 'MAX_N_DIMENSIONS must be of type FIX.', Level=-2L
  IF (dimSize.N_Dimensions NE 0) THEN $
    Message, 'MAX_N_DIMENSIONS must be a scalar.', Level=-2L

  ;; Test for max number of dimensions.
  IF (expSize.N_Dimensions GT max_n_dim) THEN $
    RETURN, StrCompress('number of dimensions must be less than ' + $
      String(max_n_dimensions) + '.') $
  ELSE $
    RETURN, ''

END

;;+ ===================================================================
; NAME:
; JAM_CHECK_N_DIMENSIONS
;
; PURPOSE:
; This function checks that the N_DIMENSIONS condition is satisfied.
;
; MODIFICATION HISTORY:
; 2004 May 01 - JAM - Initial version.
;;-

FUNCTION Jam_Check_N_Dimensions, n_dimensions, expression, types

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get information about Expression.
  expSize = Size(expression, /Structure)

  ;; Ensure that N_DIMENSIONS is a scalar or vector FIX.
  dimSize = Size(n_dimensions, /Structure)
  IF (Where(dimSize.Type_Name EQ types.FixType) EQ -1L) THEN $
    Message, 'N_DIMENSIONS must be of type FIX.', Level=-2L
  IF (dimSize.N_Dimensions GT 1) THEN $
    Message, 'N_DIMENSIONS must be a scalar or vector.', Level=-2L

  ;; Test for the correct number of dimensions.
  IF (Where(expSize.N_Dimensions EQ n_dimensions) EQ -1L) THEN BEGIN
    RETURN, (dimSize.N_Dimensions EQ 0) ? $
      StrCompress('number of dimensions must equal ' + $
        String(n_dimensions) + '.') : $
      StrCompress('number of dimensions must be in the set [' + $
        StrTrim(StrJoin(String(n_dimensions), ','), 2) + '].')
  ENDIF ELSE $
    RETURN, ''

END

;;+ ===================================================================
; NAME:
; JAM_CHECK_N_ELEMENTS
;
; PURPOSE:
; This function checks that the N_ELEMENTS condition is satisfied.
;
; MODIFICATION HISTORY:
; 2004 May 01 - JAM - Initial version.
;;-

FUNCTION Jam_Check_N_Elements, n_elements, expression, types

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get information about Expression.
  expSize = Size(expression, /Structure)

  ;; Ensure that N_ELEMENTS is a scalar or vector FIX.
  elemSize = Size(n_elements, /Structure)
  IF (Where(elemSize.Type_Name EQ types.FixType) EQ -1) THEN $
    Message, 'N_ELEMENTS must be of type FIX.', Level=-2L
  IF (elemSize.N_Dimensions GT 1) THEN $
    Message, 'N_ELEMENTS must be a scalar or vector.', Level=-2L

  ;; Test for the correct number of elements.
  IF (Where(expSize.N_Elements EQ n_elements) EQ -1L) THEN BEGIN
    RETURN, (elemSize.N_Dimensions EQ 0) ? $
      StrCompress('number of elements must equal ' + $
        String(n_elements) + '.') : $
      StrCompress('number of elements must be in the set [' + $
        StrTrim(StrJoin(String(n_elements), ','), 2) + '].')
  ENDIF ELSE $
    RETURN, ''

END

;;+ ===================================================================
; NAME:
; JAM_CHECK_TNAME
;
; PURPOSE:
; This function checks that the TNAME condition is satisfied.
;
; MODIFICATION HISTORY:
; 2004 May 01 - JAM - Initial version.
;;-

FUNCTION Jam_Check_TName, tName, expression, types

  Compile_Opt IDL2, Hidden
  On_Error, 2

  ;; Get information about Expression.
  expSize = Size(expression, /Structure)

  ;; Ensure that TNAME is a scalar or vector of STRINGs.
  tSize = Size(tName, /Structure)
  IF (tSize.Type_Name NE 'STRING') THEN $
    Message, 'TNAME must be of type STRING.', Level=-2L
  IF (tSize.N_Dimensions GT 1) THEN $
    Message, 'TNAME must be a scalar or vector.', Level=-2L

  ;; Test for the correct number of elements.
  check = 0
  nTypes = N_Elements(tName)
  FOR i=0,nTypes-1 DO BEGIN
    CASE (StrUpCase(tName[i])) OF
      'NUM': ok = $
        (Where(expSize.Type_Name EQ types.NumType) EQ -1L) ? 0 : 1
      'FIX': ok = $
        (Where(expSize.Type_Name EQ types.FixType) EQ -1L) ? 0 : 1
      'FLT': ok = $
        (Where(expSize.Type_Name EQ types.FltType) EQ -1L) ? 0 : 1
      ELSE : ok = $
        (expSize.Type_Name NE StrUpCase(tName[i])) ? 0 : 1
    ENDCASE
    IF (ok) THEN check = 1
  ENDFOR

  ;; Output error message.
  IF (check EQ 0) THEN BEGIN
    IF (nTypes EQ 1) THEN BEGIN
      types = tName
    ENDIF $
    ELSE BEGIN
      types = tName[0]
      FOR i=1,nTypes-2 DO types = types + ', ' + tName[i]
      types = types + ' or ' + tName[nTypes-1]
    ENDELSE
    RETURN, StrCompress('must be of type ' + types + '.')
  ENDIF ELSE $
    RETURN, ''

END

;; ====================================================================

FUNCTION Jam_Check, $
  expression, $
  Dimensions=dimensions, $
  Domain=domain, $
  Is_Dir=is_dir, $
  Is_File=is_file, $
  Is_Function=is_function, $
  Is_Pro=is_pro, $
  Is_Routine=is_routine, $
  Max_Value=max_value, $
  Min_Value=min_value, $
  Max_N_Dimensions=max_n_dimensions, $
  Msg=msg, $
  N_Dimensions=n_dimensions, $
  N_Elements=n_elements, $
  Tag_Name=tag_name, $
  TName=tName, $
  Debug=debug

  Compile_Opt IDL2
  Catch, theError
  IF (theError NE 0) THEN BEGIN
    Catch, /Cancel
    IF (!Error_State.Name EQ 'IDL_M_UPRO_UNDEF') THEN BEGIN
      msg = 'is not a defined procedure or function.'
      RETURN, 0L
    ENDIF
    noName = ~(StrMid(!Error_State.Msg, 0, 5) EQ 'JAM_CHECK')
    ok = Error_Message(/Error, NoName=noName)
    !Except = except
    RETURN, 0L
  ENDIF

  ;; Setup debugging.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  ;; Check syntax.
  IF (N_Params() NE 1) THEN Message, 'Incorrect calling syntax.'

  ;; Define additional types.
  fixType = ['INT', 'LONG', 'UINT', 'ULONG', 'LONG64', 'ULONG64']
  fltType = ['FLOAT', 'DOUBLE', 'COMPLEX', 'DCOMPLEX']
  numType = [fixType, fltType]
  types = { FixType:fixType, FltType:fltType, NumType:numType }

  ;; Initial error message.
  msg = ''

  ;; Get data if Expression is a structure.
  IF (N_Elements(tag_name) NE 0) THEN BEGIN

    ;; Check that TAG_NAME is a scalar string.
    tagSize = Size(tag_name, /Structure)
    IF (tagSize.Type_Name NE 'STRING') THEN $
      Message, 'TAG_NAME must be a STRING.'
    IF (tagSize.N_Dimensions NE 0) THEN $
      Message, 'TAG_NAME must be a scalar.'

    ;; Check that Expression is a structure.
    expSize = Size(expression, /Structure)
    IF (expSize.Type_Name NE 'STRUCT') THEN $
      Message, 'Expression must be a STRUCT.'
    IF ((expSize.N_Elements NE 1) && $
        (expSize.N_Dimensions NE 1)) THEN $
      Message, 'Expression must be a scalar STRUCT.'

    ;; Check that TAG_NAME exists and get data.
    index = Where(Tag_Names(expression) EQ StrUpCase(tag_name), count)
    IF (count NE 1) THEN BEGIN
      msg = tag_name + ' is not a valid tag.'
      RETURN, 0L
    ENDIF
    thisExpression = expression.(index)

  ENDIF $
  ELSE BEGIN

    ;; Get information about Expression.
    expSize = Size(expression, /Structure)

    ;; Check that Expression is a defined variable.
    IF (expSize.N_Elements EQ 0) THEN $
      msg = 'is not a defined variable.'
    IF (msg NE '') THEN RETURN, 0L

    ;; Set expression.
    thisExpression = expression

  ENDELSE

  ;; Check dimensions.
  IF (N_Elements(dimensions) NE 0) THEN $
    msg = Jam_Check_Dimensions(dimensions, thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L

  ;; Check domain.
  IF (N_Elements(domain) NE 0) THEN $
    msg = Jam_Check_Domain(domain, thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L

  ;; Check directory.
  IF (Keyword_Set(is_dir)) THEN $
    msg = Jam_Check_Is('DIR', thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L

  ;; Check file.
  IF (Keyword_Set(is_file)) THEN $
    msg = Jam_Check_Is('FILE', thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L

  ;; Check IDL routines.
  IF (Keyword_Set(is_function)) THEN $
    msg = Jam_Check_Is('FUNCTION', thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L
  IF (Keyword_Set(is_pro)) THEN $
    msg = Jam_Check_Is('PRO', thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L
  IF (Keyword_Set(is_routine)) THEN $
    msg = Jam_Check_Is('ROUTINE', thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L

  ;; Check max and min values.
  IF (N_Elements(max_value) NE 0) THEN $
    msg = Jam_Check_Max(max_value, thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L
  IF (N_Elements(min_value) NE 0) THEN $
    msg = Jam_Check_Min(min_value, thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L

  ;; Check number of dimensions.
  IF (N_Elements(max_n_dimensions) NE 0) THEN $
    msg = Jam_Check_Max_N_Dimensions(max_n_dimensions, $
      thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L
  IF (N_Elements(n_dimensions) NE 0) THEN $
    msg = Jam_Check_N_Dimensions(n_dimensions, thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L

  ;; Check number of elements.
  IF (N_Elements(n_elements) NE 0) THEN $
    msg = Jam_Check_N_Elements(n_elements, thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L

  ;; Check type name.
  IF (N_Elements(tName) NE 0) THEN $
    msg = Jam_Check_TName(tName, thisExpression, types)
  IF (msg NE '') THEN RETURN, 0L

  IF (Keyword_Set(debug)) THEN !Except = except

  ;; Return with no error.
  !Except = except
  RETURN, 1L

END

;; ====================================================================

PRO Jam_Check, $
  expression, $
  name, $
  Dimensions=dimensions, $
  Domain=domain, $
  Is_Dir=is_dir, $
  Is_File=is_file, $
  Is_Function=is_function, $
  Is_Pro=is_pro, $
  Is_Routine=is_routine, $
  Max_Value=max_value, $
  Min_Value=min_value, $
  Max_N_Dimensions=max_n_dimensions, $
  N_Dimensions=n_dimensions, $
  N_Elements=n_elements, $
  Tag_Name=tag_name, $
  TName=tName, $
  Debug=debug

  Compile_Opt IDL2
  Catch, theError
  IF (theError NE 0) THEN BEGIN
    Catch, /Cancel
    IF (!Error_State.Name EQ 'IDL_M_UPRO_UNDEF') THEN BEGIN
      msg = expression + ' is not a defined procedure or function.'
      Message, msg, Level=-1, /Continue
    ENDIF
    noName = ~(StrMid(!Error_State.Msg, 0, 5) EQ 'JAM_CHECK')
    ok = Error_Message(/Error, NoName=noName)
    !Except = except
    RETURN
  ENDIF

  ;; Setup debugging.
  except = !Except
  IF (Keyword_Set(debug)) THEN !Except = 2

  ;; Check syntax.
  IF ~((N_Params() EQ 1) || (N_Params() EQ 2)) THEN $
    Message, 'Incorrect calling syntax.'

  ;; Define additional types.
  fixType = ['INT', 'LONG', 'UINT', 'ULONG', 'LONG64', 'ULONG64']
  fltType = ['FLOAT', 'DOUBLE', 'COMPLEX', 'DCOMPLEX']
  numType = [fixType, fltType]
  types = { FixType:fixType, FltType:fltType, NumType:numType }

  ;; Set default Expression name.
  thisName = (N_Elements(name) EQ 0) ? 'Expression' : "'" + name + "'"

  ;; Set default msg.
  msg = ''

  ;; Get data if Expression is a structure.
  IF (N_Elements(tag_name) NE 0) THEN BEGIN

    ;; Check that TAG_NAME is a scalar string.
    tagSize = Size(tag_name, /Structure)
    IF (tagSize.Type_Name NE 'STRING') THEN $
      Message, 'TAG_NAME must be a STRING.'
    IF (tagSize.N_Dimensions NE 0) THEN $
      Message, 'TAG_NAME must be a scalar.'

    ;; Check that Expression is a structure.
    expSize = Size(expression, /Structure)
    IF (expSize.Type_Name NE 'STRUCT') THEN $
      Message, thisName + ' must be a STRUCT.'
    IF ((expSize.N_Elements NE 1) && $
        (expSize.N_Dimensions NE 1)) THEN $
      Message, thisName + ' must be a scalar STRUCT.'

    ;; Check that TAG_NAME exists and get data.
    index = Where(Tag_Names(expression) EQ StrUpCase(tag_name), count)
    IF (count NE 1) THEN $
      Message, "'" + tag_name + "' is not a valid tag.", Level=-1

    ;; Set expression.
    thisExpression = expression.(index)

  ENDIF $
  ELSE BEGIN

    ;; Get information about Expression.
    expSize = Size(expression, /Structure)

    ;; Check that Expression is a defined variable.
    IF (expSize.N_Elements EQ 0) THEN $
      Message, thisName + ' is not a defined variable.', Level=-1

    ;; Set expression.
    thisExpression = expression

  ENDELSE

  ;; Check dimensions.
  IF (N_Elements(dimensions) NE 0) THEN $
    msg = Jam_Check_Dimensions(dimensions, thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1

  ;; Check domain.
  IF (N_Elements(domain) NE 0) THEN $
    msg = Jam_Check_Domain(domain, thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1

  ;; Check directory.
  IF (Keyword_Set(is_dir)) THEN $
    msg = Jam_Check_Is('DIR', thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1

  ;; Check file.
  IF (Keyword_Set(is_file)) THEN $
    msg = Jam_Check_Is('FILE', thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1

  ;; Check IDL routines.
  IF (Keyword_Set(is_function)) THEN $
    msg = Jam_Check_Is('FUNCTION', thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1
  IF (Keyword_Set(is_pro)) THEN $
    msg = Jam_Check_Is('PRO', thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1
  IF (Keyword_Set(is_routine)) THEN $
    msg = Jam_Check_Is('ROUTINE', thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1

  ;; Check max and min values.
  IF (N_Elements(max_value) NE 0) THEN $
    msg = Jam_Check_Max(max_value, thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1
  IF (N_Elements(min_value) NE 0) THEN $
    msg = Jam_Check_Min(min_value, thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1

  ;; Check number of dimensions.
  IF (N_Elements(max_n_dimensions) NE 0) THEN $
    msg = Jam_Check_Max_N_Dimensions(max_n_dimensions, $
      thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1
  IF (N_Elements(n_dimensions) NE 0) THEN $
    msg = Jam_Check_N_Dimensions(n_dimensions, thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1

  ;; Check number of elements.
  IF (N_Elements(n_elements) NE 0) THEN $
    msg = Jam_Check_N_Elements(n_elements, thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1

  ;; Check type name.
  IF (N_Elements(tName) NE 0) THEN $
    msg = Jam_Check_TName(tName, thisExpression, types)
  IF (msg NE '') THEN $
    Message, thisName + ' ' + msg, Level=-1

  !Except = except
  RETURN

END
