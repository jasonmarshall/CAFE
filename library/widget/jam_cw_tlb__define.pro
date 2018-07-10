;;+
; ===================================================================
; NAME:
; JAM_CW_TLB_NOTIFY_REALIZE
;
; PURPOSE:
; This procedure is called when the TLB is realized.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;;-

PRO Jam_CW_TLB_Notify_Realize, TLB

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Get object reference.
  Widget_Control, TLB, Get_UValue=objRef

  ;; Call 'Notify_Realize' method.
  objRef->Notify_Realize

  RETURN

END

;;+
; ===================================================================
; NAME:
; JAM_CW_TLB_KILL_NOTIFY
;
; PURPOSE:
; This procedure is called when the TLB is killed.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;;-

PRO Jam_CW_TLB_Kill_Notify, TLB

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Get object reference.
  Widget_Control, TLB, Get_UValue=objRef

  ;; Destroy object.
  IF (Obj_Valid(objRef)) THEN Obj_Destroy, objRef

  RETURN

END

;;+
; ===================================================================
; NAME:
; JAM_CW_TLB_EVENT
;
; PURPOSE:
; This is the TLB event handling procedure assigned by XMANAGER.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;;-

PRO Jam_CW_TLB_Event, event

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Get object reference.
  Widget_Control, event.Top, Get_UValue=objRef

  ;; Call event handling method.
  objRef->Event, event

  RETURN

END

;;+
; ===================================================================
; NAME:
; JAM_CW_TLB::INIT
;
; PURPOSE:
; This lifecycle method initializes the self structure variables and creates
; the widgets.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;;-

FUNCTION Jam_CW_TLB::Init, input, MBar=mBar

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Set input object in self.
  self.Input = input

  ;; Create ID and UValue JAM_DATA objects in self.
  self.ID = Jam_Data()
  self.UValue = Jam_Data()

  ;; Obtain non-binary TLB keyword values.
  IF (input->Check('Scr_XSize')) THEN scr_xsize = input->Get('Scr_XSize')
  IF (input->Check('Scr_YSize')) THEN scr_ysize = input->Get('Scr_YSize')
  IF (input->Check('Title')) THEN title = input->Get('Title')
  IF (input->Check('XOffset')) THEN xoffset = input->Get('XOffset')
  IF (input->Check('YOffset')) THEN yoffset = input->Get('YOffset')
  IF (input->Check('Group_Leader')) THEN $
     group_leader = input->Get('Group_Leader')
  IF (input->Check('TLB_Frame_Attr')) THEN $
     tlb_frame_attr = input->Get('TLB_Frame_Attr')

  ;; Create base widget.
  TLB_ID = (Arg_Present(mBar)) ? $
           Widget_Base(Column=1, $
                       Base_Align_Left=1, $
                       MBar=mBar, $
                       Group_Leader=group_leader, $
                       Modal=input->Get('Modal'), $
                       Scr_XSize=scr_xsize, $
                       Scr_YSize=scr_ysize, $
                       XOffset=xoffset, $
                       YOffset=yoffset, $
                       Title=title, $
                       TLB_Frame_Attr=tlb_frame_attr, $
                       TLB_Move_Events=input->Get('TLB_Move_Events'), $
                       TLB_Size_Events=input->Get('TLB_Size_Events'), $
                       TLB_Kill_Request_Events= $
                       input->Get('TLB_Kill_Request_Events'),$
                       Tracking_Events=input->Get('Tracking_Events'), $
                       Notify_Realize='Jam_CW_TLB_Notify_Realize', $
                       Kill_Notify='Jam_CW_TLB_Kill_Notify', $
                       Space=0) : $
           Widget_Base(Column=1, $
                       Base_Align_Left=1, $
                       Group_Leader=group_leader, $
                       Modal=input->Get('Modal'), $
                       Scr_XSize=scr_xsize, $
                       Scr_YSize=scr_ysize, $
                       XOffset=xoffset, $
                       YOffset=yoffset, $
                       Title=title, $
                       TLB_Frame_Attr=tlb_frame_attr, $
                       TLB_Move_Events=input->Get('TLB_Move_Events'), $
                       TLB_Size_Events=input->Get('TLB_Size_Events'), $
                       TLB_Kill_Request_Events= $
                       input->Get('TLB_Kill_Request_Events'),$
                       Tracking_Events=input->Get('Tracking_Events'), $
                       Notify_Realize='Jam_CW_TLB_Notify_Realize', $
                       Kill_Notify='Jam_CW_TLB_Kill_Notify', $
                       Space=0)
  self.ID->Add, 'TLB', TLB_ID

  RETURN, 1

END

;;+
; ===================================================================
; NAME:
; JAM_CW_TLB::NOTIFY_REALIZE
;
; PURPOSE:
; This method is called after the TLB is realized.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;;-

PRO Jam_CW_TLB::Notify_Realize

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Get TLB widget geometry.
  geom = Widget_Info(self.ID->Get('TLB'), /Geometry)

  ;; Save initial screen size and position information.
  self.Geom = Jam_Data()
  xSize = geom.Scr_XSize - 6
  ySize = (self.Input->Get('MBar')) ? geom.Scr_YSize - 40 : geom.Scr_YSize
  self.Geom->Add, 'Scr_XSize', xSize
  self.Geom->Add, 'Scr_YSize', ySize
  self.Geom->Add, 'Scr_XSize0', xSize
  self.Geom->Add, 'Scr_YSize0', ySize
  self.Geom->Add, 'XOffset', geom.XOffset
  self.Geom->Add, 'YOffset', geom.YOffset 

  RETURN

END

;;+
; ===================================================================
; NAME:
; JAM_CW_TLB::EVENT
;
; PURPOSE:
; This method handles events.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;;-

PRO Jam_CW_TLB::Event, event

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Get the type of event.
  thisEvent = Tag_Names(event, /Structure_Name)

  ;; Handle KILL_REQUEST events.
  IF ((thisEvent EQ 'WIDGET_KILL_REQUEST') AND $
      (self.Input->Get('Kill_Event_Pro') NE '')) THEN BEGIN
     Call_Procedure, self.Input->Get('Kill_Event_Pro'), event
     RETURN
  ENDIF

  ;; Handle TRACKING events.
  IF ((thisEvent EQ 'WIDGET_TRACKING') AND $
      (self.Input->Get('Tracking_Event_Pro') NE '')) THEN BEGIN
     Call_Procedure, self.Input->Get('Tracking_Event_Pro'), event
     RETURN
  ENDIF

  ;; Add dx and dy info to event structure for resize events.
  IF (thisEvent EQ 'WIDGET_BASE') THEN BEGIN
     dx = event.X - self.Geom->Get('Scr_XSize')
     dy = event.Y - self.Geom->Get('Scr_YSize')
     event = Create_Struct(event, 'DX', dx, 'DY', dy, $
                           'X0', self.Geom->Get('Scr_XSize0'), $
                           'Y0', self.Geom->Get('Scr_YSize0'), $
                           Name='WIDGET_TLB')
  ENDIF

  ;; Send events to event handler.
  IF (self.Input->Get('Event_Pro') NE '') THEN BEGIN

     ;; Call the external event handler.
     Call_Procedure, self.Input->Get('Event_Pro'), event

     ;; Update the geometry info in self.
     IF (Obj_Valid(self)) THEN BEGIN
        geom = Widget_Info(self.ID->Get('TLB'), /Geometry)
        self.Geom->Set, 'XOffset', geom.XOffset
        self.Geom->Set, 'YOffset', geom.YOffset
        self.Geom->Set, 'Scr_XSize', geom.Scr_XSize
        self.Geom->Set, 'Scr_YSize', geom.Scr_YSize
     ENDIF

  ENDIF $
  ELSE BEGIN

     ;; Handle events.
     CASE (thisEvent) OF
        'WIDGET_TLB_MOVE': $
           BEGIN

           ;; Set the offsets in the geometry data object.
           geom = Widget_Info(self.ID->Get('TLB'), /Geometry)
           self.Geom->Set, 'XOffset', geom.XOffset
           self.Geom->Set, 'YOffset', geom.YOffset

        END
        'WIDGET_BASE': $
           BEGIN

           ;; Force the screen size and positioning to the set values.
           Widget_Control, self.ID->Get('TLB'), $
                           Scr_XSize=self.Geom->Get('Scr_XSize'), $
                           Scr_YSize=self.Geom->Get('Scr_YSize')

        END
        ELSE:
     ENDCASE

  ENDELSE

  RETURN

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::ID
;
; PURPOSE:
; This method returns the TLB widget ID.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

FUNCTION Jam_CW_TLB::ID

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Return TLB widget ID.
  RETURN, self.ID->Get('TLB')

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::REALIZE
;
; PURPOSE:
; This method is used to realize and manage the widget heirarchy.
;
; CALLING SEQUENCE:
; objRef->JAM_CW_TLB::REALIZE
;
; KEYWORD PARAMETERS:
; CLEANUP: Set this keyword equal to a scalar string containing the name of
;   the procedure to be called when the widget is destroyed. By default, a
;   clean-up module is called that destroys the JAM_TLB object.
; BLOCK: Set this keyword to make the widget a blocking widget.
;
; EXAMPLE:
; To realize and manage the heirarchy enter:
;   IDL> objRef->REALIZE
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

PRO Jam_CW_TLB::Realize, CleanUp=cleanUp, Block=block, Position=position

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Set default keyword parameter values.
  no_block = (Keyword_Set(block)) ? 0 : 1

  ;; Unmap the widget heirarchy.
  self->UnMap

  ;; Center TLB.
  input = self.Input
  IF (input->Check('XOffset') EQ 0) THEN BEGIN
     IF (N_Elements(position) EQ 0) THEN position = [0.5, 0.5]
     CenterTLB, self.ID->Get('TLB'), position[0], position[1]
  ENDIF

  ;; Realize the widget heirarchy.
  Widget_Control, self.ID->Get('TLB'), /Realize

  ;; Map the widget heirarchy.
  self->Map

  ;; Manage the widget heirarchy.
  XManager, 'Jam_CW_TLB', self.ID->Get('TLB'), No_Block=no_block, $
            CleanUp=cleanUp, Event_Handler='Jam_CW_TLB_Event'

  RETURN

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::MAP
;
; PURPOSE:
; This method is used to map the widget heirarchy.
;
; CALLING SEQUENCE:
; objRef->JAM_CW_TLB::MAP
;
; EXAMPLE:
; To map the heirarchy enter:
;   IDL> objRef->MAP
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

PRO Jam_CW_TLB::Map

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Map the heirarchy.
  Widget_Control, self.ID->Get('TLB'), Map=1

  RETURN

END

PRO Jam_CW_TLB::Update

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Map the heirarchy.
  Widget_Control, self.ID->Get('TLB'), Update=1

  RETURN

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::UNMAP
;
; PURPOSE:
; This method is used to unmap the widget heirarchy.
;
; CALLING SEQUENCE:
; objRef->JAM_CW_TLB::UNMAP
;
; EXAMPLE:
; To unmap the heirarchy enter:
;   IDL> objRef->UNMAP
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

PRO Jam_CW_TLB::UnMap

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Unmap the heirarchy.
  Widget_Control, self.ID->Get('TLB'), Map=0

  RETURN

END

PRO Jam_CW_TLB::NoUpdate

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Unmap the heirarchy.
  Widget_Control, self.ID->Get('TLB'), Update=0

  RETURN

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::Tracking_Events
;
; PURPOSE:
; This method is used to set Tracking_Events.
;
; CALLING SEQUENCE:
; objRef->JAM_CW_TLB::Tracking_Events, set
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

PRO Jam_CW_TLB::Tracking_Events, set

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  self.Input->Set, 'Tracking_Events', (N_Elements(set) NE 0)
  Widget_Control, self.ID->Get('TLB'), $
                  Tracking_Events=self.Input->Get('Tracking_Events')

  RETURN

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::SCR_XSIZE
;
; PURPOSE:
; This method is used to set the TLB 'screen' x-size.
;
; CALLING SEQUENCE:
; objRef->JAM_CW_TLB::SCR_XSIZE, Scr_XSize
;
; INPUTS:
; Scr_XSize: Set this equal to a scalar integer containing the new x-size.
;
; EXAMPLE:
; To set the screen x-size to 300 pixels enter:
;   IDL> objRef->SCR_XSIZE, 300
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

PRO Jam_CW_TLB::Scr_XSize, scr_xsize

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Check the input syntax.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'objRef->SCR_XSIZE, Scr_XSize')

  ;; Set the x-size in the geometry data object.
  self.Geom->Set, 'Scr_XSize', scr_xsize

  ;; Check the input parameter.
  self.Geom->Require, 'Scr_XSize', TName='NUM', N_Dimensions=0

  ;; Resize the widget.
  Widget_Control, self.ID->Get('TLB'), Scr_XSize=scr_xsize

  RETURN

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::SCR_XSIZE
;
; PURPOSE:
; This method is used to get the TLB 'screen' x-size.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_CW_TLB::SCR_XSIZE()
;
; EXAMPLE:
; To get the screen x-size enter:
;   IDL> Result = objRef->SCR_XSIZE()
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

FUNCTION Jam_CW_TLB::Scr_XSize

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Return the x-size.
  RETURN, self.Geom->Get('Scr_XSize')

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::SCR_YSIZE
;
; PURPOSE:
; This method is used to set the TLB 'screen' y-size.
;
; CALLING SEQUENCE:
; objRef->JAM_CW_TLB::SCR_YSIZE, Scr_YSize
;
; INPUTS:
; Scr_YSize: Set this equal to a scalar integer containing the new y-size.
;
; EXAMPLE:
; To set the screen y-size to 300 pixels enter:
;   IDL> objRef->SCR_YSIZE, 300
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

PRO Jam_CW_TLB::Scr_YSize, scr_ysize

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Check the input syntax.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'objRef->SCR_YSIZE, Scr_YSize')

  ;; Set the y-size in the geometry data object.
  self.Geom->Set, 'Scr_YSize', scr_ysize

  ;; Check the input parameter.
  self.Geom->Require, 'Scr_YSize', TName='NUM', N_Dimensions=0

  ;; Resize the widget.
  Widget_Control, self.ID->Get('TLB'), Scr_YSize=scr_ysize

  RETURN

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::SCR_YSIZE
;
; PURPOSE:
; This method is used to get the TLB 'screen' y-size.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_CW_TLB::SCR_YSIZE()
;
; EXAMPLE:
; To get the screen y-size enter:
;   IDL> Result = objRef->SCR_YSIZE()
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

FUNCTION Jam_CW_TLB::Scr_YSize

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Return the y-size.
  RETURN, self.Geom->Get('Scr_YSize')

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::XOFFSET
;
; PURPOSE:
; This method is used to set the TLB x-offset.
;
; CALLING SEQUENCE:
; objRef->JAM_CW_TLB::XOFFSET, XOffset
;
; INPUTS:
; XOffset: Set this equal to a scalar integer containing the new x-offset.
;
; EXAMPLE:
; To set the x-offset to 100 pixels enter:
;   IDL> objRef->XOFFSET, 100
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

PRO Jam_CW_TLB::XOffset, xoffset

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Check the input syntax.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'objRef->XOffset, XOffset')

  ;; Check the input parameter.
  self.Geom->Require, 'XOffset', Data=xoffset, TName='NUM', N_Dimensions=0

  ;; Move the widget.
  Widget_Control, self.ID->Get('TLB'), XOffset=xoffset

  RETURN

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::XOFFSET
;
; PURPOSE:
; This method is used to get the TLB x-offset.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_CW_TLB::XOFFSET()
;
; EXAMPLE:
; To get the x-offset enter:
;   IDL> Result = objRef->XOFFSET()
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

FUNCTION Jam_CW_TLB::XOffset

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Return the x-offset.
  RETURN, self.Geom->Get('XOffset')

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::YOFFSET
;
; PURPOSE:
; This method is used to set the TLB y-offset.
;
; CALLING SEQUENCE:
; objRef->JAM_CW_TLB::YOFFSET, YOffset
;
; INPUTS:
; YOffset: Set this equal to a scalar integer containing the new y-offset.
;
; EXAMPLE:
; To set the y-offset to 100 pixels enter:
;   IDL> objRef->YOFFSET, 100
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

PRO Jam_CW_TLB::YOffset, yoffset

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Check the input syntax.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'objRef->YOFFSET, YOffset')

  ;; Set the y-offset in the geometry data object.
  self.Geom->Set, 'YOffset', yoffset

  ;; Check the input parameter.
  self.Geom->Require, 'YOffset', TName='NUM', N_Dimensions=0

  ;; Move the widget.
  Widget_Control, self.ID->Get('TLB'), YOffset=yoffset

  RETURN

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::YOFFSET
;
; PURPOSE:
; This method is used to get the TLB y-offset.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_CW_TLB::YOFFSET()
;
; EXAMPLE:
; To get the y-offset enter:
;   IDL> Result = objRef->YOFFSET()
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

FUNCTION Jam_CW_TLB::YOffset

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Return the y-offset.
  RETURN, self.Geom->Get('YOffset')

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::TITLE
;
; PURPOSE:
; This method is used to set the TLB title.
;
; CALLING SEQUENCE:
; objRef->JAM_CW_TLB::TITLE, Title
;
; INPUTS:
; Title: Set this equal to a scalar string containing the new TLB title.
;
; EXAMPLE:
; To set the TLB title enter:
;   IDL> objRef->TITLE, 'Title'
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

PRO Jam_CW_TLB::Title, title

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Check the input syntax.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'objRef->TITLE, Title')

  ;; Set the title in the input object.
  IF (self.Input->Check('Title')) THEN $
     self.Input->Set, 'Title', title $
  ELSE $
     self.Input->Add, 'Title', title

  ;; Check the input parameter.
  self.Input->Require, 'Title', TName='STRING', N_Dimensions=0

  ;; Re-title the widget.
  Widget_Control, self.ID->Get('TLB'), Base_Set_Title=self.Input->Get('Title')

  RETURN

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::UVALUE
;
; PURPOSE:
; This method is used to add data to the TLB user value.
;
; CALLING SEQUENCE:
; objRef->JAM_CW_TLB::UVALUE, Name, Data
;
; INPUTS:
; Name: Set this equal to a scalar string name for the data.
; Data: Set this equal to the data.
;
; EXAMPLE:
; To add data to the user value enter:
;   IDL> objRef->UValue, 'A', 1
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

PRO Jam_CW_TLB::UValue, name, data

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Check the input syntax.
  IF (N_Params() NE 2) THEN $
     Message, Jam_Message('Syntax', 'objRef->UVALUE, Name, Data')

  ;; Add data to UValue.
  IF (self.UValue->Check(name)) THEN $
     self.UValue->Set, name, data $
  ELSE $
     self.UValue->Add, name, data

  RETURN

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::UVALUE
;
; PURPOSE:
; This method is used to return the value of UValue data.
;
; CALLING SEQUENCE:
; Result = objRef->JAM_CW_TLB::UVALUE(Name)
;
; INPUTS:
; Name: Set this equal to a scalar string indicating the name of the
;   UValue data to return.
;
; EXAMPLE:
; To return data from the user value enter:
;   IDL> Result = objRef->UValue('A')
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

FUNCTION Jam_CW_TLB::UValue, name

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Check the input syntax.
  IF (N_Params() NE 1) THEN $
     Message, Jam_Message('Syntax', 'Result = objRef->UVALUE(Name)')

  ;; Return UValue data.
  RETURN, self.UValue->Get(name)

END

;+
; ===================================================================
; NAME:
; JAM_CW_TLB::DESTROY
;
; PURPOSE:
; This method is used to destroy the object.
;
; CALLING SEQUENCE:
; objRef->JAM_CW_TLB::DESTROY
;
; EXAMPLE:
; To destroy the object enter:
;   IDL> objRef->DESTROY
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;-

PRO Jam_CW_TLB::Destroy

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Destroy object.
  IF (Obj_Valid(self)) THEN BEGIN
     Widget_Control, self->ID(), /Destroy
     Obj_Destroy, self
  ENDIF

  RETURN

END

;;+
; ===================================================================
; NAME:
; JAM_CW_TLB::CLEANUP
;
; PURPOSE:
; This lifecycle method destroys the objects in the self structure.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;;-

PRO Jam_CW_TLB::CleanUp

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Destroy objects.
  IF (Obj_Valid(self.Input)) THEN self.Input->Destroy
  IF (Obj_Valid(self.ID)) THEN self.ID->Destroy
  IF (Obj_Valid(self.Geom)) THEN self.Geom->Destroy
  IF (Obj_Valid(self.UValue)) THEN self.UValue->Destroy

  RETURN

END

;;+
; ===================================================================
; NAME:
; JAM_CW_TLB__DEFINE
;
; PURPOSE:
; This procedure defines the JAM_CW_TLB object class.
;
; MODIFICATION HISTORY:
; Written by: Jason Marshall, November 1, 2004.
;;-

PRO Jam_CW_TLB__Define

  ;; Compiler options.
  Compile_Opt IDL2, Hidden

  ;; Error handling.
  On_Error, 2

  ;; Define object structure.
  class = { JAM_CW_TLB, $
            Input:Obj_New(), $
            ID:Obj_New(), $
            Geom:Obj_New(), $
            UValue:Obj_New() }

  RETURN

END
