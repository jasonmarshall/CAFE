;+ ===========================================================================
; NAME:
;       JAM_TLB
;
; PURPOSE:
;
;       This function creates a top-level base compound widget. The function
;       creates an instance of the JAM_CW_TLB object, the methods of which
;       may be used to set the properties of the GUI. The compound widget
;       also contains a base widget which may be used as the parent ID for a
;       user-defined widget heirarchy.
;
; CATEGORY:
; 
;       Compound Widget.
;
; CALLING SEQUENCE:
;
;       Result = JAM_TLB()
;
; KEYWORD PARAMETERS:
;
;       GROUP_LEADER - Set this keyword equal to a scalar long integer
;                      containing the widget ID of the group leader.
;
;       MODAL - Set this keyword to create a modal dialog widget.
;
;       SCR_XSIZE - Set this keyword equal to a scalar integer giving the
;                   desired 'screen' x-size of the widget.
;
;       SCR_YSIZE - Set this keyword equal to a scalar integer giving the
;                   desired 'screen' y-size of the widget.
;
;       TITLE - Set this keyword equal to a scalar string containing the
;               title to be used for the widget.
;
;       TLB_FRAME_ATTR - Set this keyword equal to a scalar integer
;                        indicating the properties of the TLB to be
;                        supressed. Choices are additive and consist of the
;                        following options: (1) Base cannot be resized,
;                        maximized or minimized; (2) Supress display of
;                        system menu; (4) Supress title bar; (8) Base cannot
;                        be closed; (16) Base cannot be moved.
;
;       TLB_MOVE_EVENTS - Set this keyword equal to zero to keep from
;                         sending TLB move events, which are sent by
;                         default.
;
;       TLB_SIZE_EVENTS - Set this keyword equal to zero to keep from
;                         sending TLB size events, which are sent by
;                         default.
;
;       TRACKING_EVENTS - Set this keyword to send events when the mouse
;                         enters or leaves the region.
;
;       EVENT_PRO - Set this keyword equal to a scalar string containing the
;                   name of the procedure to handle top-level-base
;                   events. By default, an event handler is provided that
;                   preserves the original TLB position and size.
;
;       TRACKING_EVENT_PRO - Set this keyword equal to a scalar string
;                            containing the name of the procedure to handle
;                            TRACKING events. By default, such events are
;                            ignored (see TRACKIN_EVENTS).
;
;       KILL_EVENT_PRO - Set this keyword equal to a scalar string
;                        containing the name of the procedure to handle
;                        KILL_REQUEST top-level-base events.  By default,
;                        such events are ignored.
;
;       XOFFSET - Set this keyword equal to a scalar integer giving the
;                 x-position of the widget on the screen in pixels.
;
;       YOFFSET - Set this keyword equal to a scalar integer giving the
;                 y-position of the widget on the screen in pixels.
;
; OUTPUTS:
; 
;       The function returns the object reference.
;
; EXAMPLE:
;
;       To create a JAM_TLB object enter:
;
;          IDL> TLB = JAM_TLB()
;
;       To construct a widget heirarchy enter:
;
;          IDL> ID = WIDGET_BUTTON(TLB->ID(), Value='Button')
;
;       To realize and manage the widget enter:
;
;          IDL> TLB->REALIZE
;
; AUTHOR:
;
;       Jason A. Marshall
;       Department of Astronomy, Cornell University, Ithaca, NY 14853
;       jam258@cornell.edu
;
; MODIFICATION HISTORY:
;
;       Written by: Jason Marshall, November 1, 2004.
;-
;; ---------------------------------------------------------------------------
; Copyright (C) 2004, 2007, Jason A. Marshall
; This software is provided as is with no warranty whatsoever. Permission to
; use, copy, modify and distribute modified or unmodified copies is granted,
; provided this copyright and disclaimer are included unchanged.
;; ---------------------------------------------------------------------------

;+
; ===================================================================
;-
FUNCTION Jam_TLB, Group_Leader=group_leader, Modal=modal, $
                  Scr_XSize=scr_xsize, Scr_YSize=scr_ysize, $
                  Title=title, TLB_Frame_Attr=tlb_frame_attr, $
                  TLB_Move_Events=tlb_move_events, $
                  TLB_Size_Events=tlb_size_events, $
                  Tracking_Events=tracking_events, $
                  XOffset=xoffset, YOffset=yoffset, $
                  SizeRatio=sizeRatio, Event_PRO=event_PRO, $
                  Kill_Event_PRO=kill_event_PRO, $
                  Tracking_Event_PRO=tracking_event_PRO, $
                  MBar=mBar, Debug=debug

  ;; Compiler options.
  Compile_Opt IDL2

  ;; Error handling.
  Catch, theError
  IF (theError NE 0) THEN BEGIN
     Catch, /Cancel
     ok = Error_Message(/Error, /Traceback)
     RETURN, Obj_New()
  ENDIF

  ;; Create input data object.
  input = Jam_Data()

  ;; Set default event handlers.
  IF (N_Elements(event_pro) EQ 0) THEN event_pro = ''
  IF (N_Elements(kill_event_pro) EQ 0) THEN kill_event_pro = ''
  IF (N_Elements(tracking_event_pro) EQ 0) THEN tracking_event_pro = ''
  IF (event_PRO NE '') THEN $
     Jam_Check, event_PRO, 'Event_PRO', /Is_PRO
  IF (kill_event_PRO NE '') THEN $
     Jam_Check, kill_event_PRO, 'Kill_Event_PRO', /Is_PRO
  IF (tracking_event_PRO NE '') THEN $
     Jam_Check, tracking_event_PRO, 'Tracking_Event_PRO', /Is_PRO
  input->Add, 'Event_Pro', event_pro
  input->Add, 'Kill_Event_Pro', kill_event_pro
  input->Add, 'Tracking_Event_Pro', tracking_event_pro

  ;; Set default binary keyword values.
  tlb_move_events = (N_Elements(tlb_move_events) NE 0) ? $
                    Keyword_Set(tlb_move_events) : 1L
  tlb_size_events = (N_Elements(tlb_size_events) NE 0) ? $
                    Keyword_Set(tlb_size_events) : 1L
  tlb_kill_request_events = (kill_event_PRO EQ '') ? 0L : 1L

  ;; Add binary keywords to data object.
  input->Add, 'MBar', Arg_Present(mBar)
  input->Add, 'Modal', Keyword_Set(modal)
  input->Add, 'TLB_Move_Events', tlb_move_events
  input->Add, 'TLB_Size_Events', tlb_size_events
  input->Add, 'Tracking_Events', Keyword_Set(tracking_events)
  input->Add, 'TLB_Kill_Request_Events', tlb_kill_request_events

  ;; Set sizes using screen ratio.
  IF (Keyword_Set(sizeRatio)) THEN BEGIN
     scrSize = Get_Screen_Size()
     IF (N_Elements(scr_XSize) NE 0) THEN scr_XSize *= scrSize[0]
     IF (N_Elements(scr_YSize) NE 0) THEN scr_YSize *= scrSize[1]
  ENDIF

  ;; Add non-binary keywords to data object.
  IF (N_Elements(scr_xsize) NE 0) THEN input->Add, 'Scr_XSize', scr_xsize
  IF (N_Elements(scr_ysize) NE 0) THEN input->Add, 'Scr_YSize', scr_ysize
  IF (N_Elements(title) NE 0) THEN input->Add, 'Title', title
  IF (N_Elements(xoffset) NE 0) THEN input->Add, 'XOffset', xoffset
  IF (N_Elements(yoffset) NE 0) THEN input->Add, 'YOffset', yoffset
  IF (N_Elements(group_leader) NE 0) THEN $
     input->Add, 'Group_Leader', group_leader
  IF (N_Elements(tlb_frame_attr) NE 0) THEN $
     input->Add, 'TLB_Frame_Attr', tlb_frame_attr

  ;; Create TLB object.
  objRef = (Arg_Present(mBar)) ? $
           Obj_New('Jam_CW_TLB', input, MBar=mBar) : $
           Obj_New('Jam_CW_TLB', input)

  ;; Put object reference in TLB user value.
  Widget_Control, objRef->ID(), Set_UValue=objRef

  ;; Return object reference.
  RETURN, objRef

END
