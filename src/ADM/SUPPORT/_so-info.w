&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME f-dlg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS f-dlg 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation ("PSC"),       *
* 14 Oak Park, Bedford, MA 01730, and other contributors as listed   *
* below.  All Rights Reserved.                                       *
*                                                                    *
* The Initial Developer of the Original Code is PSC.  The Original   *
* Code is Progress IDE code released to open source December 1, 2000.*
*                                                                    *
* The contents of this file are subject to the Possenet Public       *
* License Version 1.0 (the "License"); you may not use this file     *
* except in compliance with the License.  A copy of the License is   *
* available as of the date of this notice at                         *
* http://www.possenet.org/license.html                               *
*                                                                    *
* Software distributed under the License is distributed on an "AS IS"*
* basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. You*
* should refer to the License for the specific language governing    *
* rights and limitations under the License.                          *
*                                                                    *
* Contributors:                                                      *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: _so-info.w

  Description: Show the Supported links, external tables and internal tables 
               for a SmartObject.
               
  Input Parameters:
      p_hSMO - Procedure Handle corresponding to the SmartObject.
      p_cTitle - Title for the dialog-box.
      
  Author: Wm.T.Wood 

  Created: March, 1995

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_is_Running) eq 0 &THEN
  DEFINE INPUT PARAMETER p_hSMO AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p_cTitle AS CHAR NO-UNDO.
&ELSE
  DEFINE VAR p_hSMO AS HANDLE NO-UNDO.
  DEFINE VAR p_cTitle AS CHAR NO-UNDO.
&ENDIF

/* Shared Variable Definitions ---                                      */
&GLOBAL-DEFINE WIN95-BTN YES
{src/adm/support/admhlp.i}   /* Help pre-processor directives    */
{adecomm1/adestds.i}          /* Standard Definitions             */ 

/* Define a SKIP for alert-boxes that only exists under Motif */
&Global-define SKP &IF "{&WINDOW-SYSTEM}" = "OSF/Motif" &THEN SKIP &ELSE &ENDIF

/* Local Variable Definitions --                                        */
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE itbls AS CHAR NO-UNDO.
DEFINE VARIABLE xtbls AS CHAR NO-UNDO.
DEFINE VARIABLE keys-acc AS CHAR NO-UNDO.
DEFINE VARIABLE keys-sup AS CHAR NO-UNDO.

/* Check for an empty title. */
IF p_cTitle eq "":U THEN p_cTitle = "SmartInfo".

/* If the SmartObject is not a real valid object, return after telling
   the user. */
IF NOT (VALID-HANDLE(p_hSMO) AND 
        CAN-DO(p_hSMO:INTERNAL-ENTRIES, 'get-attribute':U))
THEN DO:
  MESSAGE "The SmartObject is invalid or cannot supply" {&SKP}
          "any attribute information about itself." 
          VIEW-AS ALERT-BOX INFORMATION TITLE p_cTitle.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME f-dlg

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_attributes list-links list-xtblist ~
list-tables list-keyacc list-keysup 
&Scoped-Define DISPLAYED-OBJECTS cFilename cType 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_attributes 
     LABEL "&Attributes..." 
     SIZE 15.57 BY 1.12.

DEFINE VARIABLE cFilename AS CHARACTER FORMAT "X(256)":U 
     LABEL "Master File" 
      VIEW-AS TEXT 
     SIZE 41.43 BY .62 NO-UNDO.

DEFINE VARIABLE cType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
      VIEW-AS TEXT 
     SIZE 41.43 BY .62 NO-UNDO.

DEFINE VARIABLE list-keyacc AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 24.57 BY 3.04 NO-UNDO.

DEFINE VARIABLE list-keysup AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 24.57 BY 3.04 NO-UNDO.

DEFINE VARIABLE list-links AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 24.57 BY 6.85 NO-UNDO.

DEFINE VARIABLE list-tables AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 24.57 BY 3 NO-UNDO.

DEFINE VARIABLE list-xtblist AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 24.57 BY 3 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME f-dlg
     btn_attributes AT ROW 1.35 COL 60.43
     list-links AT ROW 4.15 COL 2 NO-LABEL
     list-xtblist AT ROW 4.15 COL 27 NO-LABEL
     list-tables AT ROW 4.15 COL 52 NO-LABEL
     list-keyacc AT ROW 7.92 COL 27 NO-LABEL
     list-keysup AT ROW 7.92 COL 52 NO-LABEL
     cFilename AT ROW 1.31 COL 12 COLON-ALIGNED
     cType AT ROW 2.12 COL 12 COLON-ALIGNED
     "Can Send these Keys:" VIEW-AS TEXT
          SIZE 23.86 BY .54 AT ROW 7.35 COL 52
     "Can Request  these Keys:" VIEW-AS TEXT
          SIZE 25 BY .54 AT ROW 7.35 COL 27
     "in these Tables:" VIEW-AS TEXT
          SIZE 23.86 BY .54 AT ROW 3.54 COL 52
     "for these Tables:" VIEW-AS TEXT
          SIZE 23.86 BY .54 AT ROW 3.54 COL 27
     "ADM Supported Links:" VIEW-AS TEXT
          SIZE 24.57 BY .65 AT ROW 3.35 COL 2
     "Can Send Records" VIEW-AS TEXT
          SIZE 23.14 BY .77 AT ROW 2.69 COL 52
     "Requires Record-Source" VIEW-AS TEXT
          SIZE 23.86 BY .77 AT ROW 2.69 COL 27
     SPACE(26.56) SKIP(7.53)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "SmartInfo".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX f-dlg
   FRAME-NAME                                                           */
ASSIGN 
       FRAME f-dlg:SCROLLABLE       = FALSE
       FRAME f-dlg:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cFilename IN FRAME f-dlg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cType IN FRAME f-dlg
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST list-keyacc IN FRAME f-dlg
   NO-DISPLAY                                                           */
/* SETTINGS FOR SELECTION-LIST list-keysup IN FRAME f-dlg
   NO-DISPLAY                                                           */
/* SETTINGS FOR SELECTION-LIST list-links IN FRAME f-dlg
   NO-DISPLAY                                                           */
/* SETTINGS FOR SELECTION-LIST list-tables IN FRAME f-dlg
   NO-DISPLAY                                                           */
/* SETTINGS FOR SELECTION-LIST list-xtblist IN FRAME f-dlg
   NO-DISPLAY                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX f-dlg
/* Query rebuild information for DIALOG-BOX f-dlg
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX f-dlg */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME f-dlg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-dlg f-dlg
ON WINDOW-CLOSE OF FRAME f-dlg /* SmartInfo */
DO:
  /* Close the dialog-box */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_attributes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_attributes f-dlg
ON CHOOSE OF btn_attributes IN FRAME f-dlg /* Attributes... */
DO:
  /* Show the standard SmartObject attribute viewer. */
  RUN adm/support/_so-attr.w (INPUT p_hSMO) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK f-dlg 


/* This file should only be run in a GUI environment. */
&IF "{&WINDOW-SYSTEM}" eq "TTY" &THEN
  MESSAGE "SmartInfo is not available in Character mode."
          VIEW-AS ALERT-BOX INFORMATION TITLE "SmartInfo":U.
&ELSE
  /* *****************  Standard Buttons and Dialog Setup *************** */
  
  /* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
  IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
  THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.
  
  /* standard button rectangle */
  DEFINE RECTANGLE rect_btn_bar {&STDPH_OKBOX}.
  
  /* Buttons for the bottom of the screen                                 */
  DEFINE BUTTON Btn_Close  LABEL "&Close":C12 {&STDPH_OKBTN} AUTO-GO.
  DEFINE BUTTON Btn_Help   LABEL "&Help":C12  {&STDPH_OKBTN}.
  
  /* Add these to the frame */
  DEFINE FRAME {&FRAME-NAME} 
    {adecomm1/okform.i &OK   = "Btn_Close"
                      &HELP = "Btn_Help"
                      &BOX  = "rect_btn_bar" } 
                      .
    
  /* Do run-time layout.  ok_run.i lays out width.  We need to also do 
     HEIGHT-P. */
  {adecomm1/okrun.i 
    &FRAME = "FRAME {&FRAME-NAME}"
    &OK    = Btn_Close
    &Help  = Btn_Help 
    &BOX   = rect_btn_bar 
  }
  
  /* Adjust the height based on the position of the OK button and the relevent
     borders (standard borders plus dialog-box border). */
  ASSIGN FRAME {&FRAME-NAME}:SCROLLABLE = no
         FRAME {&FRAME-NAME}:HEIGHT     = (Btn_Close:ROW  - 1) + Btn_Close:HEIGHT + 
         FRAME {&FRAME-NAME}:BORDER-TOP + FRAME {&FRAME-NAME}:BORDER-BOTTOM +
         /* if there is an ok box, skip to allow for it below the buttons */
         &IF {&OKBOX} &THEN {&IVM_OKBOX} + &ENDIF
         /* skip a margin below the box or the buttons */
         {&VM_OKBOX}
         .
  /* Add a standard ADE help trigger */
  ON CHOOSE OF Btn_Help OR HELP OF FRAME {&FRAME-NAME} DO:
     RUN adecomm/_adehelp.p ("AB":U, "CONTEXT":U, {&SmartInfo_Dlg_Box}, "").
  END.
  
  /* Enable these new buttons */
  ENABLE Btn_Close Btn_Help WITH FRAME {&FRAME-NAME}.
  FRAME {&FRAME-NAME}:DEFAULT-BUTTON = btn_Close:HANDLE.
  
  /* ***************************  Main Block  *************************** */
  
  /* Populate the selection lists from the data in the object.
     Special Cases -- blank out the table lists if we not a RECORD-SOURCE or TARGET. */
  
  RUN get-attribute IN p_hSMO ('TYPE':U).
  ASSIGN cType     = RETURN-VALUE
         cFilename = p_hSMO:FILE-NAME
         FRAME {&FRAME-NAME}:TITLE = p_cTitle
         .
         
  /* Get attributes for the object. */
  RUN get-attribute IN p_hSMO ('Supported-Links':U).
  list-links:LIST-ITEMS = RETURN-VALUE.
  RUN get-attribute IN p_hSMO ('External-Tables':U).
  xtbls = REPLACE(RETURN-VALUE, ' ':U, ',':U).
  RUN get-attribute IN p_hSMO ('Internal-Tables':U).
  itbls = REPLACE(RETURN-VALUE, ' ':U, ',':U).
  RUN get-attribute IN p_hSMO ('Keys-Accepted':U).
  keys-acc = REPLACE(RETURN-VALUE, ' ':U, ',':U).
  RUN get-attribute IN p_hSMO ('Keys-Supplied':U).
  keys-sup = REPLACE(RETURN-VALUE, ' ':U, ',':U).
  
  /* External tables and Keys-Accpeted are needed by Record-Targets. */
  IF list-links:LOOKUP ('Record-Target':U) > 0 
  THEN ASSIGN list-xtblist:LIST-ITEMS = xtbls
              list-keyacc:LIST-ITEMS  = keys-acc.
  
  /* Record-Sources can supply the external tables passed in, plus any internal
     tables found. Keys supplied are explicit. */
  IF list-links:LOOKUP ('Record-Source':U) > 0 THEN DO:
    list-tables:LIST-ITEMS = xtbls.
    IF itbls ne '':U THEN ldummy = list-tables:ADD-LAST (itbls).
    list-keysup:LIST-ITEMS = keys-sup.
  END.
  
  /* Don't show empty lists */
  IF list-xtblist:NUM-ITEMS eq 0 THEN list-xtblist:LIST-ITEMS = "[none required]".
  IF list-tables:NUM-ITEMS eq 0 THEN list-tables:LIST-ITEMS = "[none sent]".
  IF list-keyacc:NUM-ITEMS eq 0 THEN list-keyacc:LIST-ITEMS = "[none accepted]".
  IF list-keysup:NUM-ITEMS eq 0 THEN list-keysup:LIST-ITEMS = "[none sent]".
  
  /* Now enable the interface and wait for the exit condition.            */
  /* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
  MAIN-BLOCK:
  DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
     ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /* Enable the attribute viewer if it can be found. */
    RUN enable_UI.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  END.
  RUN disable_UI.

&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI f-dlg  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME f-dlg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI f-dlg  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cFilename cType 
      WITH FRAME f-dlg.
  ENABLE btn_attributes list-links list-xtblist list-tables list-keyacc 
         list-keysup 
      WITH FRAME f-dlg.
  VIEW FRAME f-dlg.
  {&OPEN-BROWSERS-IN-QUERY-f-dlg}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

