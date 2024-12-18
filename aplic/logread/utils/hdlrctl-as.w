&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF OUTPUT PARAM clogtype AS CHAR NO-UNDO.
DEF OUTPUT PARAM ctypename AS CHAR NO-UNDO.
DEF OUTPUT PARAM chidcols AS CHAR NO-UNDO.
DEF OUTPUT PARAM cmrgflds AS CHAR NO-UNDO.
DEF OUTPUT PARAM cnomrgflds AS CHAR NO-UNDO.
DEF OUTPUT PARAM lOK AS LOGICAL INIT NO NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tLogType tTypeName thidcols tdatefld ~
trawtimefld tmsgfld tchartimefld tnomrgflds Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tLogType tTypeName thidcols tdatefld ~
trawtimefld tmsgfld tchartimefld tnomrgflds 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE tchartimefld AS CHARACTER FORMAT "X(256)":U 
     LABEL "Char Time Field" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tdatefld AS CHARACTER FORMAT "X(256)":U 
     LABEL "Date Field" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE thidcols AS CHARACTER FORMAT "X(256)":U 
     LABEL "Hidden Columns" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE tLogType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Log Type" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE tmsgfld AS CHARACTER FORMAT "X(256)":U 
     LABEL "Message Field" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tnomrgflds AS CHARACTER FORMAT "X(256)":U 
     LABEL "Non-Merge Fields" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE trawtimefld AS CHARACTER FORMAT "X(256)":U 
     LABEL "Raw Time Field" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tTypeName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     tLogType AT ROW 1.24 COL 19 COLON-ALIGNED
     tTypeName AT ROW 2.43 COL 19 COLON-ALIGNED
     thidcols AT ROW 3.62 COL 19 COLON-ALIGNED
     tdatefld AT ROW 4.81 COL 36 COLON-ALIGNED
     trawtimefld AT ROW 6 COL 36 COLON-ALIGNED
     tmsgfld AT ROW 7.19 COL 36 COLON-ALIGNED
     tchartimefld AT ROW 8.38 COL 36 COLON-ALIGNED
     tnomrgflds AT ROW 9.57 COL 19 COLON-ALIGNED
     Btn_OK AT ROW 11 COL 17
     Btn_Cancel AT ROW 11 COL 33
     "Merge fields:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 5 COL 8
     SPACE(44.13) SKIP(6.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Add Pseudo Type"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Add Pseudo Type */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN 
      tlogtype
      ttypename
      thidcols
      tdatefld
      trawtimefld
      tmsgfld
      tchartimefld
      tnomrgflds.
  ASSIGN 
      clogtype = tlogtype
      ctypename = ttypename
      chidcols = thidcols
      cmrgflds = tdatefld + "," + trawtimefld + "," + tmsgfld + "," + tchartimefld
      cnomrgflds = tnomrgflds
      lOK = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY tLogType tTypeName thidcols tdatefld trawtimefld tmsgfld tchartimefld 
          tnomrgflds 
      WITH FRAME Dialog-Frame.
  ENABLE tLogType tTypeName thidcols tdatefld trawtimefld tmsgfld tchartimefld 
         tnomrgflds Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

