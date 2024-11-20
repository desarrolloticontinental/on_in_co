&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME SmartTab95-Dlg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS SmartTab95-Dlg 
/*------------------------------------------------------------------------

  File: tab95d.w 

  Description: Dialog for getting settable attributes for a SmartTab95.

  Input Parameters:
      Handle of calling SmartTab95.

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER p-Parent-Hdl AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE attr-list  AS CHARACTER NO-UNDO.
DEFINE VARIABLE attr-entry AS CHARACTER NO-UNDO.
DEFINE VARIABLE attr-value AS CHARACTER NO-UNDO.
DEFINE VARIABLE attr-name  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cntr       AS INTEGER   NO-UNDO.
DEFINE VARIABLE F-Labels   AS CHARACTER NO-UNDO.
DEFINE VARIABLE sts        AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE tab-label
       FIELD      tab-number AS INTEGER  FORMAT ">9":U
       FIELD      tab-value  AS CHARACTER FORMAT "X(30)":U
       INDEX      tab-number tab-number.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME SmartTab95-Dlg
&Scoped-define BROWSE-NAME Tab-Browse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tab-label

/* Definitions for BROWSE Tab-Browse                                    */
&Scoped-define FIELDS-IN-QUERY-Tab-Browse tab-label.tab-number tab-label.tab-value   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Tab-Browse tab-label.tab-value   
&Scoped-define ENABLED-TABLES-IN-QUERY-Tab-Browse tab-label
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Tab-Browse tab-label
&Scoped-define SELF-NAME Tab-Browse
&Scoped-define QUERY-STRING-Tab-Browse FOR EACH tab-label NO-LOCK     BY tab-label.tab-number
&Scoped-define OPEN-QUERY-Tab-Browse OPEN QUERY Tab-Browse FOR EACH tab-label NO-LOCK     BY tab-label.tab-number.
&Scoped-define TABLES-IN-QUERY-Tab-Browse tab-label
&Scoped-define FIRST-TABLE-IN-QUERY-Tab-Browse tab-label


/* Definitions for DIALOG-BOX SmartTab95-Dlg                            */
&Scoped-define OPEN-BROWSERS-IN-QUERY-SmartTab95-Dlg ~
    ~{&OPEN-QUERY-Tab-Browse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS F-Font F-LColor F-Color F-PColor Tab-Browse ~
Btn-Add Btn-Remove 
&Scoped-Define DISPLAYED-OBJECTS F-Font F-LColor F-Color F-PColor 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Add 
     LABEL "&Insert" 
     SIZE 9.72 BY 1.

DEFINE BUTTON Btn-Remove 
     LABEL "&Remove" 
     SIZE 9.72 BY 1.

DEFINE VARIABLE F-Color AS INTEGER FORMAT ">9":U INITIAL 8 
     LABEL "Folder BGColor" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85 NO-UNDO.

DEFINE VARIABLE F-Font AS INTEGER FORMAT ">9":U INITIAL 4 
     LABEL "Tab Label Font" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85 NO-UNDO.

DEFINE VARIABLE F-LColor AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Label FGColor" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85 NO-UNDO.

DEFINE VARIABLE F-PColor AS INTEGER FORMAT ">9":U INITIAL 8 
     LABEL "Folder Parent Frame BGColor" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .85 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Tab-Browse FOR 
      tab-label SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Tab-Browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Tab-Browse SmartTab95-Dlg _FREEFORM
  QUERY Tab-Browse NO-LOCK DISPLAY
      tab-label.tab-number COLUMN-LABEL "tab#"
      tab-label.tab-value COLUMN-LABEL "Label" 
          WIDTH-PIXELS 139
  ENABLE
      tab-label.tab-value
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 27 BY 5.65
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME SmartTab95-Dlg
     F-Font AT ROW 1.27 COL 21 COLON-ALIGNED
     F-LColor AT ROW 2.15 COL 21 COLON-ALIGNED
     F-Color AT ROW 3.42 COL 21 COLON-ALIGNED
     F-PColor AT ROW 4.31 COL 21 COLON-ALIGNED
     Tab-Browse AT ROW 6.12 COL 3
     Btn-Add AT ROW 6.12 COL 32
     Btn-Remove AT ROW 7.46 COL 32
     "Tab Labels:" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 5.31 COL 3
     SPACE(29.13) SKIP(6.02)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER NO-HELP 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "SmartTab95 Attributes":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX SmartTab95-Dlg
   FRAME-NAME                                                           */
/* BROWSE-TAB Tab-Browse F-PColor SmartTab95-Dlg */
ASSIGN 
       FRAME SmartTab95-Dlg:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX SmartTab95-Dlg
/* Query rebuild information for DIALOG-BOX SmartTab95-Dlg
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX SmartTab95-Dlg */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Tab-Browse
/* Query rebuild information for BROWSE Tab-Browse
     _START_FREEFORM
OPEN QUERY Tab-Browse FOR EACH tab-label NO-LOCK
    BY tab-label.tab-number.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* BROWSE Tab-Browse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Add SmartTab95-Dlg
ON CHOOSE OF Btn-Add IN FRAME SmartTab95-Dlg /* Insert */
DO:
  RUN insert-tab("AFTER":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Remove SmartTab95-Dlg
ON CHOOSE OF Btn-Remove IN FRAME SmartTab95-Dlg /* Remove */
DO:
  DEFINE VARIABLE deleted-tab AS INTEGER NO-UNDO.
  /* Don't actually delete the first tab. */
  IF NUM-RESULTS("{&BROWSE-NAME}") = 1 THEN 
  DO:
    tab-label.tab-value:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = "":U.   
    ASSIGN tab-label.tab-value
           Btn-Remove:SENSITIVE = no.
  END.
  ELSE 
  DO:
    deleted-tab = tab-label.tab-number.
    DO TRANSACTION:
      FIND CURRENT tab-label EXCLUSIVE-LOCK.
      DELETE tab-label.
      REPEAT PRESELECT EACH tab-label  /* avoid finding same rec twice */
        WHERE tab-label.tab-number > deleted-tab:
           FIND NEXT tab-label.
           tab-label.tab-number = tab-label.tab-number - 1.
      END.
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
  END.
  APPLY "ENTRY":U TO tab-label.tab-value IN BROWSE Tab-Browse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Tab-Browse
&Scoped-define SELF-NAME Tab-Browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tab-Browse SmartTab95-Dlg
ON OFF-END OF Tab-Browse IN FRAME SmartTab95-Dlg
DO:
  IF LAST-EVENT:LABEL = "OFF-END" THEN
         RUN insert-tab ("AFTER":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tab-Browse SmartTab95-Dlg
ON OFF-HOME OF Tab-Browse IN FRAME SmartTab95-Dlg
DO:
  IF LAST-EVENT:LABEL = "OFF-HOME" THEN
         RUN insert-tab ("BEFORE":U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tab-Browse SmartTab95-Dlg
ON ROW-LEAVE OF Tab-Browse IN FRAME SmartTab95-Dlg
DO:
  DEFINE VARIABLE last-hdl AS HANDLE NO-UNDO.
  last-hdl = LAST-EVENT:WIDGET-ENTER.
  IF LOOKUP (LAST-EVENT:LABEL, "CURSOR-UP,CURSOR-DOWN,ENTER,TAB":U) NE 0
     OR (LAST-EVENT:LABEL = "ROW-LEAVE" AND VALID-HANDLE(last-hdl) AND
         (last-hdl:NAME = "Btn-Add":U OR last-hdl:NAME = "btn_OK":U)) THEN 
  DO:
     IF INDEX (Tab-Label.Tab-Value:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
          "|":U) NE 0 OR
        INDEX (Tab-Label.Tab-Value:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
          ",":U) NE 0 THEN 
     DO:
          MESSAGE "Invalid character in tab label."
              VIEW-AS ALERT-BOX WARNING.
          RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK SmartTab95-Dlg 


/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Define Context ID's for HELP files */
{ adm/support/admhlp.i }    

/* Attach the standard OK/Cancel/Help button bar. */
{ adecomm1/okbar.i }

ON CHOOSE OF btn_help OR HELP OF FRAME {&FRAME-NAME}
DO:
  DEFINE VARIABLE help-file AS CHARACTER INITIAL "adm-free\support\Tab95hlp.hlp".
  help-file = SEARCH(help-file).
  IF help-file = ? THEN
    MESSAGE "adm-free\support\Tab95hlp.hlp not found !"
      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  ELSE
    SYSTEM-HELP help-file CONTEXT 4.
END.


/* ***************************  Main Block  *************************** */
FRAME {&FRAME-NAME}:DEFAULT-BUTTON = btn_OK:HANDLE.

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Keep newly added entries from being at the top of the viewport. */
sts = {&BROWSE-NAME}:SET-REPOSITIONED-ROW
    ({&BROWSE-NAME}:DOWN,"ALWAYS":U). 
    
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   RUN get-attribute-list IN p-Parent-Hdl (OUTPUT attr-list).     
   DO cntr = 1 TO NUM-ENTRIES(attr-list): 
      attr-entry = ENTRY(cntr, attr-list).
      attr-name = TRIM(SUBSTR(attr-entry, 1, INDEX(attr-entry,"=":U) - 1,
          "CHARACTER":U)).
      attr-value = TRIM(SUBSTR(attr-entry, INDEX(attr-entry,"=":U) + 1,
          -1, "CHARACTER":U)).
      CASE attr-name :
      WHEN "LABEL-FONT":U THEN
          F-Font = INT(attr-value).
      WHEN "LABEL-FGCOLOR":U THEN
          F-LColor = INT(attr-value).
      WHEN "FOLDER-BGCOLOR":U THEN
          F-Color = INT(attr-value).
      WHEN "FOLDER-PARENT-BGCOLOR":U THEN
          F-PColor = INT(attr-value).
      WHEN "LABELS":U THEN
          F-Labels = attr-value.
      END CASE.
  END.
  
  IF F-Labels = "":U THEN   /* Give browser a starting point if no labels. */
  DO TRANSACTION:
      CREATE tab-label.
      ASSIGN tab-label.tab-number = 1.
      
  END.  
  ELSE DO TRANSACTION cntr = 1 TO NUM-ENTRIES(F-Labels,'|':U):
      CREATE tab-label.
      ASSIGN tab-label.tab-number = cntr
             tab-label.tab-value = ENTRY(cntr, F-Labels, '|':U).
  END.
      
  RUN enable_UI.
  
  IF F-Labels = "":U THEN
      Btn-Remove:SENSITIVE IN FRAME {&FRAME-NAME} = no.
      
  /* Set the cursor */
  RUN adecomm/_setcurs.p ("":U). 
  
  APPLY "ENTRY":U TO tab-label.tab-value IN BROWSE Tab-Browse.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.  
  ASSIGN
    F-Font
    F-LColor
    F-Color
    F-PColor.
  CLOSE QUERY {&BROWSE-NAME}.
  F-Labels = "":U.
  FOR EACH tab-label BY tab-number:
      IF tab-label.tab-number NE 1 THEN F-Labels = F-Labels + '|':U.
      F-Labels = F-labels + TRIM(tab-label.tab-value).
  END.
   
  /* If the first item in the list is empty, suppress it. */
  IF SUBSTR(F-Labels,1,1, "CHARACTER":U) = '|':U THEN
      F-Labels = SUBSTR(F-Labels,2, -1, "CHARACTER":U).
         
  /* If the last item in the list is empty, suppress the extra tab. */
  IF R-INDEX(F-Labels,'|':U) = LENGTH(F-Labels, "CHARACTER":U) THEN 
      F-Labels = SUBSTR(F-Labels,1,LENGTH(F-Labels, "CHARACTER":U) - 1,
          "CHARACTER":U).
      
  attr-list = "LABEL-FONT = ":U + STRING(F-Font) + 
              ",LABEL-FGCOLOR = ":U + STRING(F-LColor) + 
              ",FOLDER-BGCOLOR = ":U + STRING(F-Color) + 
              ",FOLDER-PARENT-BGCOLOR = ":U + STRING(F-PColor) + 
              ",LABELS = ":U + F-Labels.

  RUN set-attribute-list IN p-Parent-Hdl (INPUT attr-list).
  RUN initialize-folder IN p-Parent-Hdl.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI SmartTab95-Dlg  _DEFAULT-DISABLE
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
  HIDE FRAME SmartTab95-Dlg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI SmartTab95-Dlg  _DEFAULT-ENABLE
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
  DISPLAY F-Font F-LColor F-Color F-PColor 
      WITH FRAME SmartTab95-Dlg.
  ENABLE F-Font F-LColor F-Color F-PColor Tab-Browse Btn-Add Btn-Remove 
      WITH FRAME SmartTab95-Dlg.
  {&OPEN-BROWSERS-IN-QUERY-SmartTab95-Dlg}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE insert-tab SmartTab95-Dlg 
PROCEDURE insert-tab :
/*------------------------------------------------------------------------------
  Purpose:  Allows insertion of a new folder tab.   
  Parameters:  BEFORE or AFTER
  Notes:    AFTER is used for the Insert button (Btn_Add) and 
            OFF-END and ENTER. BEFORE is used for OFF-HOME (so only
            applies for inserting before the first existing tab).   
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER insert-direction AS CHARACTER NO-UNDO.
  DEFINE VARIABLE tab#      AS INTEGER NO-UNDO.
  DEFINE VARIABLE tab-rowid AS ROWID   NO-UNDO.
  
  ASSIGN tab# = IF insert-direction = "BEFORE":U THEN 0
                ELSE tab-label.tab-number.   /* save off the current tab # */
  FIND LAST tab-label.
  IF tab-label.tab-number >= 20 THEN    /* 20 is max-labels in folder.w */
    MESSAGE "Maximum of 20 labels allows in a tab folder."
        VIEW-AS ALERT-BOX WARNING.
  ELSE DO:
    DO TRANSACTION:
       /* Use PRESELECT to avoid finding the same rec twice. */
      REPEAT PRESELECT EACH tab-label WHERE tab-label.tab-number > tab#:
          FIND NEXT tab-label.  
          ASSIGN tab-label.tab-number = tab-label.tab-number + 1.
      END.
      CREATE tab-label.
      ASSIGN tab-label.tab-number = tab# + 1
                             tab# = tab-label.tab-number.
    END.

    {&OPEN-QUERY-{&BROWSE-NAME}}
 
    FIND tab-label WHERE tab-label.tab-number = tab#.
    ASSIGN tab-rowid = ROWID(tab-label).
    REPOSITION {&BROWSE-NAME} TO ROWID tab-rowid.
    Btn-Remove:SENSITIVE IN FRAME {&FRAME-NAME} = yes.
    APPLY "ENTRY":U TO tab-label.tab-value IN BROWSE {&BROWSE-NAME}.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

