&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r2 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

  Description: from VIEWER.W - Template for SmartViewer Objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE V-ROWID       AS ROWID   EXTENT 20 NO-UNDO.
DEFINE VARIABLE ULT-LINE      AS INTEGER NO-UNDO.
DEFINE VARIABLE ACT-LINE      AS INTEGER NO-UNDO.
DEFINE VARIABLE COLOR-BG-NOR  AS INTEGER NO-UNDO.
DEFINE VARIABLE COLOR-FG-NOR  AS INTEGER NO-UNDO.
DEFINE VARIABLE COLOR-BG-BAR  AS INTEGER NO-UNDO.
DEFINE VARIABLE COLOR-FG-BAR  AS INTEGER NO-UNDO.
DEFINE VARIABLE GRP           AS WIDGET-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer

/* Definitions for FRAME F-Main                                         */
&Scoped-define FIELDS-IN-QUERY-F-Main Customer.Cust-Num Customer.Name ~
Customer.Address 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F-Main Customer.Cust-Num ~
Customer.Name Customer.Address 
&Scoped-define ENABLED-TABLES-IN-QUERY-F-Main Customer
&Scoped-define FIELD-PAIRS-IN-QUERY-F-Main ~
      ~{&FP1}Cust-Num       ~{&FP2}Cust-Num        ~{&FP3} ~
      ~{&FP1}Name           ~{&FP2}Name            ~{&FP3} ~
      ~{&FP1}Address        ~{&FP2}Address         ~{&FP3}
&Scoped-define OPEN-QUERY-F-Main OPEN QUERY F-Main FOR EACH Customer NO-LOCK INDEXED-REPOSITION.
&Scoped-define FIRST-TABLE-IN-QUERY-F-Main Customer
&Scoped-define TABLES-IN-QUERY-F-Main Customer

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Customer.Cust-Num Customer.Name ~
Customer.Address 
&Scoped-define FIELD-PAIRS ~
      ~{&FP1}Cust-Num       ~{&FP2}Cust-Num        ~{&FP3} ~
      ~{&FP1}Name           ~{&FP2}Name            ~{&FP3} ~
      ~{&FP1}Address        ~{&FP2}Address         ~{&FP3}
&Scoped-Define ENABLED-TABLES Customer
&Scoped-Define DISPLAYED-FIELDS Customer.Cust-Num Customer.Name ~
Customer.Address 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS Customer.Cust-Num 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F-Main FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Customer.Cust-Num AT ROW 1 COL 1
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
     Customer.Name AT ROW 1 COL 8
          VIEW-AS FILL-IN 
          SIZE 21.43 BY .81
     Customer.Address AT ROW 1 COL 31
          VIEW-AS FILL-IN 
          SIZE 21.43 BY .81
    WITH DOWN KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 0 FONT 4.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 5.15
         WIDTH              = 55.57.
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE UNDERLINE Size-to-Fit                                    */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Customer.Address IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN Customer.Cust-Num IN FRAME F-Main
   ALIGN-L 1                                                            */
/* SETTINGS FOR FILL-IN Customer.Name IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _TblList          = "sports.Customer"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON END OF FRAME F-Main
ANYWHERE
DO:
  RUN dispatch IN THIS-PROCEDURE ('end-page':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HOME OF FRAME F-Main
ANYWHERE
DO:
  RUN dispatch IN THIS-PROCEDURE ('home-page':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON PAGE-DOWN OF FRAME F-Main
ANYWHERE
DO:
  RUN dispatch IN THIS-PROCEDURE ('page-down':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON PAGE-UP OF FRAME F-Main
ANYWHERE
DO:
  RUN dispatch IN THIS-PROCEDURE ('page-up':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  ASSIGN FRAME {&FRAME-NAME}:DOWN = 6.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-end-page V-table-Win 
PROCEDURE adm-end-page :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  GET LAST {&FRAME-NAME}.
  DO ACT-LINE = FRAME {&FRAME-NAME}:DOWN TO 2 BY -1:
    IF QUERY-OFF-END("{&FRAME-NAME}") THEN LEAVE.
    GET PREV {&FRAME-NAME}.
  END.
  RUN adm-print-page.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-home-page V-table-Win 
PROCEDURE adm-home-page :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  GET FIRST {&FRAME-NAME}.
  RUN adm-print-page.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-page-down V-table-Win 
PROCEDURE adm-page-down :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT QUERY-OFF-END("{&FRAME-NAME}")
  THEN RUN adm-print-page.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-page-up V-table-Win 
PROCEDURE adm-page-up :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF V-ROWID[ 1 ] = ?
  THEN DO:
    BELL.
    RETURN.
  END.
  REPOSITION {&FRAME-NAME} TO ROWID V-ROWID[ 1 ].
  GET NEXT {&FRAME-NAME}.
  DO ACT-LINE = FRAME {&FRAME-NAME}:DOWN TO 1 BY -1:
    IF QUERY-OFF-END("{&FRAME-NAME}") THEN LEAVE.
    GET PREV {&FRAME-NAME}.
  END.
  IF QUERY-OFF-END("{&FRAME-NAME}") THEN GET FIRST {&FRAME-NAME}.
  RUN adm-print-page.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-print-page V-table-Win 
PROCEDURE adm-print-page :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE s-ok-status AS LOGICAL NO-UNDO.
ASSIGN V-ROWID = ?.

DO WITH FRAME {&FRAME-NAME}:
    PAUSE 0.
    IF ULT-LINE <> ?
    THEN DOWN ( FRAME-DOWN - FRAME-LINE + 1).

    DO i = 1 TO FRAME-DOWN:
        IF NOT QUERY-OFF-END("{&FRAME-NAME}")
        THEN 
          DO:
            V-ROWID[ i ] = ROWID( {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}} ).
            ULT-LINE     = i.
            RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
          END.  
        ELSE LEAVE.
        GET NEXT {&FRAME-NAME}.
        DOWN.
    END.

    IF NOT QUERY-OFF-END("{&FRAME-NAME}")
    THEN DO:
        V-ROWID[ ULT-LINE + 1 ] = ROWID( {&FIRST-TABLE-IN-QUERY-{&FRAME-NAME}} ).
    END.
    GRP = FRAME {&FRAME-NAME}:FIRST-CHILD.
    IF FRAME-LINE > 1
    THEN UP FRAME-LINE - 1.
    
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the record-
               source has a new row available.  This procedure
               tries to get the new row and display it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* ADM-ROW-AVAILABLE does nothing because there are no External
     Tables specified for this SmartViewer.
     (See 'Procedure Settings' dialog to set External Tables.) */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  DISPLAY {&FIELDS-IN-QUERY-{&FRAME-NAME}} WITH FRAME {&FRAME-NAME}.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'print-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  CHOOSE ROW {&ADM-CREATE-FIELDS}
        NO-ERROR GO-ON(CURSOR-RIGHT) WITH FRAME {&FRAME-NAME}.
  message "estoy aqui" view-as alert-box.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query V-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */

  {&OPEN-QUERY-{&FRAME-NAME}}
  GET FIRST {&FRAME-NAME}.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Customer"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


