&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES cb-cfgg
&Scoped-define FIRST-EXTERNAL-TABLE cb-cfgg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR cb-cfgg.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS cb-cfgg.CodCia cb-cfgg.Codcfg ~
cb-cfgg.Descripcion cb-cfgg.Codope cb-cfgg.codaux[1] cb-cfgg.CCo[1] ~
cb-cfgg.codcta[1] cb-cfgg.codaux[2] cb-cfgg.CCo[2] cb-cfgg.codcta[2] ~
cb-cfgg.codaux[3] cb-cfgg.CCo[3] cb-cfgg.codcta[3] cb-cfgg.codaux[4] ~
cb-cfgg.CCo[4] cb-cfgg.codcta[4] cb-cfgg.codaux[5] cb-cfgg.CCo[5] ~
cb-cfgg.codcta[5] cb-cfgg.codaux[6] cb-cfgg.CCo[6] cb-cfgg.codcta[6] ~
cb-cfgg.codaux[7] cb-cfgg.CCo[7] cb-cfgg.codcta[7] cb-cfgg.codaux[8] ~
cb-cfgg.CCo[8] cb-cfgg.codcta[8] cb-cfgg.codaux[9] cb-cfgg.CCo[9] ~
cb-cfgg.codcta[9] cb-cfgg.codaux[10] cb-cfgg.CCo[10] cb-cfgg.codcta[10] 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}CodCia ~{&FP2}CodCia ~{&FP3}~
 ~{&FP1}Codcfg ~{&FP2}Codcfg ~{&FP3}~
 ~{&FP1}Descripcion ~{&FP2}Descripcion ~{&FP3}~
 ~{&FP1}Codope ~{&FP2}Codope ~{&FP3}~
 ~{&FP1}codaux[1] ~{&FP2}codaux[1] ~{&FP3}~
 ~{&FP1}CCo[1] ~{&FP2}CCo[1] ~{&FP3}~
 ~{&FP1}codcta[1] ~{&FP2}codcta[1] ~{&FP3}~
 ~{&FP1}codaux[2] ~{&FP2}codaux[2] ~{&FP3}~
 ~{&FP1}CCo[2] ~{&FP2}CCo[2] ~{&FP3}~
 ~{&FP1}codcta[2] ~{&FP2}codcta[2] ~{&FP3}~
 ~{&FP1}codaux[3] ~{&FP2}codaux[3] ~{&FP3}~
 ~{&FP1}CCo[3] ~{&FP2}CCo[3] ~{&FP3}~
 ~{&FP1}codcta[3] ~{&FP2}codcta[3] ~{&FP3}~
 ~{&FP1}codaux[4] ~{&FP2}codaux[4] ~{&FP3}~
 ~{&FP1}CCo[4] ~{&FP2}CCo[4] ~{&FP3}~
 ~{&FP1}codcta[4] ~{&FP2}codcta[4] ~{&FP3}~
 ~{&FP1}codaux[5] ~{&FP2}codaux[5] ~{&FP3}~
 ~{&FP1}CCo[5] ~{&FP2}CCo[5] ~{&FP3}~
 ~{&FP1}codcta[5] ~{&FP2}codcta[5] ~{&FP3}~
 ~{&FP1}codaux[6] ~{&FP2}codaux[6] ~{&FP3}~
 ~{&FP1}CCo[6] ~{&FP2}CCo[6] ~{&FP3}~
 ~{&FP1}codcta[6] ~{&FP2}codcta[6] ~{&FP3}~
 ~{&FP1}codaux[7] ~{&FP2}codaux[7] ~{&FP3}~
 ~{&FP1}CCo[7] ~{&FP2}CCo[7] ~{&FP3}~
 ~{&FP1}codcta[7] ~{&FP2}codcta[7] ~{&FP3}~
 ~{&FP1}codaux[8] ~{&FP2}codaux[8] ~{&FP3}~
 ~{&FP1}CCo[8] ~{&FP2}CCo[8] ~{&FP3}~
 ~{&FP1}codcta[8] ~{&FP2}codcta[8] ~{&FP3}~
 ~{&FP1}codaux[9] ~{&FP2}codaux[9] ~{&FP3}~
 ~{&FP1}CCo[9] ~{&FP2}CCo[9] ~{&FP3}~
 ~{&FP1}codcta[9] ~{&FP2}codcta[9] ~{&FP3}~
 ~{&FP1}codaux[10] ~{&FP2}codaux[10] ~{&FP3}~
 ~{&FP1}CCo[10] ~{&FP2}CCo[10] ~{&FP3}~
 ~{&FP1}codcta[10] ~{&FP2}codcta[10] ~{&FP3}
&Scoped-define ENABLED-TABLES cb-cfgg
&Scoped-define FIRST-ENABLED-TABLE cb-cfgg
&Scoped-Define ENABLED-OBJECTS RECT-6 
&Scoped-Define DISPLAYED-FIELDS cb-cfgg.CodCia cb-cfgg.Codcfg ~
cb-cfgg.Descripcion cb-cfgg.Codope cb-cfgg.codaux[1] cb-cfgg.CCo[1] ~
cb-cfgg.codcta[1] cb-cfgg.codaux[2] cb-cfgg.CCo[2] cb-cfgg.codcta[2] ~
cb-cfgg.codaux[3] cb-cfgg.CCo[3] cb-cfgg.codcta[3] cb-cfgg.codaux[4] ~
cb-cfgg.CCo[4] cb-cfgg.codcta[4] cb-cfgg.codaux[5] cb-cfgg.CCo[5] ~
cb-cfgg.codcta[5] cb-cfgg.codaux[6] cb-cfgg.CCo[6] cb-cfgg.codcta[6] ~
cb-cfgg.codaux[7] cb-cfgg.CCo[7] cb-cfgg.codcta[7] cb-cfgg.codaux[8] ~
cb-cfgg.CCo[8] cb-cfgg.codcta[8] cb-cfgg.codaux[9] cb-cfgg.CCo[9] ~
cb-cfgg.codcta[9] cb-cfgg.codaux[10] cb-cfgg.CCo[10] cb-cfgg.codcta[10] 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 66 BY 1.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cb-cfgg.CodCia AT ROW 1.27 COL 3.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.86 BY .69
     cb-cfgg.Codcfg AT ROW 1.27 COL 14.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.43 BY .69
     cb-cfgg.Descripcion AT ROW 1.27 COL 22.43 COLON-ALIGNED NO-LABEL FORMAT "X(80)"
          VIEW-AS FILL-IN 
          SIZE 42.29 BY .69
     cb-cfgg.Codope AT ROW 2.65 COL 2.43
          LABEL "Operaciones"
          VIEW-AS FILL-IN 
          SIZE 46 BY .69
     cb-cfgg.codaux[1] AT ROW 3.69 COL 10.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .69
     cb-cfgg.CCo[1] AT ROW 3.69 COL 30.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .69
     cb-cfgg.codcta[1] AT ROW 3.69 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .69
     cb-cfgg.codaux[2] AT ROW 4.5 COL 10.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .69
     cb-cfgg.CCo[2] AT ROW 4.5 COL 30.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .69
     cb-cfgg.codcta[2] AT ROW 4.5 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .69
     cb-cfgg.codaux[3] AT ROW 5.31 COL 10.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .69
     cb-cfgg.CCo[3] AT ROW 5.31 COL 30.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .69
     cb-cfgg.codcta[3] AT ROW 5.31 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .69
     cb-cfgg.codaux[4] AT ROW 6.12 COL 10.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .69
     cb-cfgg.CCo[4] AT ROW 6.12 COL 30.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .69
     cb-cfgg.codcta[4] AT ROW 6.12 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .69
     cb-cfgg.codaux[5] AT ROW 6.92 COL 10.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .69
     cb-cfgg.CCo[5] AT ROW 6.92 COL 30.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .69
     cb-cfgg.codcta[5] AT ROW 6.92 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .69
     cb-cfgg.codaux[6] AT ROW 7.73 COL 10.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .69
     cb-cfgg.CCo[6] AT ROW 7.73 COL 30.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .69
     cb-cfgg.codcta[6] AT ROW 7.73 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .69
     cb-cfgg.codaux[7] AT ROW 8.54 COL 10.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .69
     cb-cfgg.CCo[7] AT ROW 8.54 COL 30.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .69
     cb-cfgg.codcta[7] AT ROW 8.54 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .69
     cb-cfgg.codaux[8] AT ROW 9.35 COL 10.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .69
     cb-cfgg.CCo[8] AT ROW 9.35 COL 30.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .69
     cb-cfgg.codcta[8] AT ROW 9.35 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .69
     cb-cfgg.codaux[9] AT ROW 10.15 COL 10.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .69
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     cb-cfgg.CCo[9] AT ROW 10.15 COL 30.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .69
     cb-cfgg.codcta[9] AT ROW 10.15 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .69
     cb-cfgg.codaux[10] AT ROW 11 COL 10.29 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .69
     cb-cfgg.CCo[10] AT ROW 11 COL 30.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY .69
     cb-cfgg.codcta[10] AT ROW 11 COL 46.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14.43 BY .69
     RECT-6 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: integral.cb-cfgg
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
         HEIGHT             = 11.19
         WIDTH              = 66.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit L-To-R                                       */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cb-cfgg.Codope IN FRAME F-Main
   ALIGN-L EXP-LABEL                                                    */
/* SETTINGS FOR FILL-IN cb-cfgg.Descripcion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "cb-cfgg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "cb-cfgg"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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
  {src/adm/template/snd-list.i "cb-cfgg"}

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


