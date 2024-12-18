&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  

  Description: from BROWSER.W - Basic SmartBrowser Object Template

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
&SCOPED-DEFINE CONDICION ( PL-FLG-MES.codpln = PL-PLAN.codpln AND PL-FLG-MES.Codcia = s-codcia AND PL-FLG-MES.Periodo = s-periodo AND PL-FLG-MES.Nromes = s-nromes )

{bin/s-global.i}
{pln/s-global.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES integral.PL-PLAN
&Scoped-define FIRST-EXTERNAL-TABLE integral.PL-PLAN


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR integral.PL-PLAN.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES INTEGRAL.PL-FLG-MES INTEGRAL.PL-PERS

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table integral.PL-FLG-MES.seccion ~
integral.PL-FLG-MES.codper integral.PL-PERS.patper integral.PL-PERS.matper ~
integral.PL-PERS.nomper 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table integral.PL-FLG-MES.seccion 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table integral.PL-FLG-MES
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table integral.PL-FLG-MES
&Scoped-define QUERY-STRING-br_table FOR EACH INTEGRAL.PL-FLG-MES WHERE PL-FLG-MES.codpln = PL-PLAN.codpln ~
      AND PL-FLG-MES.CodCia = s-codcia AND ~
   PL-FLG-MES.Periodo = s-periodo AND ~
      PL-FLG-MES.NROMES = s-nromes  NO-LOCK, ~
      EACH INTEGRAL.PL-PERS WHERE PL-PERS.codper = PL-FLG-MES.codper NO-LOCK ~
    BY INTEGRAL.PL-PERS.patper ~
       BY INTEGRAL.PL-PERS.matper ~
        BY INTEGRAL.PL-PERS.nomper
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH INTEGRAL.PL-FLG-MES WHERE PL-FLG-MES.codpln = PL-PLAN.codpln ~
      AND PL-FLG-MES.CodCia = s-codcia AND ~
   PL-FLG-MES.Periodo = s-periodo AND ~
      PL-FLG-MES.NROMES = s-nromes  NO-LOCK, ~
      EACH INTEGRAL.PL-PERS WHERE PL-PERS.codper = PL-FLG-MES.codper NO-LOCK ~
    BY INTEGRAL.PL-PERS.patper ~
       BY INTEGRAL.PL-PERS.matper ~
        BY INTEGRAL.PL-PERS.nomper.
&Scoped-define TABLES-IN-QUERY-br_table INTEGRAL.PL-FLG-MES ~
INTEGRAL.PL-PERS
&Scoped-define FIRST-TABLE-IN-QUERY-br_table INTEGRAL.PL-FLG-MES
&Scoped-define SECOND-TABLE-IN-QUERY-br_table INTEGRAL.PL-PERS


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS F-Codper F-DesPer br_table 
&Scoped-Define DISPLAYED-OBJECTS F-Codper F-DesPer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE F-Codper AS CHARACTER FORMAT "X(256)":U 
     LABEL "Codigo" 
     VIEW-AS FILL-IN 
     SIZE 8.72 BY .69 NO-UNDO.

DEFINE VARIABLE F-DesPer AS CHARACTER FORMAT "X(256)":U 
     LABEL "Apellido" 
     VIEW-AS FILL-IN 
     SIZE 37 BY .69 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      INTEGRAL.PL-FLG-MES, 
      INTEGRAL.PL-PERS SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      integral.PL-FLG-MES.seccion FORMAT "X(30)":U WIDTH 28
      integral.PL-FLG-MES.codper FORMAT "X(7)":U WIDTH 6.43
      integral.PL-PERS.patper FORMAT "X(40)":U WIDTH 13.86
      integral.PL-PERS.matper FORMAT "X(40)":U WIDTH 14.43
      integral.PL-PERS.nomper FORMAT "X(40)":U WIDTH 18.43
  ENABLE
      integral.PL-FLG-MES.seccion
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 89 BY 6.62
         BGCOLOR 15 FGCOLOR 0 FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     F-Codper AT ROW 1.31 COL 5.72 COLON-ALIGNED
     F-DesPer AT ROW 1.31 COL 23 COLON-ALIGNED
     br_table AT ROW 2.19 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: integral.PL-PLAN
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 8.58
         WIDTH              = 106.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table F-DesPer F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.PL-FLG-MES WHERE INTEGRAL.PL-PLAN ...,INTEGRAL.PL-PERS WHERE INTEGRAL.PL-FLG-MES ..."
     _Options          = "NO-LOCK"
     _OrdList          = "INTEGRAL.PL-PERS.patper|yes,INTEGRAL.PL-PERS.matper|yes,INTEGRAL.PL-PERS.nomper|yes"
     _JoinCode[1]      = "PL-FLG-MES.codpln = PL-PLAN.codpln"
     _Where[1]         = "PL-FLG-MES.CodCia = s-codcia AND
   PL-FLG-MES.Periodo = s-periodo AND
      PL-FLG-MES.NROMES = s-nromes "
     _JoinCode[2]      = "PL-PERS.codper = PL-FLG-MES.codper"
     _FldNameList[1]   > integral.PL-FLG-MES.seccion
"PL-FLG-MES.seccion" ? "X(30)" "character" ? ? ? ? ? ? yes ? no no "28" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > integral.PL-FLG-MES.codper
"PL-FLG-MES.codper" ? "X(7)" "character" ? ? ? ? ? ? no ? no no "6.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > integral.PL-PERS.patper
"integral.PL-PERS.patper" ? ? "character" ? ? ? ? ? ? no ? no no "13.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > integral.PL-PERS.matper
"integral.PL-PERS.matper" ? ? "character" ? ? ? ? ? ? no ? no no "14.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > integral.PL-PERS.nomper
"integral.PL-PERS.nomper" ? ? "character" ? ? ? ? ? ? no ? no no "18.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON END-SEARCH OF br_table IN FRAME F-Main
DO:
    APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  RETURN NO-APPLY.
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Codper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Codper B-table-Win
ON LEAVE OF F-Codper IN FRAME F-Main /* Codigo */
OR RETURN OF F-CodPer DO:
    ASSIGN F-CodPer.
    IF F-CodPer = "" THEN RETURN.
    IF LENGTH(F-CodPer) < 6 THEN
        F-CodPer = FILL("0", 6 - LENGTH(F-CodPer)) + F-CodPer.
    FIND FIRST PL-FLG-MES WHERE
        PL-FLG-MES.CodCia = s-codcia AND
        PL-FLG-MES.Periodo = s-periodo AND
        PL-FLG-MES.codpln = PL-PLAN.codpln AND
        PL-FLG-MES.NROMES = s-nromes AND
        PL-FLG-MES.CodPer = F-CodPer
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE PL-FLG-MES THEN DO:
        BELL.
        MESSAGE "Registro no encontrado" VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = "".
        RETURN.
    END.
    REPOSITION {&BROWSE-NAME} TO ROWID ROWID(PL-FLG-MES) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        MESSAGE
            "Registro no se encuentra en el filtro actual" SKIP
            "       Deshacer la actual selecci�n ?       "
            VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO TITLE "Pregunta"
            UPDATE answ AS LOGICAL.
        IF answ THEN DO:
            RUN dispatch IN THIS-PROCEDURE ('open-query-cases':U).
            REPOSITION {&BROWSE-NAME} TO ROWID ROWID(PL-FLG-MES) NO-ERROR.
        END.
    END.
    APPLY "VALUE-CHANGED":U TO br_table.
    ASSIGN SELF:SCREEN-VALUE = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-DesPer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-DesPer B-table-Win
ON LEAVE OF F-DesPer IN FRAME F-Main /* Apellido */
OR RETURN OF F-DesPer DO:
    IF INPUT F-DesPer = "" THEN RETURN.
    FIND LAST PL-PERS WHERE
        PL-PERS.PatPer BEGINS INPUT F-DesPer
        NO-LOCK NO-ERROR.
    IF AVAILABLE PL-PERS THEN DO:
        FOR EACH PL-FLG-MES NO-LOCK WHERE
            PL-FLG-MES.codpln = PL-PLAN.codpln AND
            PL-FLG-MES.CodCia = s-codcia AND
            PL-FLG-MES.Periodo = s-periodo AND
            PL-FLG-MES.NROMES = s-nromes,
            EACH PL-PERS OF PL-FLG-MES NO-LOCK WHERE PL-PERS.PatPer BEGINS INPUT F-DesPer:
            REPOSITION {&BROWSE-NAME} TO ROWID ROWID(PL-FLG-MES) NO-ERROR.
        END.
/*         FIND FIRST PL-FLG-MES WHERE                                    */
/*             PL-FLG-MES.codpln = PL-PLAN.codpln AND                     */
/*             PL-FLG-MES.CodCia = s-codcia AND                           */
/*             PL-FLG-MES.Periodo = s-periodo AND                         */
/*             PL-FLG-MES.NROMES = s-nromes AND                           */
/*             PL-FLG-MES.CodPer = PL-PERS.CodPer                         */
/*             NO-LOCK NO-ERROR.                                          */
/*         IF NOT AVAILABLE PL-FLG-MES THEN DO:                           */
/*             BELL.                                                      */
/*             MESSAGE "Registro no encontrado" VIEW-AS ALERT-BOX ERROR.  */
/*             SELF:SCREEN-VALUE = "".                                    */
/*             RETURN.                                                    */
/*         END.                                                           */
/*         REPOSITION {&BROWSE-NAME} TO ROWID ROWID(PL-FLG-MES) NO-ERROR. */
    END.
    ELSE DO:
        BELL.
        MESSAGE "No existe personal con ese apellido paterno" VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = "".
        RETURN.
    END.
    ASSIGN SELF:SCREEN-VALUE = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "integral.PL-PLAN"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "integral.PL-PLAN"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "integral.PL-PLAN"}
  {src/adm/template/snd-list.i "INTEGRAL.PL-FLG-MES"}
  {src/adm/template/snd-list.i "INTEGRAL.PL-PERS"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

