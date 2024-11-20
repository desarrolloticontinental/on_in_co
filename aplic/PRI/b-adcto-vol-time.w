&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
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

DEF SHARED VAR s-codcia AS INTE.
DEF SHARED VAR s-user-id AS CHAR.

DEF VAR x-Log AS CHAR FORMAT 'x(40)' NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES VtapDctoVolTime
&Scoped-define FIRST-EXTERNAL-TABLE VtapDctoVolTime


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR VtapDctoVolTime.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES VtaaDctoVolTime Almmmatg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table VtaaDctoVolTime.CodMat ~
Almmmatg.DesMat Almmmatg.codfam Almmmatg.subfam Almmmatg.UndStk ~
fLog() @ x-Log 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table VtaaDctoVolTime.CodMat 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table VtaaDctoVolTime
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table VtaaDctoVolTime
&Scoped-define QUERY-STRING-br_table FOR EACH VtaaDctoVolTime OF VtapDctoVolTime WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST Almmmatg WHERE Almmmatg.codmat = VtaaDctoVolTime.CodMat ~
      AND Almmmatg.CodCia = s-codcia NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH VtaaDctoVolTime OF VtapDctoVolTime WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST Almmmatg WHERE Almmmatg.codmat = VtaaDctoVolTime.CodMat ~
      AND Almmmatg.CodCia = s-codcia NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table VtaaDctoVolTime Almmmatg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table VtaaDctoVolTime
&Scoped-define SECOND-TABLE-IN-QUERY-br_table Almmmatg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS>
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fLog B-table-Win 
FUNCTION fLog RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      VtaaDctoVolTime, 
      Almmmatg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      VtaaDctoVolTime.CodMat COLUMN-LABEL "Producto" FORMAT "x(15)":U
      Almmmatg.DesMat FORMAT "X(45)":U WIDTH 45.86
      Almmmatg.codfam COLUMN-LABEL "L�nea" FORMAT "X(3)":U
      Almmmatg.subfam COLUMN-LABEL "Sub-L�nea" FORMAT "X(3)":U
      Almmmatg.UndStk COLUMN-LABEL "Unidad de Stock" FORMAT "X(8)":U
      fLog() @ x-Log COLUMN-LABEL "LOG" WIDTH 40
  ENABLE
      VtaaDctoVolTime.CodMat
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 127 BY 15.88
         FONT 4
         TITLE "TITULO".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: INTEGRAL.VtapDctoVolTime
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
         HEIGHT             = 16.04
         WIDTH              = 160.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm-vm/method/vmbrowser.i}
{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.VtaaDctoVolTime OF INTEGRAL.VtapDctoVolTime,INTEGRAL.Almmmatg WHERE INTEGRAL.VtaaDctoVolTime ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _JoinCode[2]      = "Almmmatg.codmat = VtaaDctoVolTime.CodMat"
     _Where[2]         = "Almmmatg.CodCia = s-codcia"
     _FldNameList[1]   > INTEGRAL.VtaaDctoVolTime.CodMat
"VtaaDctoVolTime.CodMat" "Producto" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.Almmmatg.DesMat
"Almmmatg.DesMat" ? ? "character" ? ? ? ? ? ? no ? no no "45.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.Almmmatg.codfam
"Almmmatg.codfam" "L�nea" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.Almmmatg.subfam
"Almmmatg.subfam" "Sub-L�nea" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > INTEGRAL.Almmmatg.UndStk
"Almmmatg.UndStk" "Unidad de Stock" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"fLog() @ x-Log" "LOG" ? ? ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ROW-ENTRY OF br_table IN FRAME F-Main /* TITULO */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* TITULO */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* TITULO */
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaaDctoVolTime.CodMat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaaDctoVolTime.CodMat br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF VtaaDctoVolTime.CodMat IN BROWSE br_table /* Producto */
DO:
    IF SELF:SCREEN-VALUE = "" THEN RETURN.

    DEF VAR pCodMat AS CHAR NO-UNDO.
    pCodMat = SELF:SCREEN-VALUE.
    RUN vta2/p-codigo-producto.r (INPUT-OUTPUT pCodMat, YES).
    IF pCodMat = '' THEN DO:
        ASSIGN SELF:SCREEN-VALUE = "".
        RETURN NO-APPLY.
    END.
        
    SELF:SCREEN-VALUE = pCodMat.
    FIND FIRST Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = SELF:SCREEN-VALUE
        NO-LOCK.
    IF Almmmatg.tpoart = 'D' THEN DO:
        MESSAGE 'Producto DESACTIVADO' VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = ''.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ON RETURN OF VtaaDctoVolTime.CodMat DO:
    APPLY 'TAB':U.
    RETURN NO-APPLY.
END.

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
  {src/adm/template/row-list.i "VtapDctoVolTime"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "VtapDctoVolTime"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT AVAILABLE VtapDctoVolTime THEN RETURN 'ADM-ERROR'.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      VtaaDctoVolTime.Event = "CREATE"
      VtaaDctoVolTime.Hour = STRING(TIME, 'HH:MM:SS')
      VtaaDctoVolTime.Date = TODAY
      VtaaDctoVolTime.IdUser = s-user-id.
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = "NO" THEN VtaaDctoVolTime.Event = "UPDATE".

  RUN lib/logtabla (INPUT 'VtaaDctoVolTime',
                    INPUT (STRING(VtaaDctoVolTime.IdMaster) + "|" +
                           STRING(VtaaDctoVolTime.IdPack) + "|" +
                           VtaaDctoVolTime.CodMat),
                    INPUT VtaaDctoVolTime.Event).

  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-busca B-table-Win 
PROCEDURE local-busca :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE OK-WAIT-STATE AS LOGICAL NO-UNDO.
  ASSIGN  input-var-1 = ""
          input-var-2 = ""
          input-var-3 = ""
          output-var-1 = ?
          OK-WAIT-STATE = SESSION:SET-WAIT-STATE("GENERAL").

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'busca':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
    /*RUN PL/C-XXX.W("").*/
    IF OUTPUT-VAR-1 <> ? THEN DO:
         FIND {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} WHERE
              ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = OUTPUT-VAR-1
              NO-LOCK NO-ERROR.
         IF AVAIL {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN DO:
            REPOSITION {&BROWSE-NAME}  TO ROWID OUTPUT-VAR-1.
         END.
    END.
  END.
  OK-WAIT-STATE = SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      VtaaDctoVolTime.IdMaster = VtapDctoVolTime.IdMaster
      VtaaDctoVolTime.IdPack =  VtapDctoVolTime.IdPack
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      /* Code placed here will execute PRIOR to standard behavior. */
      RUN lib/logtabla (INPUT 'VtaaDctoVolTime',
                        INPUT (STRING(VtaaDctoVolTime.IdMaster) + "|" +
                               STRING(VtaaDctoVolTime.IdPack) + "|" +
                               VtaaDctoVolTime.CodMat),
                        INPUT "DELETE").

      /* Dispatch standard ADM method.                             */
      RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
      IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

      /* Code placed here will execute AFTER standard behavior.    */
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAILABLE VtapDctoVolTime THEN
      {&BROWSE-NAME}:TITLE IN FRAME {&FRAME-NAME} = VtapDctoVolTime.Descrip.
  ELSE {&BROWSE-NAME}:TITLE IN FRAME {&FRAME-NAME} = "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valida.
  IF RETURN-VALUE = "ADM-ERROR" THEN RETURN "ADM-ERROR".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros B-table-Win 
PROCEDURE procesa-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    Variables a usar:
    output-var-1 como ROWID
    output-var-2 como CHARACTER
    output-var-3 como CHARACTER.
    */
    CASE HANDLE-CAMPO:name:
        WHEN "" THEN.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros B-table-Win 
PROCEDURE recoge-parametros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /*
    Variables a usar:
    input-var-1 como CHARACTER
    input-var-2 como CHARACTER
    input-var-3 como CHARACTER.
    */

    CASE HANDLE-CAMPO:name:
        WHEN "" THEN .
        /*
            ASSIGN
                input-para-1 = ""
                input-para-2 = ""
                input-para-3 = "".
         */      
    END CASE.


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
  {src/adm/template/snd-list.i "VtapDctoVolTime"}
  {src/adm/template/snd-list.i "VtaaDctoVolTime"}
  {src/adm/template/snd-list.i "Almmmatg"}

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

  IF p-state = 'update-begin':U THEN DO:
     RUN valida-update.
     IF RETURN-VALUE = "ADM-ERROR" THEN RETURN.
  END.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida B-table-Win 
PROCEDURE valida :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF TRUE <> (VtaaDctoVolTime.CodMat:SCREEN-VALUE IN BROWSE {&browse-name} > '') 
    THEN DO:
    MESSAGE 'NO debe dejar en blanco el producto' VIEW-AS ALERT-BOX ERROR.
    APPLY 'ENTRY':U TO VtaaDctoVolTime.CodMat.
    RETURN 'ADM-ERROR'.
END.

/* Siempre es adicionar un registro nuevo */
/* NO debe estar registrado dentro del mismo pack */
IF CAN-FIND(FIRST VtaaDctoVolTime WHERE VtaaDctoVolTime.IdMaster = VtapDctoVolTime.IdMaster
            AND VtaaDctoVolTime.IdPack = VtapDctoVolTime.IdPack
            AND VtaaDctoVolTime.CodMat = VtaaDctoVolTime.CodMat:SCREEN-VALUE IN BROWSE {&browse-name}
            NO-LOCK)
    THEN DO:
    MESSAGE 'Registro repetido' VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
END.
/* Tampoco debe estar registrado en otro pack */
DEF BUFFER B-VtaaDctoVolTime FOR VtaaDctoVolTime.
DEF BUFFER B-VtapDctoVolTime FOR VtapDctoVolTime.

FIND FIRST B-VtaaDctoVolTime WHERE B-VtaaDctoVolTime.IdMaster = VtapDctoVolTime.IdMaster
    AND B-VtaaDctoVolTime.CodMat = VtaaDctoVolTime.CodMat:SCREEN-VALUE IN BROWSE {&browse-name}
    NO-LOCK NO-ERROR.
IF AVAILABLE B-VtaaDctoVolTime THEN DO:
    FIND B-VtapDctoVolTime OF B-VtaaDctoVolTime NO-LOCK.
    MESSAGE 'Producto ya registrado en el pack:' B-VtapDctoVolTime.Descrip 
        VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
END.

/* Todos los art�culos relacionados deben tener la misma l�nea, subl�nea y unidades base */
DEF BUFFER B-Almmmatg FOR Almmmatg.
FIND FIRST B-VtaaDctoVolTime OF VtapDctoVolTime NO-LOCK NO-ERROR.
IF AVAILABLE B-VtaaDctoVolTime THEN DO:
    FIND B-Almmmatg WHERE B-Almmmatg.codcia = s-codcia AND
        B-Almmmatg.codmat = B-VtaaDctoVolTime.codmat NO-LOCK NO-ERROR.
    IF AVAILABLE B-Almmmatg THEN DO:
        IF B-Almmmatg.codfam <> Almmmatg.codfam OR
            B-Almmmatg.subfam <> Almmmatg.subfam OR
            B-Almmmatg.undstk <> Almmmatg.undstk
            THEN DO:
            MESSAGE 'Todos los art�culos deben tener pertenecer a la misma l�nea' SKIP
                 'y subl�nea y adem�s tener las mismas unidades' VIEW-AS ALERT-BOX ERROR.
            APPLY 'ENTRY':U TO VtaaDctoVolTime.CodMat.
            RETURN 'ADM-ERROR'.
        END.
    END.
END.

RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update B-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX WARNING.
RETURN 'ADM-ERROR'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fLog B-table-Win 
FUNCTION fLog RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF AVAILABLE VtaaDctoVolTime THEN DO:
      FIND LAST LogTabla WHERE LogTabla.codcia = 1 AND 
          LogTabla.tabla = 'VtaaDctoVolTime' AND
          LogTabla.ValorLlave BEGINS (STRING(VtaaDctoVolTime.IdMaster) + "|" +
                                      STRING(VtaaDctoVolTime.IdPack) + "|" +
                                      VtaaDctoVolTime.CodMat)
          NO-LOCK NO-ERROR.
      IF AVAILABLE LogTabla THEN RETURN LogTabla.Usuario + " " +
          STRING(LogTabla.Dia) + " " +
          LogTabla.Hora + " " +
          LogTabla.Evento.
  END.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

