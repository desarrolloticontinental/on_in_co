&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER COTIZACION FOR FacCPedi.
DEFINE BUFFER ORDENES FOR FacCPedi.



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
DEF SHARED VAR s-codcia AS INT.

/* Local Variable Definitions ---                                       */

&SCOPED-DEFINE Condicion (FacCPedi.CodDoc = "PED" AND FacCPedi.FlgEst = "C")

DEF TEMP-TABLE T-CLIENTE
    FIELD CodCli AS CHAR FORMAT 'x(11)'.

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
&Scoped-define EXTERNAL-TABLES GN-DIVI
&Scoped-define FIRST-EXTERNAL-TABLE GN-DIVI


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR GN-DIVI.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES FacCPedi

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table FacCPedi.CodDoc FacCPedi.NroPed ~
FacCPedi.FchPed FacCPedi.FchEnt FacCPedi.CodCli FacCPedi.NomCli ~
FacCPedi.ImpTot 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table FacCPedi.FchEnt 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table FacCPedi
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table FacCPedi
&Scoped-define QUERY-STRING-br_table FOR EACH FacCPedi OF GN-DIVI WHERE ~{&KEY-PHRASE} ~
      AND {&Condicion} ~
 AND (FILL-IN-NomCli = '' OR INDEX( FacCPedi.NomCli, FILL-IN-NomCli) > 0) NO-LOCK ~
    BY FacCPedi.FchPed DESCENDING
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH FacCPedi OF GN-DIVI WHERE ~{&KEY-PHRASE} ~
      AND {&Condicion} ~
 AND (FILL-IN-NomCli = '' OR INDEX( FacCPedi.NomCli, FILL-IN-NomCli) > 0) NO-LOCK ~
    BY FacCPedi.FchPed DESCENDING.
&Scoped-define TABLES-IN-QUERY-br_table FacCPedi
&Scoped-define FIRST-TABLE-IN-QUERY-br_table FacCPedi


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 FILL-IN-NomCli br_table 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-NomCli 

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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN-NomCli AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre del cliente" 
     VIEW-AS FILL-IN 
     SIZE 42 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 103 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      FacCPedi SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      FacCPedi.CodDoc COLUMN-LABEL "Doc." FORMAT "x(3)":U
      FacCPedi.NroPed COLUMN-LABEL "N�mero" FORMAT "X(12)":U
      FacCPedi.FchPed COLUMN-LABEL "Fch. Emisi�n" FORMAT "99/99/9999":U
      FacCPedi.FchEnt FORMAT "99/99/9999":U COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 11
      FacCPedi.CodCli COLUMN-LABEL "Cliente" FORMAT "x(11)":U WIDTH 12.43
      FacCPedi.NomCli FORMAT "x(60)":U
      FacCPedi.ImpTot FORMAT "->>,>>>,>>9.99":U
  ENABLE
      FacCPedi.FchEnt
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 103 BY 15.19
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN-NomCli AT ROW 1.38 COL 17 COLON-ALIGNED WIDGET-ID 4
     br_table AT ROW 2.35 COL 1
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: INTEGRAL.GN-DIVI
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: COTIZACION B "?" ? INTEGRAL FacCPedi
      TABLE: ORDENES B "?" ? INTEGRAL FacCPedi
   END-TABLES.
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
         HEIGHT             = 17.35
         WIDTH              = 103.57.
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
/* BROWSE-TAB br_table FILL-IN-NomCli F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.FacCPedi OF INTEGRAL.GN-DIVI"
     _Options          = "NO-LOCK KEY-PHRASE"
     _OrdList          = "INTEGRAL.FacCPedi.FchPed|no"
     _Where[1]         = "{&Condicion}
 AND (FILL-IN-NomCli = '' OR INDEX( FacCPedi.NomCli, FILL-IN-NomCli) > 0)"
     _FldNameList[1]   > INTEGRAL.FacCPedi.CodDoc
"FacCPedi.CodDoc" "Doc." ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.FacCPedi.NroPed
"FacCPedi.NroPed" "N�mero" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.FacCPedi.FchPed
"FacCPedi.FchPed" "Fch. Emisi�n" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.FacCPedi.FchEnt
"FacCPedi.FchEnt" ? ? "date" 11 0 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > INTEGRAL.FacCPedi.CodCli
"FacCPedi.CodCli" "Cliente" ? "character" ? ? ? ? ? ? no ? no no "12.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > INTEGRAL.FacCPedi.NomCli
"FacCPedi.NomCli" ? "x(60)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = INTEGRAL.FacCPedi.ImpTot
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
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
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


&Scoped-define SELF-NAME FILL-IN-NomCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NomCli B-table-Win
ON LEAVE OF FILL-IN-NomCli IN FRAME F-Main /* Nombre del cliente */
OR RETURN OF FILL-IN-NomCli
DO:
  ASSIGN {&SELF-NAME}.
   RUN adm-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

ON 'RETURN':U OF FacCPedi.FchEnt
DO:
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
  {src/adm/template/row-list.i "GN-DIVI"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "GN-DIVI"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-statement B-table-Win 
PROCEDURE local-assign-statement :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DEF BUFFER B-CPEDI FOR Faccpedi.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND B-CPEDI WHERE ROWID(B-CPEDI) = ROWID(Faccpedi) NO-LOCK NO-ERROR.
  FIND FIRST ORDENES WHERE ORDENES.CodCia = B-CPEDI.codcia
      AND ORDENES.CodDoc = "O/D"
      AND ORDENES.CodRef = B-CPEDI.coddoc
      AND ORDENES.NroRef = B-CPEDI.nroref
      AND ORDENES.FlgEst <> "A"
      EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE ORDENES THEN ASSIGN ORDENES.FchEnt = FacCPedi.FchEnt.
  FOR EACH Vtacdocu WHERE VtaCDocu.CodCia = B-CPEDI.codcia 
      AND VtaCDocu.CodPed = "O/D"
      AND VtaCDocu.CodRef = B-CPEDI.coddoc
      AND VtaCDocu.NroRef = B-CPEDI.nroped
      EXCLUSIVE-LOCK:
      ASSIGN
          VtaCDocu.FchEnt = FacCPedi.FchEnt.
  END.
  RELEASE ORDENES.
  RELEASE Vtacdocu.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      FILL-IN-NomCli:SENSITIVE = YES.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      FILL-IN-NomCli:SENSITIVE = NO.
  END.

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
  {src/adm/template/snd-list.i "GN-DIVI"}
  {src/adm/template/snd-list.i "FacCPedi"}

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

/* O/D no impreso */
DEF BUFFER B-CPEDI FOR Faccpedi.

FIND B-CPEDI WHERE ROWID(B-CPEDI) = ROWID(Faccpedi) NO-LOCK NO-ERROR.
IF B-CPEDI.FlgEst <> "C" THEN DO:
    MESSAGE 'El pedido ha sido modificado por otro usuario' VIEW-AS ALERT-BOX ERROR.
    APPLY 'ENTRY':U TO FacCPedi.FchEnt IN BROWSE {&browse-name}.
    RETURN 'ADM-ERROR'.
END.
FIND FIRST ORDENES WHERE ORDENES.CodCia = B-CPEDI.codcia
    AND ORDENES.CodDoc = "O/D"
    AND ORDENES.CodRef = B-CPEDI.coddoc
    AND ORDENES.NroRef = B-CPEDI.nroref
    AND ORDENES.FlgEst <> "A"
    AND ORDENES.FlgImpOD = YES
    NO-LOCK NO-ERROR.
IF AVAILABLE ORDENES THEN DO:
    MESSAGE 'El documento' ORDENES.coddoc ORDENES.nroped 'ya ha sido impreso'
        VIEW-AS ALERT-BOX ERROR.
    APPLY 'ENTRY':U TO FacCPedi.FchEnt.
    RETURN 'ADM-ERROR'.
END.
FIND FIRST Vtacdocu WHERE VtaCDocu.CodCia = B-CPEDI.codcia 
    AND VtaCDocu.CodPed = "O/D"
    AND VtaCDocu.CodRef = B-CPEDI.coddoc
    AND VtaCDocu.NroRef = B-CPEDI.nroped
    AND VtaCDocu.FlgImpOD = YES
    NO-LOCK NO-ERROR.
IF AVAILABLE Vtacdocu THEN DO:
    MESSAGE 'El documento' Vtacdocu.codped ENTRY(1,Vtacdocu.nroped,'-') 'ya ha sido impreso'
        VIEW-AS ALERT-BOX ERROR.
    APPLY 'ENTRY':U TO FacCPedi.FchEnt.
    RETURN 'ADM-ERROR'.
END.

IF DATE(FacCPedi.fchent:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})  < FacCPedi.FchPed THEN DO:
    MESSAGE 'Fecha de entrega errada' VIEW-AS ALERT-BOX ERROR.
    APPLY 'ENTRY':U TO FacCPedi.FchEnt.
    RETURN "ADM-ERROR".
END.
IF DATE(FacCPedi.fchent:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) <= TODAY + 1 THEN DO:
    MESSAGE 'Fecha de entrega errada' SKIP 'Debe ser a partir del' (TODAY + 2)
        VIEW-AS ALERT-BOX ERROR.
    APPLY 'ENTRY':U TO FacCPedi.FchEnt.
    RETURN "ADM-ERROR".
END.

/* ************************************************************** */
/* RHC 11/01/2017 CONSISTENCIA DIA PROGRAMADO Y DESPACHOS POR DIA */
/* ************************************************************** */
/* DEF VAR pFchEnt AS DATE NO-UNDO.                                                                 */
/* pFchEnt = DATE(FacCPedi.fchent:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).                           */
/* DEF BUFFER B-DIVI   FOR gn-divi.                                                                 */
/* DEF BUFFER BGN-DIVI FOR gn-divi.                                                                 */
/* DEF VAR pFchProg AS DATE NO-UNDO.                                                                */
/* pFchProg = pFchEnt.                                                                              */
/* DEF VAR FILL-IN-Lista AS CHAR NO-UNDO.                                                           */
/* FIND COTIZACION WHERE COTIZACION.codcia = B-CPEDI.codcia                                         */
/*     AND COTIZACION.coddiv = B-CPEDI.coddiv                                                       */
/*     AND COTIZACION.coddoc = B-CPEDI.codref                                                       */
/*     AND COTIZACION.nroped = B-CPEDI.nroref                                                       */
/*     NO-LOCK NO-ERROR.                                                                            */
/* IF AVAILABLE COTIZACION THEN FILL-IN-Lista = COTIZACION.Libre_c01.                               */
/* ELSE FILL-IN-Lista = B-CPEDI.CodDiv.                                                             */
/* FIND B-DIVI WHERE B-DIVI.codcia = s-codcia                                                       */
/*     AND B-DIVI.coddiv = FILL-IN-Lista NO-LOCK NO-ERROR.                                          */
/* IF AVAILABLE B-DIVI AND B-DIVI.Campo-Log[8] = YES THEN DO:                                       */
/*     /* Verificamos si tiene programaci�n en el almac�n de despacho */                            */
/*     FIND Almacen WHERE Almacen.codcia = B-CPEDI.CodCia                                           */
/*         AND Almacen.codalm = B-CPEDI.CodAlm                                                      */
/*         NO-LOCK.                                                                                 */
/*     FIND BGN-DIVI WHERE BGN-DIVI.CodCia = Almacen.codcia                                         */
/*         AND BGN-DIVI.CodDiv = Almacen.coddiv                                                     */
/*         NO-LOCK.                                                                                 */
/*     FIND VtaTabla WHERE VtaTabla.CodCia = s-codcia                                               */
/*         AND VtaTabla.Tabla = "CPXDIA"                                                            */
/*         AND VtaTabla.Llave_c1 = BGN-DIVI.CodDiv                                                  */
/*         AND VtaTabla.Llave_c2 = B-CPEDI.CodPos                                                   */
/*         AND VtaTabla.Valor[1] = WEEKDAY(pFchProg)                                                */
/*         NO-LOCK NO-ERROR.                                                                        */
/*     IF NOT AVAILABLE VtaTabla THEN DO:                                                           */
/*         MESSAGE 'No hay programaci�n de despachos para el d�a' pFchProg VIEW-AS ALERT-BOX ERROR. */
/*         APPLY 'ENTRY':U TO FacCPedi.FchEnt.                                                      */
/*         RETURN 'ADM-ERROR'.                                                                      */
/*     END.                                                                                         */
/*     /* Verificamos clientes por dia */                                                           */
/*     IF B-DIVI.DiasAmpCot > 0 THEN DO:                                                            */
/*         DEF VAR x-Cuentas AS INT NO-UNDO.                                                        */
/*         x-Cuentas = 0.                                                                           */
/*         EMPTY TEMP-TABLE T-CLIENTE.                                                              */
/*         FOR EACH B-CPEDI NO-LOCK WHERE B-CPEDI.codcia = Faccpedi.codcia                          */
/*             AND B-CPEDI.coddiv = Faccpedi.coddiv                                                 */
/*             AND B-CPEDI.coddoc = Faccpedi.coddoc                                                 */
/*             AND LOOKUP(B-CPEDI.flgest, "P,G,T,W,WX,WL") > 0                                      */
/*             AND B-CPEDI.fchent = pFchProg:                                                       */
/*             FIND T-CLIENTE WHERE T-CLIENTE.codcli = B-CPEDI.codcli NO-LOCK NO-ERROR.             */
/*             IF NOT AVAILABLE T-CLIENTE THEN DO:                                                  */
/*                 CREATE T-CLIENTE.                                                                */
/*                 ASSIGN T-CLIENTE.codcli = B-CPEDI.codcli.                                        */
/*                 x-Cuentas = x-Cuentas + 1.                                                       */
/*             END.                                                                                 */
/*         END.                                                                                     */
/*         IF x-Cuentas > B-DIVI.DiasAmpCot THEN DO:                                                */
/*             MESSAGE 'Ya se tienen ' x-cuentas 'clientes para ese dia' SKIP                       */
/*                 'Continuamos con la grabaci�n?' VIEW-AS ALERT-BOX QUESTION                       */
/*                 BUTTONS YES-NO UPDATE rpta AS LOG.                                               */
/*             IF rpta = NO THEN DO:                                                                */
/*                 APPLY 'ENTRY':U TO FacCPedi.FchEnt.                                              */
/*                 RETURN 'ADM-ERROR'.                                                              */
/*             END.                                                                                 */
/*         END.                                                                                     */
/*     END.                                                                                         */
/* END.                                                                                             */
/* ************************************************************** */

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
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

