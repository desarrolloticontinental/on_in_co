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

DEFINE SHARED VARIABLE CL-CODCIA LIKE gn-clie.codcia.
DEFINE SHARED VARIABLE S-CODCIA AS INTEGER.
DEFINE SHARED VARIABLE cCodUnico LIKE gn-clie.codunico.

DEFINE VAR CodUnic LIKE gn-clie.codunico NO-UNDO.

DEFINE BUFFER btc-gn-clie FOR tc-gn-clie.

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tc-gn-clie gn-clie

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tc-gn-clie.CodCli tc-gn-clie.NomCli ~
tc-gn-clie.Fching tc-gn-clie.CodUnico gn-clie.DirCli gn-clie.DNI ~
gn-clie.Ruc gn-clie.Fching 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH tc-gn-clie WHERE ~{&KEY-PHRASE} ~
      AND tc-gn-clie.CodCia = cl-codcia NO-LOCK, ~
      FIRST gn-clie OF tc-gn-clie  NO-LOCK ~
    BY tc-gn-clie.CodCli INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH tc-gn-clie WHERE ~{&KEY-PHRASE} ~
      AND tc-gn-clie.CodCia = cl-codcia NO-LOCK, ~
      FIRST gn-clie OF tc-gn-clie  NO-LOCK ~
    BY tc-gn-clie.CodCli INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_table tc-gn-clie gn-clie
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tc-gn-clie
&Scoped-define SECOND-TABLE-IN-QUERY-br_table gn-clie


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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      tc-gn-clie, 
      gn-clie SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      tc-gn-clie.CodCli FORMAT "x(11)":U WIDTH 10
      tc-gn-clie.NomCli FORMAT "x(50)":U WIDTH 40
      tc-gn-clie.Fching FORMAT "99/99/9999":U
      tc-gn-clie.CodUnico COLUMN-LABEL "Cod �nico Sugerido" FORMAT "x(11)":U
            WIDTH 14
      gn-clie.DirCli COLUMN-LABEL "Direcci�n" FORMAT "x(100)":U
      gn-clie.DNI FORMAT "x(10)":U
      gn-clie.Ruc FORMAT "x(11)":U WIDTH 12
      gn-clie.Fching FORMAT "99/99/9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 109 BY 8.62
         FONT 4
         TITLE "Clientes Nuevos".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 1 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
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
         HEIGHT             = 8.73
         WIDTH              = 109.72.
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

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.tc-gn-clie,INTEGRAL.gn-clie OF INTEGRAL.tc-gn-clie "
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE"
     _TblOptList       = ", FIRST"
     _OrdList          = "INTEGRAL.tc-gn-clie.CodCli|yes"
     _Where[1]         = "INTEGRAL.tc-gn-clie.CodCia = cl-codcia"
     _FldNameList[1]   > INTEGRAL.tc-gn-clie.CodCli
"tc-gn-clie.CodCli" ? ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.tc-gn-clie.NomCli
"tc-gn-clie.NomCli" ? ? "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = INTEGRAL.tc-gn-clie.Fching
     _FldNameList[4]   > INTEGRAL.tc-gn-clie.CodUnico
"tc-gn-clie.CodUnico" "Cod �nico Sugerido" ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > INTEGRAL.gn-clie.DirCli
"gn-clie.DirCli" "Direcci�n" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = INTEGRAL.gn-clie.DNI
     _FldNameList[7]   > INTEGRAL.gn-clie.Ruc
"gn-clie.Ruc" ? ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = INTEGRAL.gn-clie.Fching
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
ON ROW-ENTRY OF br_table IN FRAME F-Main /* Clientes Nuevos */
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main /* Clientes Nuevos */
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main /* Clientes Nuevos */
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-Registros B-table-Win 
PROCEDURE Actualiza-Registros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  MESSAGE "�La informaci�n es correcta?" SKIP
          "Usted Actualizar� el registro"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
      TITLE "" UPDATE lChoice AS LOGICAL.
  
  CASE lChoice:
      WHEN YES THEN DO: 
          MESSAGE "Se grabara como c�digo unico: " tc-gn-clie.codunico:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} SKIP
                  "  al cliente: " tc-gn-clie.codcli
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          
          FIND FIRST gn-clie 
              WHERE gn-clie.codcia = cl-codcia
              AND gn-clie.codcli = tc-gn-clie.codcli
              EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL gn-clie THEN 
              ASSIGN
                gn-clie.codunico = tc-gn-clie.codunico:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}.

          /*Elimina registro de Temporal*/
          /*
          FIND FIRST b-tc-gnclie WHERE b-tc-gnclie.codcia = s-codcia 
              AND b-tc-gnclie.codcli = tc-gn-clie.codcli EXCLUSIVE-LOCK NO-ERROR.*/
          FIND FIRST btc-gn-clie WHERE ROWID(btc-gn-clie) = ROWID(tc-gn-clie) EXCLUSIVE-LOCK.
          IF AVAILABLE btc-gn-clie THEN DELETE btc-gn-clie.
          RUN adm-open-query.              
      END.
      WHEN NO THEN DO:
        RETURN NO-APPLY.
      END.          
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Agrega-Grupo B-table-Win 
PROCEDURE Agrega-Grupo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/        
  DISPLAY
        cCodUnico @ tc-gn-clie.codunico
      WITH BROWSE {&BROWSE-NAME}.   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */   

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reinicia-Valores B-table-Win 
PROCEDURE Reinicia-Valores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FIND FIRST btc-gn-clie WHERE ROWID(btc-gn-clie) = ROWID(tc-gn-clie) NO-LOCK NO-ERROR.
  IF AVAILABLE btc-gn-clie THEN
      DISPLAY
        btc-gn-clie.CodUnico @ tc-gn-clie.CodUnico
        WITH BROWSE {&BROWSE-NAME}.
      

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
  {src/adm/template/snd-list.i "tc-gn-clie"}
  {src/adm/template/snd-list.i "gn-clie"}

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
