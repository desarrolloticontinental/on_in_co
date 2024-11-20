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
DEF SHARED VAR s-CodCia AS INT.
DEF SHARED VAR s-Tipo AS CHAR.
DEF SHARED VAR s-User-Id AS CHAR.

&SCOPED-DEFINE Condicion (tratarifas.CodCia = s-codcia AND tratarifas.Tonelaje = s-Tipo)

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
&Scoped-define INTERNAL-TABLES tratarifas GN-DIVI TabDepto TabProvi ~
TabDistr

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tratarifas.CodDiv GN-DIVI.DesDiv ~
tratarifas.CodDepto TabDepto.NomDepto tratarifas.CodProvi TabProvi.NomProvi ~
tratarifas.CodDistr TabDistr.NomDistr tratarifas.Tarifa 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table tratarifas.CodDiv ~
tratarifas.CodDepto tratarifas.CodProvi tratarifas.CodDistr ~
tratarifas.Tarifa 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table tratarifas
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table tratarifas
&Scoped-define QUERY-STRING-br_table FOR EACH tratarifas WHERE ~{&KEY-PHRASE} ~
      AND {&Condicion} NO-LOCK, ~
      FIRST GN-DIVI OF tratarifas NO-LOCK, ~
      FIRST TabDepto OF tratarifas NO-LOCK, ~
      FIRST TabProvi OF tratarifas NO-LOCK, ~
      FIRST TabDistr OF tratarifas NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH tratarifas WHERE ~{&KEY-PHRASE} ~
      AND {&Condicion} NO-LOCK, ~
      FIRST GN-DIVI OF tratarifas NO-LOCK, ~
      FIRST TabDepto OF tratarifas NO-LOCK, ~
      FIRST TabProvi OF tratarifas NO-LOCK, ~
      FIRST TabDistr OF tratarifas NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table tratarifas GN-DIVI TabDepto ~
TabProvi TabDistr
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tratarifas
&Scoped-define SECOND-TABLE-IN-QUERY-br_table GN-DIVI
&Scoped-define THIRD-TABLE-IN-QUERY-br_table TabDepto
&Scoped-define FOURTH-TABLE-IN-QUERY-br_table TabProvi
&Scoped-define FIFTH-TABLE-IN-QUERY-br_table TabDistr


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
      tratarifas, 
      GN-DIVI
    FIELDS(GN-DIVI.DesDiv), 
      TabDepto
    FIELDS(TabDepto.NomDepto), 
      TabProvi
    FIELDS(TabProvi.NomProvi), 
      TabDistr
    FIELDS(TabDistr.NomDistr) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      tratarifas.CodDiv FORMAT "x(5)":U
      GN-DIVI.DesDiv FORMAT "X(40)":U
      tratarifas.CodDepto FORMAT "X(2)":U
      TabDepto.NomDepto FORMAT "X(30)":U
      tratarifas.CodProvi FORMAT "X(2)":U
      TabProvi.NomProvi FORMAT "X(30)":U
      tratarifas.CodDistr FORMAT "X(2)":U
      TabDistr.NomDistr FORMAT "X(30)":U
      tratarifas.Tarifa FORMAT ">>>,>>>,>>9.9999":U
  ENABLE
      tratarifas.CodDiv
      tratarifas.CodDepto
      tratarifas.CodProvi
      tratarifas.CodDistr
      tratarifas.Tarifa
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 136 BY 21.81
         FONT 4.


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
         HEIGHT             = 22.04
         WIDTH              = 136.72.
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
     _TblList          = "INTEGRAL.tratarifas,INTEGRAL.GN-DIVI OF INTEGRAL.tratarifas,INTEGRAL.TabDepto OF INTEGRAL.tratarifas,INTEGRAL.TabProvi OF INTEGRAL.tratarifas,INTEGRAL.TabDistr OF INTEGRAL.tratarifas"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST USED, FIRST USED, FIRST USED, FIRST USED"
     _Where[1]         = "{&Condicion}"
     _FldNameList[1]   > INTEGRAL.tratarifas.CodDiv
"tratarifas.CodDiv" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = INTEGRAL.GN-DIVI.DesDiv
     _FldNameList[3]   > INTEGRAL.tratarifas.CodDepto
"tratarifas.CodDepto" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = INTEGRAL.TabDepto.NomDepto
     _FldNameList[5]   > INTEGRAL.tratarifas.CodProvi
"tratarifas.CodProvi" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = INTEGRAL.TabProvi.NomProvi
     _FldNameList[7]   > INTEGRAL.tratarifas.CodDistr
"tratarifas.CodDistr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = INTEGRAL.TabDistr.NomDistr
     _FldNameList[9]   > INTEGRAL.tratarifas.Tarifa
"tratarifas.Tarifa" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define SELF-NAME tratarifas.CodDiv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tratarifas.CodDiv br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF tratarifas.CodDiv IN BROWSE br_table /* Division */
DO:
  FIND gn-divi WHERE GN-DIVI.CodCia = s-codcia AND
      GN-DIVI.CodDiv = SELF:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAILABLE gn-divi THEN DISPLAY GN-DIVI.DesDiv WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tratarifas.CodDepto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tratarifas.CodDepto br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF tratarifas.CodDepto IN BROWSE br_table /* Depto. */
DO:
  FIND TabDepto WHERE TabDepto.CodDepto = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE TabDepto THEN DISPLAY TabDepto.NomDepto WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tratarifas.CodProvi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tratarifas.CodProvi br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF tratarifas.CodProvi IN BROWSE br_table /* Provincia */
DO:
  FIND TabProvi WHERE TabProvi.CodDepto = tratarifas.CodDepto:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} AND
      TabProvi.CodProvi = SELF:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAILABLE TabProvi THEN DISPLAY TabProvi.NomProvi WITH BROWSE {&BROWSE-NAME}.
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      tratarifas.CodCia = s-codcia 
      tratarifas.Tonelaje = s-Tipo.
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'YES' THEN DO:
      tratarifas.FchCreacion = TODAY.
      tratarifas.UsrCreacion = s-user-id.
  END.
  ELSE DO:
      tratarifas.FchModificacion = TODAY.
      tratarifas.UsrModificacion = s-user-id.
  END.

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
        WHEN "CodProvi" THEN 
            ASSIGN
                input-var-1 = tratarifas.CodDepto:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                input-var-2 = ""
                input-var-3 = "".
        WHEN "CodDistr" THEN 
            ASSIGN
                input-var-1 = tratarifas.CodDepto:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                input-var-2 = tratarifas.CodProvi:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
                input-var-3 = "".
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
  {src/adm/template/snd-list.i "tratarifas"}
  {src/adm/template/snd-list.i "GN-DIVI"}
  {src/adm/template/snd-list.i "TabDepto"}
  {src/adm/template/snd-list.i "TabProvi"}
  {src/adm/template/snd-list.i "TabDistr"}

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

  FIND gn-divi WHERE GN-DIVI.CodCia = s-codcia AND
      GN-DIVI.CodDiv = tratarifas.CodDiv:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE gn-divi THEN DO:
      MESSAGE 'Divisi�n inv�lida' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO tratarifas.CodDiv.
      RETURN 'ADM-ERROR'.
  END.
  FIND TabDepto WHERE TabDepto.CodDepto = tratarifas.CodDepto:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE TabDepto THEN DO:
      MESSAGE 'Departamento inv�lido' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO tratarifas.CodDepto.
      RETURN 'ADM-ERROR'.
  END.
  FIND TabProvi WHERE TabProvi.CodDepto = tratarifas.CodDepto:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} AND
      TabProvi.CodProvi = tratarifas.CodProvi:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE TabProvi THEN DO:
      MESSAGE 'Provincia inv�lido' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO tratarifas.CodProvi.
      RETURN 'ADM-ERROR'.
  END.
  FIND TabDistr WHERE TabDistr.CodDepto = tratarifas.CodDepto:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} AND
      TabDistr.CodProvi = tratarifas.CodProvi:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} AND
      TabDistr.CodDistr = tratarifas.CodDistr:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE TabDistr THEN DO:
      MESSAGE 'Departamento inv�lido' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO tratarifas.CodDistr.
      RETURN 'ADM-ERROR'.
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
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

