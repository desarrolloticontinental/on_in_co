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
DEF SHARED VAR s-coddiv AS CHAR.
DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR cl-codcia AS INT.
DEF SHARED VAR s-user-id AS CHAR.
DEF SHARED VAR s-coddoc AS CHAR.

DEF VAR x-Moneda AS CHAR NO-UNDO.

DEF BUFFER b-rutad FOR di-rutad.
DEF BUFFER b-rutac FOR di-rutac.

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
&Scoped-define EXTERNAL-TABLES DI-RutaC
&Scoped-define FIRST-EXTERNAL-TABLE DI-RutaC


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR DI-RutaC.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES DI-RutaD CcbCDocu

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table DI-RutaD.CodRef DI-RutaD.NroRef ~
DI-RutaD.HorEst CcbCDocu.NomCli ~
IF CcbCDocu.CodMon = 1 THEN 'S/.' ELSE 'US$' @ x-moneda CcbCDocu.ImpTot 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table DI-RutaD.NroRef 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table DI-RutaD
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table DI-RutaD
&Scoped-define QUERY-STRING-br_table FOR EACH DI-RutaD OF DI-RutaC WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH CcbCDocu WHERE CcbCDocu.CodCia = DI-RutaD.CodCia ~
  AND CcbCDocu.CodDoc = DI-RutaD.CodRef ~
  AND CcbCDocu.NroDoc = DI-RutaD.NroRef NO-LOCK ~
    BY CcbCDocu.CodCli
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH DI-RutaD OF DI-RutaC WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH CcbCDocu WHERE CcbCDocu.CodCia = DI-RutaD.CodCia ~
  AND CcbCDocu.CodDoc = DI-RutaD.CodRef ~
  AND CcbCDocu.NroDoc = DI-RutaD.NroRef NO-LOCK ~
    BY CcbCDocu.CodCli.
&Scoped-define TABLES-IN-QUERY-br_table DI-RutaD CcbCDocu
&Scoped-define FIRST-TABLE-IN-QUERY-br_table DI-RutaD
&Scoped-define SECOND-TABLE-IN-QUERY-br_table CcbCDocu


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
      DI-RutaD, 
      CcbCDocu SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      DI-RutaD.CodRef COLUMN-LABEL "Doc." FORMAT "x(3)":U
      DI-RutaD.NroRef FORMAT "X(11)":U
      DI-RutaD.HorEst COLUMN-LABEL "Hora!Estimada" FORMAT "XX:XX":U
      CcbCDocu.NomCli FORMAT "x(40)":U
      IF CcbCDocu.CodMon = 1 THEN 'S/.' ELSE 'US$' @ x-moneda COLUMN-LABEL "Mon." FORMAT "x(3)":U
      CcbCDocu.ImpTot COLUMN-LABEL "Importe" FORMAT "->>,>>>,>>9.99":U
  ENABLE
      DI-RutaD.NroRef
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 90 BY 7.5
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: INTEGRAL.DI-RutaC
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
         HEIGHT             = 7.69
         WIDTH              = 90.14.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmbrowser.i}
{src/adm/method/browser.i}
{src/adm-vm/method/vmviewer.i}

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
     _TblList          = "INTEGRAL.DI-RutaD OF INTEGRAL.DI-RutaC,INTEGRAL.CcbCDocu WHERE INTEGRAL.DI-RutaD ..."
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ","
     _OrdList          = "INTEGRAL.CcbCDocu.CodCli|yes"
     _JoinCode[2]      = "INTEGRAL.CcbCDocu.CodCia = INTEGRAL.DI-RutaD.CodCia
  AND INTEGRAL.CcbCDocu.CodDoc = INTEGRAL.DI-RutaD.CodRef
  AND INTEGRAL.CcbCDocu.NroDoc = INTEGRAL.DI-RutaD.NroRef"
     _FldNameList[1]   > INTEGRAL.DI-RutaD.CodRef
"DI-RutaD.CodRef" "Doc." ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.DI-RutaD.NroRef
"DI-RutaD.NroRef" ? "X(11)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.DI-RutaD.HorEst
"DI-RutaD.HorEst" "Hora!Estimada" "XX:XX" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.CcbCDocu.NomCli
"CcbCDocu.NomCli" ? "x(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"IF CcbCDocu.CodMon = 1 THEN 'S/.' ELSE 'US$' @ x-moneda" "Mon." "x(3)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > INTEGRAL.CcbCDocu.ImpTot
"CcbCDocu.ImpTot" "Importe" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
/*   RUN GET-ATTRIBUTE('ADM-NEW-RECORD').                                */
/*   IF RETURN-VALUE = 'YES'                                             */
/*   THEN DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = 'G/R'. */
  IF DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ''
  THEN DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = 'G/R'.
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


&Scoped-define SELF-NAME DI-RutaD.CodRef
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DI-RutaD.CodRef br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF DI-RutaD.CodRef IN BROWSE br_table /* Doc. */
DO:
  ASSIGN SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DI-RutaD.NroRef
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DI-RutaD.NroRef br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF DI-RutaD.NroRef IN BROWSE br_table /* Numero */
DO:
    SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
    /* RUTINA CON EL SCANNER */
    CASE SUBSTRING(SELF:SCREEN-VALUE,1,1):
      WHEN '9' THEN DO:           /* G/R */
          ASSIGN
              SELF:SCREEN-VALUE = SUBSTRING(SELF:SCREEN-VALUE,2,3) + SUBSTRING(SELF:SCREEN-VALUE,6,6).
      END.
    END CASE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DI-RutaD.HorEst
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DI-RutaD.HorEst br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF DI-RutaD.HorEst IN BROWSE br_table /* Hora!Estimada */
DO:
  IF SELF:SCREEN-VALUE = "  :  " THEN RETURN.
  /* Consistencia */
  DEF VAR x-Hora AS INT.
  DEF VAR x-Min  AS INT.
  
  ASSIGN
    x-Hora = INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2))
    NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE 'Ingrese correctamente la hora' VIEW-AS ALERT-BOX WARNING.
    RETURN NO-APPLY.
  END.
  ASSIGN
    x-Min = INTEGER(SUBSTRING(SELF:SCREEN-VALUE,4,2))
    NO-ERROR.
  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE 'Ingrese correctamente los minutos' VIEW-AS ALERT-BOX WARNING.
    RETURN NO-APPLY.
  END.
  SELF:SCREEN-VALUE = STRING(x-Hora, '99') + STRING(x-Min, '99').
  IF NOT (x-Hora >= 0 AND x-Hora <= 23) THEN DO:
    MESSAGE 'La hora debe estar en 00 y 23' VIEW-AS ALERT-BOX WARNING.
    RETURN NO-APPLY.
  END.
  IF NOT (x-Min >= 0 AND x-Min <= 59) THEN DO:
    MESSAGE 'Los minutos deben estar en 00 y 59' VIEW-AS ALERT-BOX WARNING.
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

ON RETURN OF DI-RutaD.CodRef, DI-RutaD.HorEst, DI-RutaD.NroRef 
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
  {src/adm/template/row-list.i "DI-RutaC"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "DI-RutaC"}

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
  IF LOOKUP (di-rutac.flgest, 'C,A') > 0 THEN DO:
      MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR". 
  END.

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
  Notes:       SOLO se puede crear registros
------------------------------------------------------------------------------*/

    DEFINE VARIABLE pResumen AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iInt     AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cValor   AS CHARACTER   NO-UNDO.

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
      
    ASSIGN
        DI-RutaD.CodCia = DI-RutaC.CodCia
        DI-RutaD.CodDiv = DI-RutaC.CodDiv
        DI-RutaD.CodDoc = DI-RutaC.CodDoc
        DI-RutaD.NroDoc = DI-RutaC.NroDoc
        DI-RutaD.CodRef = DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&browse-name}.

    RUN Vta/resumen-pedido (DI-RutaD.CodDiv, DI-RutaD.CodRef, DI-RutaD.NroRef, OUTPUT pResumen).
    pResumen = SUBSTRING(pResumen,2,(LENGTH(pResumen) - 2)).
    DO iint = 1 TO NUM-ENTRIES(pResumen,"/"):
        cValor = cValor + SUBSTRING(ENTRY(iint,pResumen,"/"),4) + ','.
    END.
    DI-RutaD.Libre_c01 = cValor.

    /* Grabamos el orden de impresion */
    ASSIGN
        DI-RutaD.Libre_d01 = 9999.
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = ccbcdocu.codcli
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie THEN DO:
        FIND Vtaubidiv WHERE VtaUbiDiv.CodCia = s-codcia
            AND VtaUbiDiv.CodDiv = s-coddiv
            AND VtaUbiDiv.CodDept = gn-clie.CodDept 
            AND VtaUbiDiv.CodProv = gn-clie.CodProv 
            AND VtaUbiDiv.CodDist = gn-clie.CodDist
            NO-LOCK NO-ERROR.
        IF AVAILABLE VtaUbiDiv THEN DI-RutaD.Libre_d01 = VtaUbiDiv.Libre_d01.
        /* Clientes de provincias */
/*         IF NOT ( (gn-clie.CodDept = '15' AND gn-clie.CodProv = '01')              */
/*                  OR (gn-clie.CodDept = '07' AND gn-clie.CodProv = '01') )         */
/*             THEN DO:                                                              */
/*             /* Buscamos LA VICTORIA */                                            */
/*             FIND Vtaubidiv WHERE VtaUbiDiv.CodCia = s-codcia                      */
/*                 AND VtaUbiDiv.CodDiv = s-coddiv                                   */
/*                 AND VtaUbiDiv.CodDept = '15'                                      */
/*                 AND VtaUbiDiv.CodProv = '01'                                      */
/*                 AND VtaUbiDiv.CodDist = '13'                                      */
/*                 NO-LOCK NO-ERROR.                                                 */
/*             IF AVAILABLE Vtaubidiv THEN DI-RutaD.Libre_d01 = VtaUbiDiv.Libre_d01. */
/*         END.                                                                      */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
/*   IF di-rutac.flgest <> 'P' THEN DO:                     */
/*       MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX ERROR. */
/*       RETURN "ADM-ERROR".                                */
/*   END.                                                   */
  
/*   {adm/i-DocPssw.i s-CodCia s-CodDoc ""DEL""} */

  IF LOOKUP (di-rutac.flgest, 'C,A') > 0 THEN DO:
      MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR". 
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  RUN GET-ATTRIBUTE('ADM-NEW-RECORD').
  IF RETURN-VALUE = 'NO' THEN DO:
/*       DI-RutaD.CodRef:READ-ONLY IN BROWSE {&browse-name} = YES. */
      DI-RutaD.NroRef:READ-ONLY IN BROWSE {&browse-name} = YES.
      APPLY 'ENTRY':U TO DI-RutaD.HorEst IN BROWSE {&browse-name}.
  END.
  ELSE DO:
/*       DI-RutaD.CodRef:READ-ONLY IN BROWSE {&browse-name} = NO. */
      DI-RutaD.NroRef:READ-ONLY IN BROWSE {&browse-name} = NO.
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
  {src/adm/template/snd-list.i "DI-RutaC"}
  {src/adm/template/snd-list.i "DI-RutaD"}
  {src/adm/template/snd-list.i "CcbCDocu"}

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
  Notes:  EN CASO DE ERROR RETORNAR : RETURN "ADM-ERROR"
------------------------------------------------------------------------------*/
  DEF VAR x-Rowid AS ROWID.
  
  IF LOOKUP(TRIM(DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}), 'G/R') = 0
  THEN DO:
    MESSAGE 'El documento debe ser G/R' VIEW-AS ALERT-BOX ERROR.
    APPLY 'ENTRY':U TO DI-RutaD.CodRef IN BROWSE {&BROWSE-NAME}.
    RETURN 'ADM-ERROR'.
  END.
  FIND ccbcdocu WHERE ccbcdocu.codcia = s-codcia
    AND ccbcdocu.coddoc = DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
    AND ccbcdocu.nrodoc = DI-RutaD.NroRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
    NO-LOCK NO-ERROR.
  IF NOT AVAILABLE ccbcdocu
  THEN DO:
    MESSAGE "El documento" DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        DI-RutaD.NroRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        "no est� registrado" VIEW-AS ALERT-BOX ERROR.
    RETURN 'ADM-ERROR'.
  END.
/*   IF LOOKUP(CcbCdocu.TpoFac, 'A,S') > 0 THEN DO:                 */
/*       MESSAGE 'Documento NO v�lido' SKIP                         */
/*           'El documento es por ADELANTO o por SERVICIO'          */
/*           VIEW-AS ALERT-BOX ERROR.                               */
/*       APPLY 'entry' TO DI-RutaD.NroRef IN BROWSE {&browse-name}. */
/*       RETURN "ADM-ERROR".                                        */
/*   END.                                                           */
  IF Ccbcdocu.flgest = "A" THEN DO:
      MESSAGE "El documento se encuentra anulado" VIEW-AS ALERT-BOX ERROR.
      APPLY 'entry' TO DI-RutaD.NroRef IN BROWSE {&browse-name}.
      RETURN "ADM-ERROR".
  END.
  IF Ccbcdocu.flgest <> "F" THEN DO:
      MESSAGE "El documento NO est� FACTURADO" VIEW-AS ALERT-BOX ERROR.
      APPLY 'entry' TO DI-RutaD.NroRef IN BROWSE {&browse-name}.
      RETURN "ADM-ERROR".
  END.
  IF di-rutac.flgest = "C" THEN DO:
      MESSAGE "Hoja de Ruta esta CERRADA" VIEW-AS ALERT-BOX ERROR.
      RETURN "ADM-ERROR".
  END.
  RUN GET-ATTRIBUTE("ADM-NEW-RECORD").
  IF RETURN-VALUE = "YES" THEN DO:
      IF CAN-FIND(FIRST DI-RutaD OF DI-RutaC WHERE DI-RutaD.codref = DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        AND DI-RutaD.nroref = DI-RutaD.NroRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
        NO-LOCK)
      THEN DO:
        MESSAGE "El documento" DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            DI-RutaD.NroRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
            "ya fue registrado en esta hoja de ruta" VIEW-AS ALERT-BOX ERROR.
        RETURN 'ADM-ERROR'.
      END.
      /* consistencia en otros documentos */
      FOR EACH b-rutad NO-LOCK WHERE b-rutad.codcia = s-codcia
          AND b-rutad.coddoc = s-coddoc
          AND b-rutad.codref = DI-RutaD.CodRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}
          AND b-rutad.nroref = DI-RutaD.NroRef:SCREEN-VALUE IN BROWSE {&BROWSE-NAME},
          FIRST b-rutac OF b-rutad NO-LOCK:
          IF b-rutac.flgest = "E" OR b-rutac.flgest = "P" THEN DO:
              MESSAGE "Documento ya se encuentra registrado en la H.R.:" b-rutac.nrodoc
                  VIEW-AS ALERT-BOX ERROR.
              RETURN "ADM-ERROR".
          END.
          IF b-rutac.flgest = "C"  AND b-rutad.flgest = "C" THEN DO:
              MESSAGE "Documento ya se encuentra registrado en la H.R.:" b-rutac.nrodoc
                  VIEW-AS ALERT-BOX ERROR.
              RETURN "ADM-ERROR".
          END.
      END.
  END.
/*   IF Di-RutaD.HorEst:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ''                     */
/*         OR Di-RutaD.HorEst:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = '  :  ' THEN DO: */
/*     MESSAGE 'Debe ingresar la hora estimada' VIEW-AS ALERT-BOX ERROR.               */
/*     APPLY 'ENTRY':U TO Di-RutaD.HorEst IN BROWSE {&BROWSE-NAME}.                    */
/*     RETURN 'ADM-ERROR'.                                                             */
/*   END.                                                                              */
  
  RETURN "OK".
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valida-update B-table-Win 
PROCEDURE valida-update :
/*------------------------------------------------------------------------------
  Purpose:     Consistenciar la modificacion de la fila
  Parameters:  Retornar "ADM-ERROR" en caso de bloquear la modificacion
  Notes:       
------------------------------------------------------------------------------*/

IF LOOKUP (di-rutac.flgest, 'C,A') > 0 THEN DO:
    MESSAGE 'Acceso Denegado' VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR". 
END.
{adm/i-DocPssw.i s-CodCia s-CodDoc ""UPD""}
RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

