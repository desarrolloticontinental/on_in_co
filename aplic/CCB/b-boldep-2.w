&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS L-table-Win 
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

/* Definicion de variables compartidas */
DEFINE VARIABLE input-var-1  AS CHARACTER.
DEFINE VARIABLE input-var-2  AS CHARACTER.
DEFINE VARIABLE input-var-3  AS CHARACTER.
DEFINE VARIABLE output-var-1 AS ROWID.
DEFINE VARIABLE output-var-2 AS CHARACTER.
DEFINE VARIABLE output-var-3 AS CHARACTER.
DEFINE SHARED VARIABLE s-codcia     AS INTEGER.
DEFINE SHARED VARIABLE s-user-id    AS CHARACTER.
DEFINE SHARED VARIABLE s-coddoc     AS CHARACTER.
DEFINE SHARED VARIABLE s-coddiv     AS CHARACTER.
DEFINE SHARED VARIABLE f-coddoc     AS CHARACTER.

/* Definici�n de variables locales */
DEFINE VARIABLE whpadre AS WIDGET-HANDLE.
DEFINE VARIABLE wh      AS WIDGET-HANDLE.
DEFINE VARIABLE curr-record AS RECID.
DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.

DEF BUFFER buf-ccbboldep FOR ccbboldep.
DEF VAR x-CndPgo AS CHAR NO-UNDO.
DEF VAR x-moneda AS CHAR NO-UNDO.

/* Preprocesadores para condiciones */
&SCOPED-DEFINE CONDICION ( CcbBolDep.CodCia = S-CODCIA ~
AND CcbBolDep.CodDoc = S-CODDOC ~
AND (CcbBolDep.FlgEst = "P" OR CcbBolDep.FlgEst = "") ~
AND CcbBolDep.FlgSit BEGINS COMBO-Situacion )

&SCOPED-DEFINE CODIGO CcbBolDep.NroDoc

/* Preprocesadores para cada campo filtro */
&SCOPED-DEFINE FILTRO1 ( CcbBolDep.NomCli BEGINS FILL-IN-filtro )
&SCOPED-DEFINE FILTRO2 ( INDEX ( CcbBolDep.NomCli, FILL-IN-filtro ) <> 0 )
&SCOPED-DEFINE FILTRO3 ( CcbBolDep.ImpTot = FILL-IN-Importe )
&SCOPED-DEFINE FILTRO4 ( CcbBolDep.ImpTot > FILL-IN-Importe )
&SCOPED-DEFINE FILTRO5 ( CcbBolDep.ImpTot < FILL-IN-Importe )

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 10 BY .88
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Aceptar" 
     SIZE 10 BY .88
     BGCOLOR 8 .

DEFINE VARIABLE CMB-condicion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-buscar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Buscar" 
     VIEW-AS FILL-IN 
     SIZE 14.57 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-chr AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 18.43 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-date AS DATE FORMAT "99/99/9999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-dec AS DECIMAL FORMAT "->>>>>9,99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.14 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-int AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.43 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 55.14 BY 1.35.

DEFINE FRAME Dialog-Frame
     FILL-IN-buscar AT ROW 1.23 COL 6.14 COLON-ALIGNED
     CMB-condicion AT ROW 1.19 COL 21 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 2.62 COL 34.29
     FILL-IN-chr AT ROW 1.23 COL 34.43 COLON-ALIGNED NO-LABEL
     FILL-IN-date AT ROW 1.23 COL 34.43 COLON-ALIGNED NO-LABEL
     FILL-IN-int AT ROW 1.23 COL 34.43 COLON-ALIGNED NO-LABEL
     FILL-IN-dec AT ROW 1.23 COL 34.43 COLON-ALIGNED NO-LABEL
     Btn_Cancel AT ROW 2.62 COL 44.86
     RECT-2 AT ROW 1 COL 1
     SPACE(0.00) SKIP(1.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         FONT 4 TITLE "Condiciones de B�squeda"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel CENTERED.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartLookup

&Scoped-define ADM-SUPPORTED-LINKS                           Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES CcbBolDep

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table CcbBolDep.NroDoc CcbBolDep.NomCli ~
IF CcbBolDep.TipDep = 'EFE' THEN 'Efectivo' ELSE 'Cheque' @ x-CndPgo ~
CcbBolDep.NroRef CcbBolDep.FchReg ~
IF CcbBolDep.CodMon = 1 THEN 'S/.' ELSE 'US$' @ x-moneda CcbBolDep.ImpTot ~
CcbBolDep.CodCta CcbBolDep.DesCta CcbBolDep.usuario CcbBolDep.Autorizo ~
CcbBolDep.FchAut CcbBolDep.FlgSit 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define FIELD-PAIRS-IN-QUERY-br_table
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH CcbBolDep WHERE ~{&KEY-PHRASE} ~
      AND {&CONDICION} NO-LOCK ~
    BY CcbBolDep.NroDoc DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_table CcbBolDep
&Scoped-define FIRST-TABLE-IN-QUERY-br_table CcbBolDep


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 COMBO-filtro FILL-IN-codigo ~
FILL-IN-filtro br_table FILL-IN-Importe COMBO-Situacion 
&Scoped-Define DISPLAYED-OBJECTS COMBO-filtro FILL-IN-codigo FILL-IN-filtro ~
FILL-IN-Importe COMBO-Situacion 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" L-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
Nombres que inicien con|y||integral.CcbBolDep.NomCli
Nombres que contengan|y||integral.CcbBolDep.NomCli
Importes iguales a|y||integral.CcbBolDep.ImpTot
Importes mayores a|y||integral.CcbBolDep.ImpTot
Importes menores a|y||integral.CcbBolDep.ImpTot
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "Nombres que inicien con,Nombres que contengan,Importes iguales a,Importes mayores a,Importes menores a",
     Keys-Supplied = ':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" L-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
Codigo|y||integral.CcbBolDep.CodCia|yes,integral.CcbBolDep.CodDoc|yes,integral.CcbBolDep.NroDoc|yes
Nombre de Cliente|||integral.CcbBolDep.CodCia|yes,integral.CcbBolDep.NomCli|yes
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = "Codigo,Nombre de Cliente",
     Sort-Case = Codigo':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).

/* This SmartObject is a valid SortBy-Target. */
&IF '{&user-supported-links}':U ne '':U &THEN
  &Scoped-define user-supported-links {&user-supported-links},SortBy-Target
&ELSE
  &Scoped-define user-supported-links SortBy-Target
&ENDIF

/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES></FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE COMBO-filtro AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Todos" 
     SIZE 21.29 BY 1
     BGCOLOR 15 FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE COMBO-Situacion AS CHARACTER FORMAT "X(256)":U INITIAL "Pendiente" 
     LABEL "Situaci�n" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","Pendiente","Autorizada" 
     SIZE 14.57 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-codigo AS CHARACTER FORMAT "X(9)":U 
     LABEL "N�mero Documento" 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-filtro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29.43 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-Importe AS DECIMAL FORMAT "->>,>>>,>>9.99":U INITIAL 0 
     LABEL "Importe" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 120 BY 11.54.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      CcbBolDep SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table L-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      CcbBolDep.NroDoc FORMAT "XXX-XXXXXXXXX"
      CcbBolDep.NomCli FORMAT "x(40)"
      IF CcbBolDep.TipDep = 'EFE' THEN 'Efectivo' ELSE 'Cheque' @ x-CndPgo COLUMN-LABEL "Condici�n!Pago"
      CcbBolDep.NroRef COLUMN-LABEL "N�mero!Dep�sito" FORMAT "X(12)"
      CcbBolDep.FchReg COLUMN-LABEL "Fecha de!Dep�sito" FORMAT "99/99/99"
      IF CcbBolDep.CodMon = 1 THEN 'S/.' ELSE 'US$' @ x-moneda COLUMN-LABEL "Moneda"
      CcbBolDep.ImpTot
      CcbBolDep.CodCta
      CcbBolDep.DesCta COLUMN-LABEL "Descripci�n"
      CcbBolDep.usuario COLUMN-LABEL "Usuario!Registro"
      CcbBolDep.Autorizo
      CcbBolDep.FchAut COLUMN-LABEL "Fecha!Autorizaci�n"
      CcbBolDep.FlgSit
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS MULTIPLE SIZE 118 BY 9.12
         BGCOLOR 15 FGCOLOR 0 FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     COMBO-filtro AT ROW 2.15 COL 2 NO-LABEL
     FILL-IN-codigo AT ROW 1.19 COL 21 COLON-ALIGNED
     FILL-IN-filtro AT ROW 2.15 COL 23 NO-LABEL
     br_table AT ROW 3.12 COL 2
     FILL-IN-Importe AT ROW 1.19 COL 61 COLON-ALIGNED
     COMBO-Situacion AT ROW 2.15 COL 61 COLON-ALIGNED
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartLookup
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
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
  CREATE WINDOW L-table-Win ASSIGN
         HEIGHT             = 11.54
         WIDTH              = 120.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW L-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit Custom                                       */
/* BROWSE-TAB br_table FILL-IN-filtro F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main = 1.

/* SETTINGS FOR COMBO-BOX COMBO-filtro IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-filtro IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "integral.CcbBolDep"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE"
     _OrdList          = "integral.CcbBolDep.NroDoc|no"
     _Where[1]         = "{&CONDICION}"
     _FldNameList[1]   > integral.CcbBolDep.NroDoc
"NroDoc" ? "XXX-XXXXXXXXX" "character" ? ? ? ? ? ? no ?
     _FldNameList[2]   > integral.CcbBolDep.NomCli
"NomCli" ? "x(40)" "character" ? ? ? ? ? ? no ?
     _FldNameList[3]   > "_<CALC>"
"IF CcbBolDep.TipDep = 'EFE' THEN 'Efectivo' ELSE 'Cheque' @ x-CndPgo" "Condici�n!Pago" ? ? ? ? ? ? ? ? no ?
     _FldNameList[4]   > integral.CcbBolDep.NroRef
"NroRef" "N�mero!Dep�sito" "X(12)" "character" ? ? ? ? ? ? no ?
     _FldNameList[5]   > integral.CcbBolDep.FchReg
"FchReg" "Fecha de!Dep�sito" "99/99/99" "date" ? ? ? ? ? ? no ?
     _FldNameList[6]   > "_<CALC>"
"IF CcbBolDep.CodMon = 1 THEN 'S/.' ELSE 'US$' @ x-moneda" "Moneda" ? ? ? ? ? ? ? ? no ?
     _FldNameList[7]   = integral.CcbBolDep.ImpTot
     _FldNameList[8]   = integral.CcbBolDep.CodCta
     _FldNameList[9]   > integral.CcbBolDep.DesCta
"DesCta" "Descripci�n" ? "character" ? ? ? ? ? ? no ?
     _FldNameList[10]   > integral.CcbBolDep.usuario
"usuario" "Usuario!Registro" ? "character" ? ? ? ? ? ? no ?
     _FldNameList[11]   = integral.CcbBolDep.Autorizo
     _FldNameList[12]   > integral.CcbBolDep.FchAut
"FchAut" "Fecha!Autorizaci�n" ? "date" ? ? ? ? ? ? no ?
     _FldNameList[13]   = integral.CcbBolDep.FlgSit
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB L-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table L-table-Win
ON ANY-PRINTABLE OF br_table IN FRAME F-Main
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table L-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
    RETURN NO-APPLY.
    /* This code displays initial values for newly added or copied rows. */
    {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table L-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table L-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main
DO:

    ASSIGN
        wh = br_table:CURRENT-COLUMN
        FILL-IN-chr:VISIBLE IN FRAME Dialog-Frame = FALSE
        FILL-IN-date:VISIBLE = FALSE
        FILL-IN-int:VISIBLE = FALSE
        FILL-IN-dec:VISIBLE = FALSE
        FILL-IN-buscar = wh:LABEL
        CMB-condicion:LIST-ITEMS = "".

    CASE wh:DATA-TYPE:
        WHEN "CHARACTER" THEN DO:
            ASSIGN
                FILL-IN-chr:VISIBLE = TRUE
                CMB-condicion:LIST-ITEMS = "=,Inicie con,Que contenga".
            IF LOOKUP(CMB-condicion, CMB-condicion:LIST-ITEMS) = 0 THEN
                ASSIGN CMB-condicion = CMB-condicion:ENTRY(1).
            DISPLAY FILL-IN-buscar WITH FRAME Dialog-Frame.
            UPDATE
                CMB-condicion
                FILL-IN-chr
                Btn_OK
                Btn_Cancel
                WITH FRAME Dialog-Frame.
        END.
        WHEN "INTEGER" THEN DO:
            ASSIGN
                FILL-IN-int:VISIBLE = TRUE
                CMB-condicion:LIST-ITEMS = "=,>,<,>=,<=".
            IF LOOKUP(CMB-condicion, CMB-condicion:LIST-ITEMS) = 0 THEN
                ASSIGN CMB-condicion = CMB-condicion:ENTRY(1).
            DISPLAY FILL-IN-buscar WITH FRAME Dialog-Frame.
            UPDATE
                CMB-condicion
                FILL-IN-int
                Btn_OK
                Btn_Cancel
                WITH FRAME Dialog-Frame.
        END.
        WHEN "DECIMAL" THEN DO:
            ASSIGN
                FILL-IN-dec:VISIBLE = TRUE
                CMB-condicion:LIST-ITEMS = "=,>,<,>=,<=".
            IF LOOKUP(CMB-condicion, CMB-condicion:LIST-ITEMS) = 0 THEN
                ASSIGN CMB-condicion = CMB-condicion:ENTRY(1).
            DISPLAY FILL-IN-buscar WITH FRAME Dialog-Frame.
            UPDATE
                CMB-condicion
                FILL-IN-dec
                Btn_OK
                Btn_Cancel
                WITH FRAME Dialog-Frame.
        END.
        WHEN "DATE" THEN DO:
            ASSIGN
                FILL-IN-date:VISIBLE = TRUE
                CMB-condicion:LIST-ITEMS = "=,>,<,>=,<=".
            IF LOOKUP(CMB-condicion, CMB-condicion:LIST-ITEMS) = 0 THEN
                ASSIGN CMB-condicion = CMB-condicion:ENTRY(1).
            DISPLAY FILL-IN-buscar WITH FRAME Dialog-Frame.
            UPDATE
                CMB-condicion
                FILL-IN-date
                Btn_OK
                Btn_Cancel
                WITH FRAME Dialog-Frame.
        END.
    END CASE.

    RUN busqueda-secuencial.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table L-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-filtro L-table-Win
ON VALUE-CHANGED OF COMBO-filtro IN FRAME F-Main
DO:
    IF COMBO-filtro = COMBO-filtro:SCREEN-VALUE AND
        FILL-IN-filtro = FILL-IN-filtro:SCREEN-VALUE AND
        FILL-IN-Importe = DECIMAL(FILL-IN-Importe:SCREEN-VALUE) THEN RETURN.
    ASSIGN
        FILL-IN-filtro
        COMBO-filtro
        FILL-IN-Importe.
    IF COMBO-filtro = "Todos" THEN RUN set-attribute-list('Key-Name=?').
    ELSE RUN set-attribute-list('Key-Name=' + COMBO-filtro).
    RUN dispatch IN THIS-PROCEDURE('open-query-cases':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-Situacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-Situacion L-table-Win
ON VALUE-CHANGED OF COMBO-Situacion IN FRAME F-Main /* Situaci�n */
DO:
  ASSIGN COMBO-Situacion.
  RUN dispatch IN THIS-PROCEDURE ('open-query-cases':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-codigo L-table-Win
ON LEAVE OF FILL-IN-codigo IN FRAME F-Main /* N�mero Documento */
DO:
    IF INPUT FILL-IN-codigo = "" THEN RETURN.
    &IF "{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}" &THEN
        FIND FIRST {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} WHERE
            {&CONDICION} AND
            ( {&CODIGO} = INPUT FILL-IN-codigo )
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN DO:
            BELL.
            MESSAGE "Registro no encontrado" VIEW-AS ALERT-BOX ERROR.
            SELF:SCREEN-VALUE = "".
            RETURN.
        END.
        REPOSITION {&BROWSE-NAME} TO ROWID ROWID( {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE
                "Registro no se encuentra en el filtro actual" SKIP
                "       Deshacer la actual selecci�n ?       "
                VIEW-AS ALERT-BOX QUESTION
                BUTTONS YES-NO TITLE "Pregunta"
                UPDATE answ AS LOGICAL.
            IF answ THEN DO:
                ASSIGN
                    FILL-IN-filtro:SCREEN-VALUE = ""
                    COMBO-filtro:SCREEN-VALUE = COMBO-filtro:ENTRY(1).
                APPLY "VALUE-CHANGED" TO COMBO-filtro.
                RUN dispatch IN THIS-PROCEDURE ('open-query-cases':U).
                REPOSITION {&BROWSE-NAME} TO ROWID ROWID( {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} ) NO-ERROR.
            END.
        END.
        ASSIGN SELF:SCREEN-VALUE = "".
    &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-filtro L-table-Win
ON LEAVE OF FILL-IN-filtro IN FRAME F-Main
DO:
    APPLY "VALUE-CHANGED" TO COMBO-filtro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Importe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Importe L-table-Win
ON LEAVE OF FILL-IN-Importe IN FRAME F-Main /* Importe */
DO:
    APPLY "VALUE-CHANGED" TO COMBO-filtro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK L-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases L-table-Win adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.

  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'Nombres que inicien con':U THEN DO:
       &Scope KEY-PHRASE ( {&Filtro1} )
       RUN get-attribute ('SortBy-Case':U).
       CASE RETURN-VALUE:
         WHEN 'Codigo':U THEN DO:
           &Scope SORTBY-PHRASE BY CcbBolDep.CodCia BY CcbBolDep.CodDoc BY CcbBolDep.NroDoc
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Nombre de Cliente':U THEN DO:
           &Scope SORTBY-PHRASE BY CcbBolDep.CodCia BY CcbBolDep.NomCli
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         OTHERWISE DO:
           &Undefine SORTBY-PHRASE
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END. /* OTHERWISE...*/
       END CASE.
    END. /* Nombres que inicien con */
    WHEN 'Nombres que contengan':U THEN DO:
       &Scope KEY-PHRASE ( {&Filtro2} )
       RUN get-attribute ('SortBy-Case':U).
       CASE RETURN-VALUE:
         WHEN 'Codigo':U THEN DO:
           &Scope SORTBY-PHRASE BY CcbBolDep.CodCia BY CcbBolDep.CodDoc BY CcbBolDep.NroDoc
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Nombre de Cliente':U THEN DO:
           &Scope SORTBY-PHRASE BY CcbBolDep.CodCia BY CcbBolDep.NomCli
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         OTHERWISE DO:
           &Undefine SORTBY-PHRASE
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END. /* OTHERWISE...*/
       END CASE.
    END. /* Nombres que contengan */
    WHEN 'Importes iguales a':U THEN DO:
       &Scope KEY-PHRASE ( {&Filtro3} )
       RUN get-attribute ('SortBy-Case':U).
       CASE RETURN-VALUE:
         WHEN 'Codigo':U THEN DO:
           &Scope SORTBY-PHRASE BY CcbBolDep.CodCia BY CcbBolDep.CodDoc BY CcbBolDep.NroDoc
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Nombre de Cliente':U THEN DO:
           &Scope SORTBY-PHRASE BY CcbBolDep.CodCia BY CcbBolDep.NomCli
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         OTHERWISE DO:
           &Undefine SORTBY-PHRASE
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END. /* OTHERWISE...*/
       END CASE.
    END. /* Importes iguales a */
    WHEN 'Importes mayores a':U THEN DO:
       &Scope KEY-PHRASE ( {&Filtro4} )
       RUN get-attribute ('SortBy-Case':U).
       CASE RETURN-VALUE:
         WHEN 'Codigo':U THEN DO:
           &Scope SORTBY-PHRASE BY CcbBolDep.CodCia BY CcbBolDep.CodDoc BY CcbBolDep.NroDoc
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Nombre de Cliente':U THEN DO:
           &Scope SORTBY-PHRASE BY CcbBolDep.CodCia BY CcbBolDep.NomCli
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         OTHERWISE DO:
           &Undefine SORTBY-PHRASE
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END. /* OTHERWISE...*/
       END CASE.
    END. /* Importes mayores a */
    WHEN 'Importes menores a':U THEN DO:
       &Scope KEY-PHRASE ( {&Filtro5} )
       RUN get-attribute ('SortBy-Case':U).
       CASE RETURN-VALUE:
         WHEN 'Codigo':U THEN DO:
           &Scope SORTBY-PHRASE BY CcbBolDep.CodCia BY CcbBolDep.CodDoc BY CcbBolDep.NroDoc
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Nombre de Cliente':U THEN DO:
           &Scope SORTBY-PHRASE BY CcbBolDep.CodCia BY CcbBolDep.NomCli
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         OTHERWISE DO:
           &Undefine SORTBY-PHRASE
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END. /* OTHERWISE...*/
       END CASE.
    END. /* Importes menores a */
    OTHERWISE DO:
       &Scope KEY-PHRASE TRUE
       RUN get-attribute ('SortBy-Case':U).
       CASE RETURN-VALUE:
         WHEN 'Codigo':U THEN DO:
           &Scope SORTBY-PHRASE BY CcbBolDep.CodCia BY CcbBolDep.CodDoc BY CcbBolDep.NroDoc
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Nombre de Cliente':U THEN DO:
           &Scope SORTBY-PHRASE BY CcbBolDep.CodCia BY CcbBolDep.NomCli
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         OTHERWISE DO:
           &Undefine SORTBY-PHRASE
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END. /* OTHERWISE...*/
       END CASE.
    END. /* OTHERWISE...*/
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available L-table-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Anula L-table-Win 
PROCEDURE Anula :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
    MESSAGE '�Continua con la ANULACION de los dep�sitos?'
        VIEW-AS ALERT-BOX QUESTION BUTTONS
        YES-NO UPDATE rpta AS LOGICAL.
    IF rpta = NO THEN RETURN.
    DO i = 1 TO {&BROWSE-NAME}:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
        IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(i) THEN DO:
            FIND buf-ccbboldep WHERE
                ROWID(buf-ccbboldep) = ROWID(Ccbboldep)
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE buf-ccbboldep AND
                (buf-CcbBolDep.FlgEst = "" OR buf-CcbBolDep.FlgEst = "P") AND
                buf-ccbboldep.imptot = buf-CcbBolDep.sdoact THEN DO:
                ASSIGN 
                    buf-CcbBolDep.Autorizo = s-user-id
                    buf-CcbBolDep.FchAut = TODAY
                    buf-CcbBolDep.FlgEst = "A"
                    buf-CcbBolDep.SdoAct = 0.
                RUN proc_anula-mov.
            END.
            RELEASE buf-ccbboldep.
        END.
    END.
    RUN dispatch IN THIS-PROCEDURE ('open-query':U).
    RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asigna L-table-Win 
PROCEDURE Asigna :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE i AS INTEGER NO-UNDO.

    MESSAGE
        '�Continua con la AUTORIZACION de los dep�sitos?'
        VIEW-AS ALERT-BOX QUESTION
        BUTTONS YES-NO UPDATE rpta AS LOGICAL.
    IF rpta = NO THEN RETURN.
    DO i = 1 TO {&BROWSE-NAME}:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
        IF {&BROWSE-NAME}:FETCH-SELECTED-ROW(i) THEN DO:
            FIND buf-ccbboldep WHERE
                ROWID(buf-ccbboldep) = ROWID(Ccbboldep)
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE buf-ccbboldep AND
                buf-ccbboldep.FlgSit = "Pendiente" AND
                buf-CcbBolDep.FlgEst = "" THEN DO:
                ASSIGN 
                    buf-ccbboldep.FlgSit = "Autorizada"
                    buf-CcbBolDep.Autorizo = s-user-id
                    buf-CcbBolDep.FchAut = TODAY
                    buf-CcbBolDep.FlgEst = "P".
                /* Crea movimiento en ccbcdocu */
                RUN proc_movimiento.
                RELEASE buf-ccbboldep.
            END.
        END.
    END.
    RUN dispatch IN THIS-PROCEDURE ('open-query':U).
    RUN dispatch IN THIS-PROCEDURE ('display-fields':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE busqueda-secuencial L-table-Win 
PROCEDURE busqueda-secuencial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

&IF DEFINED (FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}) > 0 &THEN

DEFINE VARIABLE pto AS LOGICAL NO-UNDO.
pto = SESSION:SET-WAIT-STATE("GENERAL").

ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).

lazo:
DO WHILE AVAILABLE({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) ON STOP UNDO, LEAVE lazo:

    GET NEXT {&BROWSE-NAME}.

    IF QUERY-OFF-END("{&BROWSE-NAME}") THEN GET FIRST {&BROWSE-NAME}.

    REPOSITION br_table TO ROWID ROWID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).

    IF RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}) = curr-record THEN LEAVE lazo.

    CASE wh:DATA-TYPE:
    WHEN "INTEGER" THEN DO:
        CASE CMB-condicion:
        WHEN "=" THEN
            IF INTEGER(wh:SCREEN-VALUE) = FILL-IN-int THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN ">" THEN
            IF INTEGER(wh:SCREEN-VALUE) > FILL-IN-int THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN ">=" THEN
            IF INTEGER(wh:SCREEN-VALUE) >= FILL-IN-int THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN "<" THEN
            IF INTEGER(wh:SCREEN-VALUE) <  FILL-IN-int THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN "<=" THEN
            IF INTEGER(wh:SCREEN-VALUE) <= FILL-IN-int THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        END CASE.
    END.
    WHEN "DECIMAL" THEN DO:
        CASE CMB-condicion:
        WHEN "=" THEN
            IF DECIMAL(wh:SCREEN-VALUE) = FILL-IN-dec THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN ">" THEN
            IF DECIMAL(wh:SCREEN-VALUE) > FILL-IN-dec THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN ">=" THEN
            IF DECIMAL(wh:SCREEN-VALUE) >= FILL-IN-dec THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN "<" THEN
            IF DECIMAL(wh:SCREEN-VALUE) <  FILL-IN-dec THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN "<=" THEN
            IF DECIMAL(wh:SCREEN-VALUE) <= FILL-IN-dec THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        END CASE.
    END.
    WHEN "DATE" THEN DO:
        CASE CMB-condicion:
        WHEN "=" THEN
            IF DATE(wh:SCREEN-VALUE) = FILL-IN-date THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN ">" THEN
            IF DATE(wh:SCREEN-VALUE) > FILL-IN-date THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN ">=" THEN
            IF DATE(wh:SCREEN-VALUE) >= FILL-IN-date THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN "<" THEN
            IF DATE(wh:SCREEN-VALUE) <  FILL-IN-date THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN "<=" THEN
            IF DATE(wh:SCREEN-VALUE) <= FILL-IN-date THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        END CASE.
    END.
    WHEN "CHARACTER" THEN
        CASE CMB-condicion:
        WHEN "=" THEN
            IF wh:SCREEN-VALUE = FILL-IN-chr THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN "Inicie con" THEN
            IF wh:SCREEN-VALUE BEGINS FILL-IN-chr THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        WHEN "Que contenga" THEN
            IF INDEX(wh:SCREEN-VALUE, FILL-IN-chr) > 0 THEN DO:
                ASSIGN curr-record = RECID({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}).
                LEAVE lazo.
            END.
        END CASE.
    OTHERWISE LEAVE lazo.
    END CASE.
END.

pto = SESSION:SET-WAIT-STATE("").

REPOSITION {&BROWSE-NAME} TO RECID curr-record.

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE captura-datos L-table-Win 
PROCEDURE captura-datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
&IF DEFINED (FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}) > 0 &THEN

IF AVAILABLE {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN
    ASSIGN
        output-var-1 = ROWID( {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} )
        output-var-2 = STRING( {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.NroDoc, "999" )
        output-var-3 = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.nomcli.

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI L-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize L-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    RUN get-attribute ('Keys-Accepted').

    IF RETURN-VALUE <> "" AND RETURN-VALUE <> ? THEN
        ASSIGN
            COMBO-filtro:LIST-ITEMS IN FRAME {&FRAME-NAME} =
            COMBO-filtro:LIST-ITEMS + "," + RETURN-VALUE.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    ASSIGN
        output-var-1 = ?
        output-var-2 = ?
        output-var-3 = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc_anula-mov L-table-Win 
PROCEDURE proc_anula-mov :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FIND CcbCdocu WHERE
        CcbCdocu.CodCia = buf-ccbboldep.CodCia AND
        CcbCdocu.CodDiv = buf-ccbboldep.CodDiv AND
        CcbCdocu.CodDoc = buf-ccbboldep.CodDoc AND
        CcbCdocu.NroDoc = buf-ccbboldep.NroDoc
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE CcbCdocu THEN DO:
        IF CcbCdocu.SdoAct <> CcbCdocu.ImpTot THEN DO:
            MESSAGE
                'Dep�sito presenta cancelaciones'
                VIEW-AS ALERT-BOX ERROR.
            RETURN "ADM-ERROR".
        END.
        ASSIGN
            CcbCDocu.UsuAnu = s-user-id
            CcbCDocu.FchAnu = TODAY
            CcbCdocu.SdoAct = 0
            CcbCdocu.FlgEst = "A".
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc_crea-mov L-table-Win 
PROCEDURE proc_crea-mov :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    CREATE CcbCDocu.
    ASSIGN
        CcbCDocu.CodCia = buf-ccbboldep.CodCia
        CcbCDocu.CodDiv = buf-ccbboldep.CodDiv
        CcbCDocu.Coddoc = buf-ccbboldep.CodDoc
        CcbCDocu.NroDoc = buf-ccbboldep.NroDoc
        CcbCDocu.CodCli = buf-ccbboldep.CodCli
        CcbCDocu.NomCli = buf-ccbboldep.NomCli
        CcbCDocu.CodRef = buf-ccbboldep.CodDoc
        CcbCDocu.NroRef = buf-ccbboldep.NroRef
        CcbCDocu.FchDoc = TODAY
        CcbCDocu.FchVto = TODAY
        CcbCDocu.CodMon = buf-ccbboldep.CodMon
        CcbCDocu.TpoCmb = buf-ccbboldep.TpoCmb
        CcbCDocu.ImpTot = buf-ccbboldep.ImpTot
        CcbCDocu.SdoAct = buf-ccbboldep.ImpTot
        CcbCDocu.CodAge = buf-ccbboldep.CodAge
        CcbCDocu.CodCta = buf-ccbboldep.CodCta
        CcbCDocu.usuario = buf-ccbboldep.Autorizo
        CcbCDocu.Codope = buf-ccbboldep.Codope
        CcbCDocu.FlgEst = "P".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key L-table-Win adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* There are no foreign keys supplied by this SmartObject. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records L-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "CcbBolDep"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed L-table-Win 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE toma-handle L-table-Win 
PROCEDURE toma-handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-handle AS WIDGET-HANDLE.
    
    ASSIGN whpadre = p-handle.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


