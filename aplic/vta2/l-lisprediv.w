&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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
DEFINE SHARED VARIABLE input-var-1  AS CHARACTER.
DEFINE SHARED VARIABLE input-var-2  AS CHARACTER.
DEFINE SHARED VARIABLE input-var-3  AS CHARACTER.
DEFINE SHARED VARIABLE output-var-1 AS ROWID.
DEFINE SHARED VARIABLE output-var-2 AS CHARACTER.
DEFINE SHARED VARIABLE output-var-3 AS CHARACTER.
DEFINE SHARED VARIABLE S-CODCIA     AS INTEGER.
DEFINE SHARED VARIABLE s-FlgRotacion LIKE GN-DIVI.FlgRotacion.
DEFINE SHARED VARIABLE S-CODALM     AS CHAR.
/*DEFINE SHARED VARIABLE pCodAlm      AS CHAR.*/

/* NOTA: s-codalm puede ser m�s de uno */
DEFINE SHARED VAR s-codmat AS CHAR.
DEFINE SHARED VAR s-coddiv AS CHAR.

/* Definici�n de variables locales */
DEFINE VARIABLE whpadre AS WIDGET-HANDLE.
DEFINE VARIABLE wh      AS WIDGET-HANDLE.
DEFINE VARIABLE curr-record AS RECID.
DEFINE VARIABLE F-STKACT AS DECIMAL NO-UNDO.
DEFINE VARIABLE f-StockComprometido AS DEC NO-UNDO.
DEFINE VARIABLE x-CodAlm AS CHAR NO-UNDO.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.
 
/* Preprocesadores para condiciones */
&SCOPED-DEFINE CONDICION ( Vtalistamay.Codfam BEGINS F-Codfam ~
AND Vtalistamay.CodDiv = s-coddiv ~
AND Vtalistamay.Subfam BEGINS F-Subfam ~
AND Vtalistamay.DesMar BEGINS F-marca )

&SCOPED-DEFINE CODIGO VtaListaMay.codmat

/* Preprocesadores para cada campo filtro */ 
&SCOPED-DEFINE FILTRO1 VtaListaMay.DesMat BEGINS FILL-IN-filtro
&SCOPED-DEFINE FILTRO2 INDEX ( VtaListaMay.DesMat , FILL-IN-filtro ) <> 0 

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

DEFINE VARIABLE F-PREUSSA AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PREUSSB AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PREUSSC AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PREUSSD AS DECIMAL NO-UNDO.

DEFINE VARIABLE F-PRESOLA AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PRESOLB AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PRESOLC AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PRESOLD AS DECIMAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartLookup
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES VtaListaMay Almmmatg Almmmate

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table VtaListaMay.codfam ~
VtaListaMay.subfam VtaListaMay.codmat VtaListaMay.DesMat VtaListaMay.DesMar ~
F-PRESOLA @ VtaListaMay.PreOfi VtaListaMay.Chr__01 Almmmatg.UndBas ~
_StkAct() @ F-STKACT ~
_Comprometido(Almmmatg.codmat, x-codalm) @ f-StockComprometido 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH VtaListaMay WHERE ~{&KEY-PHRASE} ~
      AND {&CONDICION} NO-LOCK, ~
      EACH Almmmatg OF VtaListaMay NO-LOCK, ~
      FIRST Almmmate OF Almmmatg ~
      WHERE Almmmate.CodAlm = x-codalm ~
 AND (RADIO-SET-Stock = 'T' OR Almmmate.StkAct > 0) NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH VtaListaMay WHERE ~{&KEY-PHRASE} ~
      AND {&CONDICION} NO-LOCK, ~
      EACH Almmmatg OF VtaListaMay NO-LOCK, ~
      FIRST Almmmate OF Almmmatg ~
      WHERE Almmmate.CodAlm = x-codalm ~
 AND (RADIO-SET-Stock = 'T' OR Almmmate.StkAct > 0) NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_table VtaListaMay Almmmatg Almmmate
&Scoped-define FIRST-TABLE-IN-QUERY-br_table VtaListaMay
&Scoped-define SECOND-TABLE-IN-QUERY-br_table Almmmatg
&Scoped-define THIRD-TABLE-IN-QUERY-br_table Almmmate


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS COMBO-BOX-CodAlm FILL-IN-codigo F-marca ~
RADIO-SET-Stock F-Codfam F-SubFam CMB-filtro FILL-IN-filtro br_table 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-CodAlm FILL-IN-codigo F-marca ~
RADIO-SET-Stock F-Codfam F-SubFam CMB-filtro FILL-IN-filtro 

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
Nombres que inicien con|y||INTEGRAL.VtaListaMay.DesMat
Nombres que contengan|y||INTEGRAL.VtaListaMay.DesMat
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "Nombres que inicien con,Nombres que contengan",
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
Codigo|||INTEGRAL.VtaListaMay.CodCia|yes,INTEGRAL.VtaListaMay.codmat|yes
Familia|||INTEGRAL.VtaListaMay.codfam|yes
Descripcion|y||INTEGRAL.VtaListaMay.DesMat|yes
Marca|||INTEGRAL.VtaListaMay.DesMar|yes
Sub-Familia|||INTEGRAL.VtaListaMay.codfam|yes,INTEGRAL.VtaListaMay.subfam|yes
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = "':U + 'Codigo,Familia,Descripcion,Marca,Sub-Familia' + '",
     SortBy-Case = ':U + 'Descripcion').

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD _Comprometido L-table-Win 
FUNCTION _Comprometido RETURNS DECIMAL
  ( INPUT pCodMat AS CHAR, INPUT pCodAlm AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD _StkAct L-table-Win 
FUNCTION _StkAct RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE CMB-filtro AS CHARACTER FORMAT "X(256)":U INITIAL "Nombres que inicien con" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Todos" 
     DROP-DOWN-LIST
     SIZE 20.72 BY 1
     BGCOLOR 15 FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE COMBO-BOX-CodAlm AS CHARACTER FORMAT "X(256)":U 
     LABEL "Almac�n" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE F-Codfam AS CHARACTER FORMAT "X(3)":U 
     LABEL "Familia" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE F-marca AS CHARACTER FORMAT "X(256)":U 
     LABEL "Marca" 
     VIEW-AS FILL-IN 
     SIZE 20.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-SubFam AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sub-Familia" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-codigo AS CHARACTER FORMAT "X(9)":U 
     LABEL "Codigo" 
     VIEW-AS FILL-IN 
     SIZE 10.14 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-filtro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE RADIO-SET-Stock AS CHARACTER INITIAL "C" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", "T",
"Solo con Stock", "C"
     SIZE 21 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      VtaListaMay, 
      Almmmatg, 
      Almmmate SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table L-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      VtaListaMay.codfam COLUMN-LABEL "C�d!Fam" FORMAT "X(3)":U
      VtaListaMay.subfam COLUMN-LABEL "Sub!Fam" FORMAT "X(3)":U
      VtaListaMay.codmat COLUMN-LABEL "Codigo!Articulo" FORMAT "X(6)":U
      VtaListaMay.DesMat FORMAT "X(80)":U WIDTH 50.14
      VtaListaMay.DesMar COLUMN-LABEL "Marca" FORMAT "X(30)":U
            WIDTH 19.86
      F-PRESOLA @ VtaListaMay.PreOfi COLUMN-LABEL "Precio Venta S/." FORMAT ">>>,>>9.9999":U
      VtaListaMay.Chr__01 COLUMN-LABEL "Unidad!Venta" FORMAT "X(8)":U
      Almmmatg.UndBas FORMAT "X(4)":U
      _StkAct() @ F-STKACT COLUMN-LABEL "Stock Actual" FORMAT "->>>>,>>9.99":U
      _Comprometido(Almmmatg.codmat, x-codalm) @ f-StockComprometido COLUMN-LABEL "Stock!Comprometido" FORMAT "->>>>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 130 BY 10.38
         BGCOLOR 15 FGCOLOR 0 FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     COMBO-BOX-CodAlm AT ROW 1 COL 7 COLON-ALIGNED WIDGET-ID 8
     FILL-IN-codigo AT ROW 2.08 COL 7.14 COLON-ALIGNED
     F-marca AT ROW 2.08 COL 23.14 COLON-ALIGNED
     RADIO-SET-Stock AT ROW 2.08 COL 53.14 NO-LABEL WIDGET-ID 4
     F-Codfam AT ROW 3.15 COL 7.14 COLON-ALIGNED
     F-SubFam AT ROW 3.15 COL 23.14 COLON-ALIGNED
     CMB-filtro AT ROW 3.15 COL 32.14 NO-LABEL
     FILL-IN-filtro AT ROW 3.15 COL 53.14 NO-LABEL
     br_table AT ROW 4.15 COL 2
     "Presione F8 para consultar el stock en otros almacenes" VIEW-AS TEXT
          SIZE 39 BY .5 AT ROW 14.73 COL 2.14 WIDGET-ID 2
          BGCOLOR 7 FGCOLOR 15 
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
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW L-table-Win ASSIGN
         HEIGHT             = 15.5
         WIDTH              = 139.72.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB L-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW L-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table FILL-IN-filtro F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB-filtro IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-filtro IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.VtaListaMay,INTEGRAL.Almmmatg OF INTEGRAL.VtaListaMay,INTEGRAL.Almmmate OF INTEGRAL.Almmmatg"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ",, FIRST, FIRST"
     _Where[1]         = "{&CONDICION}"
     _Where[3]         = "Almmmate.CodAlm = x-codalm
 AND (RADIO-SET-Stock = 'T' OR Almmmate.StkAct > 0)"
     _FldNameList[1]   > INTEGRAL.VtaListaMay.codfam
"VtaListaMay.codfam" "C�d!Fam" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.VtaListaMay.subfam
"VtaListaMay.subfam" "Sub!Fam" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.VtaListaMay.codmat
"VtaListaMay.codmat" "Codigo!Articulo" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.VtaListaMay.DesMat
"VtaListaMay.DesMat" ? "X(80)" "character" ? ? ? ? ? ? no ? no no "50.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > INTEGRAL.VtaListaMay.DesMar
"VtaListaMay.DesMar" "Marca" ? "character" ? ? ? ? ? ? no ? no no "19.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"F-PRESOLA @ VtaListaMay.PreOfi" "Precio Venta S/." ">>>,>>9.9999" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > INTEGRAL.VtaListaMay.Chr__01
"VtaListaMay.Chr__01" "Unidad!Venta" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = INTEGRAL.Almmmatg.UndBas
     _FldNameList[9]   > "_<CALC>"
"_StkAct() @ F-STKACT" "Stock Actual" "->>>>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"_Comprometido(Almmmatg.codmat, x-codalm) @ f-StockComprometido" "Stock!Comprometido" "->>>>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table L-table-Win
ON ANY-PRINTABLE OF br_table IN FRAME F-Main
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table L-table-Win
ON F8 OF br_table IN FRAME F-Main
DO:
    s-codmat = Almmmatg.codmat.
  RUN vta/d-stkalm.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table L-table-Win
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
OR "RETURN" OF br_table
DO:
    RUN p-aceptar IN whpadre.
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
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB-filtro L-table-Win
ON VALUE-CHANGED OF CMB-filtro IN FRAME F-Main
DO:
    IF CMB-filtro = CMB-filtro:SCREEN-VALUE AND
        FILL-IN-filtro = FILL-IN-filtro:SCREEN-VALUE AND
        F-Codfam = F-Codfam:SCREEN-VALUE AND
        F-Subfam = F-Subfam:SCREEN-VALUE AND
        F-marca = F-marca:SCREEN-VALUE AND
        RADIO-SET-Stock = RADIO-SET-Stock:SCREEN-VALUE AND
        COMBO-BOX-CodAlm = COMBO-BOX-CodAlm:SCREEN-VALUE
        THEN RETURN.
    ASSIGN
        FILL-IN-filtro
        CMB-filtro
        F-Codfam
        F-Subfam
        F-marca
        RADIO-SET-Stock
        COMBO-BOX-CodAlm.
    x-CodAlm = ENTRY(1, COMBO-BOX-CodAlm, ' - ').
        
    CASE CMB-filtro:
         WHEN 'Todos':U THEN DO:
             RUN set-attribute-list('Key-Name=?').
         END.
         OTHERWISE RUN set-attribute-list('Key-Name=' + CMB-filtro).
    END CASE.
    RUN dispatch IN THIS-PROCEDURE('open-query-cases':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-CodAlm
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-CodAlm L-table-Win
ON VALUE-CHANGED OF COMBO-BOX-CodAlm IN FRAME F-Main /* Almac�n */
DO:
    APPLY "VALUE-CHANGED" TO CMB-filtro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Codfam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Codfam L-table-Win
ON LEAVE OF F-Codfam IN FRAME F-Main /* Familia */
DO:
  APPLY "VALUE-CHANGED" TO CMB-filtro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Codfam L-table-Win
ON MOUSE-SELECT-DBLCLICK OF F-Codfam IN FRAME F-Main /* Familia */
OR F8 OF F-Codfam
DO:
  RUN LKUP\C-famili.R ("Maestro de Familias").
  IF output-var-1 <> ? THEN 
     F-CodFam:SCREEN-VALUE = output-var-2. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-marca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-marca L-table-Win
ON LEAVE OF F-marca IN FRAME F-Main /* Marca */
DO:
  APPLY "VALUE-CHANGED" TO CMB-filtro.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-SubFam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-SubFam L-table-Win
ON LEAVE OF F-SubFam IN FRAME F-Main /* Sub-Familia */
DO:
  APPLY "VALUE-CHANGED" TO CMB-filtro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-SubFam L-table-Win
ON MOUSE-SELECT-DBLCLICK OF F-SubFam IN FRAME F-Main /* Sub-Familia */
OR F8 OF F-CodFam
DO:
  RUN LKUP\C-Famili.R ("Maestro de Familias").
  IF output-var-1 <> ? THEN 
     F-CodFam:SCREEN-VALUE = output-var-2. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-codigo L-table-Win
ON LEAVE OF FILL-IN-codigo IN FRAME F-Main /* Codigo */
DO:
    IF INPUT FILL-IN-codigo = "" THEN RETURN.
    &IF "{&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}" &THEN
        FIND FIRST {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} WHERE
            {&CONDICION} AND
            ( {&CODIGO} = string (integer(INPUT FILL-IN-codigo:screen-value),"999999" ))
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
                    CMB-filtro:SCREEN-VALUE = CMB-filtro:ENTRY(1).
                APPLY "VALUE-CHANGED" TO CMB-filtro.
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
    APPLY "VALUE-CHANGED" TO CMB-filtro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-Stock
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-Stock L-table-Win
ON VALUE-CHANGED OF RADIO-SET-Stock IN FRAME F-Main
DO:
    APPLY "VALUE-CHANGED" TO CMB-filtro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK L-table-Win 


/* ***************************  Main Block  *************************** */

ON FIND OF ALMMMATG
DO:
  IF Almmmatg.MonVta = 1 THEN DO:
    F-PRESOLA = VtaListaMay.PreOfi.
    F-PREUSSA = VtaListaMay.PreOfi / VtaListaMay.TpoCmb.
  END.
  ELSE DO:
    F-PREUSSA = VtaListaMay.PreOfi.
    F-PRESOLA = VtaListaMay.PreOfi * VtaListaMay.TpoCmb.
  END.
  
END.
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases L-table-Win  adm/support/_adm-opn.p
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
           &Scope SORTBY-PHRASE BY VtaListaMay.CodCia BY VtaListaMay.codmat
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Familia':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.codfam
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Descripcion':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.DesMat
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Marca':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.DesMar
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Sub-Familia':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.codfam BY VtaListaMay.subfam
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
           &Scope SORTBY-PHRASE BY VtaListaMay.CodCia BY VtaListaMay.codmat
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Familia':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.codfam
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Descripcion':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.DesMat
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Marca':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.DesMar
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Sub-Familia':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.codfam BY VtaListaMay.subfam
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         OTHERWISE DO:
           &Undefine SORTBY-PHRASE
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END. /* OTHERWISE...*/
       END CASE.
    END. /* Nombres que contengan */
    OTHERWISE DO:
       &Scope KEY-PHRASE TRUE
       RUN get-attribute ('SortBy-Case':U).
       CASE RETURN-VALUE:
         WHEN 'Codigo':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.CodCia BY VtaListaMay.codmat
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Familia':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.codfam
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Descripcion':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.DesMat
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Marca':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.DesMar
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Sub-Familia':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaListaMay.codfam BY VtaListaMay.subfam
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available L-table-Win  _ADM-ROW-AVAILABLE
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
        output-var-2 = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.codmat
        output-var-3 = x-codalm.

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI L-table-Win  _DEFAULT-DISABLE
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
    DEF VAR i AS INT NO-UNDO.
    DEF VAR k AS INT INIT 1 NO-UNDO.

    RUN get-attribute ('Keys-Accepted').

    IF RETURN-VALUE <> "" AND RETURN-VALUE <> ? THEN
        ASSIGN
            CMB-filtro:LIST-ITEMS IN FRAME {&FRAME-NAME} =
            CMB-filtro:LIST-ITEMS + "," + RETURN-VALUE.

    DO WITH FRAME {&FRAME-NAME}:
        DO i = 1 TO NUM-ENTRIES(s-CodAlm):
            FIND Almacen WHERE Almacen.codcia = s-codcia
                AND Almacen.codalm = ENTRY(i, s-CodAlm)
                NO-LOCK NO-ERROR.
            IF AVAILABLE Almacen THEN DO:
                COMBO-BOX-CodAlm:ADD-LAST(almacen.codalm + ' - ' + almacen.descrip).
                /*IF Almacen.codalm = pCodAlm THEN DO:*/
                IF Almacen.codalm = input-var-1 THEN DO:
                    k = i.
                    COMBO-BOX-CodAlm:SCREEN-VALUE =  almacen.codalm + ' - ' + almacen.descrip.
                END.
            END.
            IF i = 1 THEN COMBO-BOX-CodAlm:SCREEN-VALUE =  almacen.codalm + ' - ' + almacen.descrip.
        END.
        x-CodAlm = ENTRY(k, s-CodAlm).
    END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key L-table-Win  adm/support/_key-snd.p
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records L-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "VtaListaMay"}
  {src/adm/template/snd-list.i "Almmmatg"}
  {src/adm/template/snd-list.i "Almmmate"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION _Comprometido L-table-Win 
FUNCTION _Comprometido RETURNS DECIMAL
  ( INPUT pCodMat AS CHAR, INPUT pCodAlm AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR pComprometido AS DEC.

RUN vtagn/stock-comprometido (pCodMat, pCodAlm, OUTPUT pComprometido).

  RETURN pComprometido.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION _StkAct L-table-Win 
FUNCTION _StkAct RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR pAlmacenes AS CHAR.
DEF VAR pCodMat AS CHAR.
DEF VAR pStkAct AS DEC.

ASSIGN
    pAlmacenes = x-codalm
    pCodMat = Almmmatg.codmat.
RUN vtagn/p-stkact-01 (pAlmacenes, pCodMat, OUTPUT pStkAct).

  RETURN pStkAct.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

