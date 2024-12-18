&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER B-MATE FOR Almmmate.
DEFINE SHARED TEMP-TABLE T-GENER NO-UNDO LIKE TabGener.
DEFINE TEMP-TABLE T-MATE NO-UNDO LIKE Almmmate.
DEFINE TEMP-TABLE T-MATG NO-UNDO LIKE Almmmatg.
DEFINE SHARED TEMP-TABLE t-report NO-UNDO LIKE w-report.



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

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR pv-codcia AS INT.

DEF VAR x-DesMat AS CHAR NO-UNDO.

&SCOPED-DEFINE Condicion Almmmatg.codcia = s-codcia ~
    AND ( COMBO-BOX-CodFam = 'Todas' OR Almmmatg.codfam = COMBO-BOX-CodFam ) ~
    AND ( COMBO-BOX-SubFam = 'Todas' OR Almmmatg.subfam = COMBO-BOX-SubFam ) ~
    AND ( RADIO-SET-TpoArt = 'Todos' OR Almmmatg.tpoart = RADIO-SET-TpoArt) ~
    AND ( FILL-IN-DesMat = '' OR INDEX(Almmmatg.desmat, FILL-IN-DesMat) > 0) ~
    AND ( FILL-IN-DesMar = '' OR INDEX(Almmmatg.desmar, FILL-IN-DesMar) > 0) ~
    AND ( FILL-IN-CodPro = '' OR Almmmatg.CodPr1 = FILL-IN-CodPro )

/* &SCOPED-DEFINE Condicion Almmmatg.codcia = s-codcia ~                          */
/*     AND ( FILL-IN-CodMat = '' OR Almmmatg.codmat = FILL-IN-CodMat) ~           */
/*     AND ( COMBO-BOX-CodFam = 'Todas' OR Almmmatg.codfam = COMBO-BOX-CodFam ) ~ */
/*     AND ( COMBO-BOX-SubFam = 'Todas' OR Almmmatg.subfam = COMBO-BOX-SubFam ) ~ */
/*     AND ( RADIO-SET-TpoArt = 'Todos' OR Almmmatg.tpoart = RADIO-SET-TpoArt) ~  */
/*     AND ( FILL-IN-DesMat = '' OR INDEX(Almmmatg.desmat, FILL-IN-DesMat) > 0) ~ */
/*     AND ( FILL-IN-DesMar = '' OR INDEX(Almmmatg.desmar, FILL-IN-DesMar) > 0)   */

DEF VAR pTipo AS CHAR INIT "NC".     /* CAMPA�A  (NC NO CAMPA�A) */
DEF BUFFER BT-MATE FOR T-MATE.

DEF SHARED VAR lh_handle AS HANDLE.

DEFINE TEMP-TABLE tmp-tabla
    FIELD t-CodAlm LIKE Almacen.codalm  FORMAT 'x(3)'
    FIELD t-CodDoc LIKE FacDPedi.CodDoc FORMAT "XXX"
    FIELD t-Nroped LIKE FacDPedi.NroPed FORMAT "XXX-XXXXXXXX"
    FIELD t-CodDiv LIKE FacCPedi.CodDiv FORMAT 'x(5)'
    FIELD t-FchPed LIKE FacDPedi.FchPed
    FIELD t-NomCli LIKE FacCPedi.NomCli COLUMN-LABEL "Cliente" FORMAT "x(35)"
    FIELD t-CodMat LIKE FacDPedi.codmat
    FIELD t-Canped LIKE FacDPedi.CanPed.

DEFINE TEMP-TABLE tt-excel
    FIELD   tcodmat     LIKE almmmatg.codmat    COLUMN-LABEL "Cod.Art"
    FIELD   tdesmat     LIKE almmmatg.desmat    COLUMN-LABEL "Descripcion Articulo"
    FIELD   tcodfam     AS  CHAR    FORMAT  'x(80)'  COLUMN-LABEL "Linea"
    FIELD   tsubfam     AS  CHAR    FORMAT  'x(80)' COLUMN-LABEL "SubLinea"
    FIELD   tmarca      AS  CHAR    FORMAT  'x(80)' COLUMN-LABEL "Marca"
    FIELD   tunidad     AS  CHAR    FORMAT  'x(5)'  COLUMN-LABEL "Unidad"
    FIELD   tproveedor  AS  CHAR    FORMAT  'x(80)' COLUMN-LABEL "Proveedor"
    FIELD   tvol        AS  DEC     COLUMN-LABEL "Volumen m3"
    FIELD   tpeso       AS  DEC     COLUMN-LABEL "Peso Kg"
    FIELD   tcosrepo    AS  DEC     COLUMN-LABEL "Costo Reposicion"
    FIELD   tclsfG      AS  CHAR    FORMAT  'x(5)'  COLUMN-LABEL "Clasf. General"
    FIELD   tclsfU      AS  CHAR    FORMAT  'x(5)'  COLUMN-LABEL "Clasf. Utilex"
    FIELD   tclsfM      AS  CHAR    FORMAT  'x(5)'  COLUMN-LABEL "Clasf. Mayorista"
    FIELD   temprepo    AS  DEC     FORMAT  '->>,>>>,>>9.99'    COLUMN-LABEL "Emp Repo"
    FIELD   tempmaster  AS  DEC     FORMAT  '->>,>>>,>>9.99'    COLUMN-LABEL "Emp Master"
    FIELD   tempinner   AS  DEC     FORMAT  '->>,>>>,>>9.99'    COLUMN-LABEL "Emp Inner"
    FIELD   tgrupos     AS  CHAR    FORMAT  'x(25)' COLUMN-LABEL "Grupos" EXTENT 25
    FIELD   tgrpoalm    AS  CHAR    FORMAT  'x(25)' COLUMN-LABEL "Grupos" EXTENT 25

    FIELD   tstkdispo   AS  DEC FORMAT  '->>,>>>,>>9.99'    COLUMN-LABEL "Stk Disponible" EXTENT 25
    FIELD   tstkmax     AS  DEC FORMAT  '->>,>>>,>>9.99'    COLUMN-LABEL "Stk Maximo" EXTENT 25
    FIELD   tstksegu    AS  DEC FORMAT  '->>,>>>,>>9.99'    COLUMN-LABEL "Stk Seguridad" EXTENT 25
    FIELD   tstktrans   AS  DEC FORMAT  '->>,>>>,>>9.99'    COLUMN-LABEL "Stk Transito" EXTENT 25.

DEFINE TEMP-TABLE ttZonas
    FIELD   tcodzona    AS  CHAR    FORMAT 'x(25)'
    FIELD   tcodalm     AS  CHAR    FORMAT 'x(5)'
    FIELD   tmainalm    AS  CHAR    FORMAT 'x(1)'
    FIELD   tposdispo   AS  INT     FORMAT '>>,>>>' INIT 0
    FIELD   tposmax     AS  INT     FORMAT '>>,>>>' INIT 0
    FIELD   tposseg     AS  INT     FORMAT '>>,>>>' INIT 0
    FIELD   tpostrans   AS  INT     FORMAT '>>,>>>' INIT 0
    FIELD   tposcompr   AS  INT     FORMAT '>>,>>>' INIT 0
    FIELD   tposdeta    AS  INT     FORMAT '>>,>>>' INIT 0
    FIELD   tswOk       AS  INT     FORMAT '>,>>>,>>>' INIT 0

    INDEX idx01 IS PRIMARY tcodzona tcodalm.

DEFINE IMAGE IMAGE-1 FILENAME "IMG\clock.ico" SIZE 5 BY 1.5.
DEF VAR FI-MENSAJE AS CHAR FORMAT "X(40)" .

DEFINE FRAME F-Proceso
     IMAGE-1 AT ROW 1.5 COL 5
     "Espere un momento" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 1.5 COL 16 FONT 6
     "por favor ...." VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 2.5 COL 19 FONT 6
          SKIP
     Fi-Mensaje FORMAT 'x(30)' NO-LABEL FONT 6
     SKIP     
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         BGCOLOR 15 FGCOLOR 0 
         TITLE "Procesando ..." FONT 7.

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
&Scoped-define INTERNAL-TABLES T-MATG Almmmatg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table Almmmatg.codmat Almmmatg.DesMat ~
Almmmatg.UndStk Almmmatg.DesMar 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH T-MATG WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST Almmmatg OF T-MATG NO-LOCK ~
    BY T-MATG.DesMat
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH T-MATG WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST Almmmatg OF T-MATG NO-LOCK ~
    BY T-MATG.DesMat.
&Scoped-define TABLES-IN-QUERY-br_table T-MATG Almmmatg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table T-MATG
&Scoped-define SECOND-TABLE-IN-QUERY-br_table Almmmatg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rsCuales BUTTON-12 FILL-IN-CodPro BUTTON-11 ~
RADIO-SET-TpoArt COMBO-BOX-CodFam FILL-IN-CodMat COMBO-BOX-SubFam ~
FILL-IN-DesMat FILL-IN-DesMar BUTTON-10 BUTTON-6 br_table RECT-1 
&Scoped-Define DISPLAYED-OBJECTS rsCuales FILL-IN-CodPro FILL-IN-NomPro ~
EDITOR-1 FILL-IN-Temporada RADIO-SET-TpoArt COMBO-BOX-CodFam FILL-IN-CodMat ~
COMBO-BOX-SubFam FILL-IN-DesMat FILL-IN-DesMar FILL-IN-Mensaje 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fComprasTransito B-table-Win 
FUNCTION fComprasTransito RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fPosCelda B-table-Win 
FUNCTION fPosCelda RETURNS INTEGER
  ( INPUT cCodZona AS CHAR, INPUT cAlm AS CHAR, INPUT nBloque AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fStockComprometido B-table-Win 
FUNCTION fStockComprometido RETURNS DECIMAL
  ( INPUT pCodMat AS CHAR, INPUT pCodAlm AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fStockTransito B-table-Win 
FUNCTION fStockTransito RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-10 
     LABEL "Aplicar Filtros" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "img/b-buscar.ico":U
     LABEL "Button 11" 
     SIZE 4 BY .96.

DEFINE BUTTON BUTTON-12 
     LABEL "Excel" 
     SIZE 10 BY 1.12.

DEFINE BUTTON BUTTON-6 
     LABEL "Limpiar Filtros" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE COMBO-BOX-CodFam AS CHARACTER FORMAT "X(256)":U INITIAL "Todas" 
     LABEL "L�nea" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "Todas","Todas"
     DROP-DOWN-LIST
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-SubFam AS CHARACTER FORMAT "X(256)":U INITIAL "Todas" 
     LABEL "Sub-L�nea" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "Todas","Todas"
     DROP-DOWN-LIST
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 34 BY 1.54 NO-UNDO.

DEFINE VARIABLE FILL-IN-CodMat AS CHARACTER FORMAT "X(256)":U 
     LABEL "C�digo Art�culo" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-CodPro AS CHARACTER FORMAT "X(8)":U 
     LABEL "Proveedor" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-DesMar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Marca" 
     VIEW-AS FILL-IN 
     SIZE 45 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-DesMat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descripci�n" 
     VIEW-AS FILL-IN 
     SIZE 45 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-Mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-NomPro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-Temporada AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.15
     BGCOLOR 0 FGCOLOR 14 FONT 8 NO-UNDO.

DEFINE VARIABLE RADIO-SET-TpoArt AS CHARACTER INITIAL "A" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activado", "A",
"Desactivado", "D",
"Baja Rotaci�n", "B",
"Todos", "Todos"
     SIZE 44 BY .81 NO-UNDO.

DEFINE VARIABLE rsCuales AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 1,
"Con valores", 2
     SIZE 22 BY .77 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 2.88.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      T-MATG, 
      Almmmatg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      Almmmatg.codmat COLUMN-LABEL "Articulo" FORMAT "X(6)":U WIDTH 8.43
      Almmmatg.DesMat FORMAT "X(60)":U WIDTH 64.43
      Almmmatg.UndStk COLUMN-LABEL "Unidad" FORMAT "X(6)":U WIDTH 9.14
      Almmmatg.DesMar COLUMN-LABEL "Marca" FORMAT "X(20)":U WIDTH 19
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 106 BY 4.77
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rsCuales AT ROW 5.31 COL 69.86 NO-LABEL WIDGET-ID 54
     BUTTON-12 AT ROW 9.54 COL 108 WIDGET-ID 52
     FILL-IN-CodPro AT ROW 5.23 COL 12 COLON-ALIGNED WIDGET-ID 48
     FILL-IN-NomPro AT ROW 5.23 COL 24 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     BUTTON-11 AT ROW 3.5 COL 110 WIDGET-ID 40
     EDITOR-1 AT ROW 3.5 COL 74 NO-LABEL WIDGET-ID 42
     FILL-IN-Temporada AT ROW 1 COL 88 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     RADIO-SET-TpoArt AT ROW 1.19 COL 15 NO-LABEL WIDGET-ID 16
     COMBO-BOX-CodFam AT ROW 2.15 COL 12 COLON-ALIGNED WIDGET-ID 14
     FILL-IN-CodMat AT ROW 2.54 COL 72 COLON-ALIGNED WIDGET-ID 34
     COMBO-BOX-SubFam AT ROW 2.92 COL 12 COLON-ALIGNED WIDGET-ID 26
     FILL-IN-DesMat AT ROW 3.69 COL 12 COLON-ALIGNED WIDGET-ID 12
     FILL-IN-DesMar AT ROW 4.46 COL 12 COLON-ALIGNED WIDGET-ID 30
     FILL-IN-Mensaje AT ROW 6 COL 62.72 NO-LABEL WIDGET-ID 28
     BUTTON-10 AT ROW 1 COL 60 WIDGET-ID 32
     BUTTON-6 AT ROW 1 COL 75 WIDGET-ID 24
     br_table AT ROW 6.85 COL 1
     "Estado:" VIEW-AS TEXT
          SIZE 6 BY .5 AT ROW 1.38 COL 9 WIDGET-ID 22
     "Desde Texto:" VIEW-AS TEXT
          SIZE 9 BY .5 AT ROW 3.69 COL 64 WIDGET-ID 46
     RECT-1 AT ROW 2.35 COL 60 WIDGET-ID 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: B-MATE B "?" ? INTEGRAL Almmmate
      TABLE: T-GENER T "SHARED" NO-UNDO INTEGRAL TabGener
      TABLE: T-MATE T "?" NO-UNDO INTEGRAL Almmmate
      TABLE: T-MATG T "?" NO-UNDO INTEGRAL Almmmatg
      TABLE: t-report T "SHARED" NO-UNDO INTEGRAL w-report
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
         HEIGHT             = 13.65
         WIDTH              = 129.57.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB br_table BUTTON-6 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Mensaje IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-NomPro IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Temporada IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "Temp-Tables.T-MATG,INTEGRAL.Almmmatg OF Temp-Tables.T-MATG"
     _Options          = "NO-LOCK KEY-PHRASE"
     _TblOptList       = ", FIRST"
     _OrdList          = "Temp-Tables.T-MATG.DesMat|yes"
     _FldNameList[1]   > INTEGRAL.Almmmatg.codmat
"Almmmatg.codmat" "Articulo" ? "character" ? ? ? ? ? ? no ? no no "8.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.Almmmatg.DesMat
"Almmmatg.DesMat" ? "X(60)" "character" ? ? ? ? ? ? no ? no no "64.43" yes no no "U" "" "" "FILL-IN" "," ? ? 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.Almmmatg.UndStk
"Almmmatg.UndStk" "Unidad" "X(6)" "character" ? ? ? ? ? ? no ? no no "9.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.Almmmatg.DesMar
"Almmmatg.DesMar" "Marca" "X(20)" "character" ? ? ? ? ? ? no ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main B-table-Win
ON GO OF FRAME F-Main
DO:
  ASSIGN
      COMBO-BOX-CodFam = 'Todas'
      FILL-IN-DesMat   = ''
      RADIO-SET-TpoArt = 'A'
      x-DesMat = ''.
RUN dispatch IN THIS-PROCEDURE ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define SELF-NAME BUTTON-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-10 B-table-Win
ON CHOOSE OF BUTTON-10 IN FRAME F-Main /* Aplicar Filtros */
DO:
  ASSIGN
      COMBO-BOX-CodFam COMBO-BOX-SubFam FILL-IN-DesMar FILL-IN-DesMat RADIO-SET-TpoArt RsCuales
      FILL-IN-CodMat EDITOR-1
      FILL-IN-CodPro.
  ASSIGN
      COMBO-BOX-CodFam:SENSITIVE = NO
      COMBO-BOX-SubFam:SENSITIVE = NO
      FILL-IN-CodMat:SENSITIVE = NO
      FILL-IN-DesMar:SENSITIVE = NO
      FILL-IN-DesMat:SENSITIVE = NO
      RADIO-SET-TpoArt:SENSITIVE = NO
      FILL-IN-CodPro:SENSITIVE = NO.
  SESSION:SET-WAIT-STATE('GENERAL').
  RUN Carga-Temporal.
  SESSION:SET-WAIT-STATE('').

  RUN Procesa-Handle IN lh_handle ('Open-Queries').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-11 B-table-Win
ON CHOOSE OF BUTTON-11 IN FRAME F-Main /* Button 11 */
DO:
  DEF VAR x-Archivo AS CHAR NO-UNDO.
  DEF VAR rpta AS LOG NO-UNDO.

  SYSTEM-DIALOG GET-FILE x-Archivo
      MUST-EXIST
      TITLE "Seleccione el archivo con los art�culos"
      UPDATE rpta.
  IF rpta = NO THEN RETURN NO-APPLY.
  EDITOR-1:SCREEN-VALUE = x-Archivo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 B-table-Win
ON CHOOSE OF BUTTON-12 IN FRAME F-Main /* Excel */
DO:
  RUN envio-excel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 B-table-Win
ON CHOOSE OF BUTTON-6 IN FRAME F-Main /* Limpiar Filtros */
DO:
  ASSIGN
      COMBO-BOX-CodFam = 'Todas'
      COMBO-BOX-SubFam = 'Todas'
      FILL-IN-DesMat   = ''
      RADIO-SET-TpoArt = 'A'
      x-DesMat = ''
      FILL-IN-DesMar = ''
      FILL-IN-CodMat = ''
      EDITOR-1 = ''
      FILL-IN-CodPro = ''
      FILL-IN-NomPro = ''.
  ASSIGN
      COMBO-BOX-CodFam:SENSITIVE = YES
      COMBO-BOX-SubFam:SENSITIVE = YES
      FILL-IN-CodMat:SENSITIVE = YES
      FILL-IN-DesMar:SENSITIVE = YES
      FILL-IN-DesMat:SENSITIVE = YES
      RADIO-SET-TpoArt:SENSITIVE = YES
      FILL-IN-CodPro:SENSITIVE = YES.

  EMPTY TEMP-TABLE T-MATG.
  EMPTY TEMP-TABLE t-report.

  /*RUN dispatch IN THIS-PROCEDURE ('open-query':U).*/
  RUN Procesa-Handle IN lh_handle ('Open-Queries').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-CodFam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-CodFam B-table-Win
ON VALUE-CHANGED OF COMBO-BOX-CodFam IN FRAME F-Main /* L�nea */
DO:
    ASSIGN {&self-name}.
    /* Carga Sublineas */
    DO WITH FRAME {&FRAME-NAME}:
        COMBO-BOX-SubFam:DELETE(COMBO-BOX-SubFam:LIST-ITEM-PAIRS).
        COMBO-BOX-SubFam:ADD-LAST('Todas','Todas').
        FOR EACH Almsfami NO-LOCK WHERE Almsfami.codcia = s-codcia
            AND Almsfami.codfam = COMBO-BOX-CodFam:
            COMBO-BOX-SubFam:ADD-LAST( AlmSFami.subfam + ' - ' + REPLACE(AlmSFami.dessub, ',', ' ')
                                       , AlmSFami.subfam).
        END.
        COMBO-BOX-SubFam:SCREEN-VALUE = 'Todas'.
        APPLY 'VALUE-CHANGED':U TO COMBO-BOX-SubFam.
    END.
    /*RUN dispatch IN THIS-PROCEDURE ('open-query':U).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-SubFam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-SubFam B-table-Win
ON VALUE-CHANGED OF COMBO-BOX-SubFam IN FRAME F-Main /* Sub-L�nea */
DO:
    ASSIGN {&self-name}.
    /*RUN dispatch IN THIS-PROCEDURE ('open-query':U).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-CodMat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodMat B-table-Win
ON LEAVE OF FILL-IN-CodMat IN FRAME F-Main /* C�digo Art�culo */
DO:
    DEF VAR pCodMat AS CHAR NO-UNDO.
    pCodMat = SELF:SCREEN-VALUE.
    RUN vta2/p-codigo-producto (INPUT-OUTPUT pCodMat, YES).
    /*IF pCodMat = '' THEN RETURN NO-APPLY.*/
    SELF:SCREEN-VALUE = pCodMat.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-CodPro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodPro B-table-Win
ON LEAVE OF FILL-IN-CodPro IN FRAME F-Main /* Proveedor */
DO:
  FIND gn-prov WHERE gn-prov.CodCia = pv-codcia
      AND gn-prov.CodPro = INPUT {&SELF-NAME}
      NO-LOCK NO-ERROR.
  IF AVAILABLE gn-prov THEN FILL-IN-NomPro:SCREEN-VALUE = gn-prov.NomPro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DesMat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DesMat B-table-Win
ON ANY-PRINTABLE OF FILL-IN-DesMat IN FRAME F-Main /* Descripci�n */
DO:
/*     ASSIGN {&self-name}.                             */
/*     x-desmat = FILL-IN-DesMat.                       */
/*     RUN dispatch IN THIS-PROCEDURE ('open-query':U). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-TpoArt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-TpoArt B-table-Win
ON VALUE-CHANGED OF RADIO-SET-TpoArt IN FRAME F-Main
DO:
/*   ASSIGN {&self-name}.                             */
/*   RUN dispatch IN THIS-PROCEDURE ('open-query':U). */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal B-table-Win 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-----------------------------------------------------------------------------*/

EMPTY TEMP-TABLE T-MATG.
EMPTY TEMP-TABLE T-GENER.

DEFINE VAR lPasoFiltro AS LOG.

IF EDITOR-1 <> '' THEN DO:
    DEF VAR x-CodMat AS CHAR NO-UNDO.
    INPUT FROM VALUE(EDITOR-1).
    REPEAT:
        DEF VAR x-Linea AS CHAR NO-UNDO.
        IMPORT UNFORMATTED x-Linea.
        IF x-Linea = '' THEN LEAVE.
        FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
            AND Almmmatg.codmat = x-linea NO-LOCK NO-ERROR.
        IF AVAILABLE Almmmatg THEN DO:
            FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
            "*** Procesando C�digo: " + Almmmatg.codmat.
            CREATE T-MATG.
            BUFFER-COPY Almmmatg TO T-MATG.
        END.
    END.
    INPUT CLOSE.
END.
ELSE DO:
    IF FILL-IN-CodMat <> '' THEN DO:
        FOR EACH Almmmatg NO-LOCK WHERE Almmmatg.codmat = FILL-IN-CodMat
            AND {&Condicion}:
            FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
                        "*** Procesando C�digo: " + Almmmatg.codmat.
            CREATE T-MATG.
            BUFFER-COPY Almmmatg TO T-MATG.
        END.
    END.
    ELSE DO:
        FOR EACH Almmmatg NO-LOCK WHERE {&Condicion}:
            FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
                        "*** Procesando C�digo: " + Almmmatg.codmat.
            CREATE T-MATG.
            BUFFER-COPY Almmmatg TO T-MATG.
        END.
    END.
END.

DEF VAR pStkComprometido AS DEC NO-UNDO.
DEF VAR x-Item AS INTE NO-UNDO.

FOR EACH T-MATG NO-LOCK,
    EACH TabGener NO-LOCK WHERE TabGener.CodCia = T-MATG.codcia
        AND TabGener.Clave = "ZG",
    FIRST Almtabla NO-LOCK WHERE Almtabla.tabla = Tabgener.clave
        AND Almtabla.codigo = Tabgener.codigo,
    FIRST Almmmate OF T-MATG NO-LOCK WHERE Almmmate.codalm = TabGener.Libre_c01,
    FIRST Almacen OF Almmmate NO-LOCK:
    x-Item = x-Item + 1.
    /*IF x-Item MODULO 1000 = 0 THEN FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "*** CALCULANDO C�digo: " + T-MATG.codmat.*/
    /*FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "*** CALCULANDO C�digo: " + T-MATG.codmat.*/
    DISPLAY
    "  Procesando: " + T-MATG.CodMat @ Fi-Mensaje WITH FRAME F-Proceso.
    /* ------------------------ */
    CREATE T-GENER.
    BUFFER-COPY Tabgener TO T-GENER.
    pStkComprometido = fStockComprometido(Almmmate.CodMat, Almmmate.CodAlm).
    ASSIGN
        T-GENER.libre_c02 = Almacen.Descripcion
        T-GENER.libre_c03 = T-MATG.codmat
        T-GENER.libre_d01 = Almmmate.StkAct - pStkComprometido.
    ASSIGN
        T-GENER.libre_d02 = fStockTransito().
    ASSIGN
        T-GENER.Libre_d04 = fComprasTransito().
    /* Faltante/Sobrante */
    ASSIGN
        T-GENER.libre_d03 = (T-GENER.libre_d01 + T-GENER.libre_d02 + T-GENER.Libre_d04) - Almmmate.StkMin.
END.
/* Aplicar filtro Todos (1) o Con Valores (2) */
IF rsCuales = 2 THEN DO:
    FOR EACH t-gener EXCLUSIVE-LOCK:
        lPasoFiltro = NO.
        IF T-GENER.libre_d01 <> 0 THEN lPasoFiltro = YES.
        IF T-GENER.libre_d02 <> 0 THEN lPasoFiltro = YES.
        IF T-GENER.libre_d04 <> 0 THEN lPasoFiltro = YES.
        /**/
        FIND FIRST almmmate WHERE almmmate.codcia = s-codcia AND 
            almmmate.codalm = t-gener.libre_c01 AND
            almmmate.codmat = t-gener.libre_c03 NO-LOCK NO-ERROR.
        IF AVAILABLE almmmate THEN DO:
            IF almmmate.Vctmn1 <> 0 THEN lPasoFiltro = YES.
            IF almmmate.Vctmn2 <> 0 THEN lPasoFiltro = YES.
            IF almmmate.stkmax <> 0 THEN lPasoFiltro = YES.
        END.
        /* ---- */
        IF lPasoFiltro = NO THEN DELETE t-gener.
    END.
    /**/
    FOR EACH T-MATG EXCLUSIVE-LOCK:
        FIND FIRST T-GENER WHERE T-GENER.CodCia = T-MATG.CodCia
            AND T-GENER.Clave = "ZG"
            AND T-GENER.Libre_c03 = T-MATG.CodMat NO-LOCK NO-ERROR.
        /**/
        IF NOT AVAILABLE t-gener THEN DELETE T-MATG.
    END.

END.
/* ---------------- */
DEF BUFFER B-AlmTabla FOR AlmTabla.
EMPTY TEMP-TABLE t-report.
FOR EACH T-MATG NO-LOCK,
    EACH T-GENER NO-LOCK WHERE T-GENER.CodCia = T-MATG.CodCia
    AND T-GENER.Clave = "ZG"
    AND T-GENER.Libre_c03 = T-MATG.CodMat,
    FIRST Almmmate NO-LOCK WHERE Almmmate.codcia = T-GENER.CodCia
    AND Almmmate.CodAlm = T-GENER.Libre_c01
    AND Almmmate.CodMat = T-GENER.Libre_c03:
    FIND FIRST B-AlmTabla WHERE B-AlmTabla.Tabla = T-GENER.Clave
        AND B-AlmTabla.Codigo = T-GENER.Codigo
        NO-LOCK NO-ERROR.
    IF AVAILABLE B-AlmTabla THEN DO:
        FIND t-report WHERE t-report.Llave-C = B-AlmTabla.Codigo 
            AND t-report.Campo-C[10] = T-MATG.CodMat
            NO-ERROR.
        IF NOT AVAILABLE t-report THEN CREATE t-report.
        ASSIGN
            t-report.Llave-C    = B-AlmTabla.Codigo
            t-report.Campo-C[10]= T-MATG.CodMat 
            t-report.Campo-C[1] = CAPS(B-AlmTabla.Nombre)
            t-report.Campo-F[1] = t-report.Campo-F[1] + T-GENER.libre_d03.  /* Faltante/Sobrante */
        /* Datos Resumen */
        ASSIGN
            t-report.Campo-F[2] = t-report.Campo-F[2] + Almmmate.StockMax     /* Maximo */
            t-report.Campo-F[3] = t-report.Campo-F[3] + Almmmate.StockSeg     /* Seguridad */
            t-report.Campo-F[4] = t-report.Campo-F[4] + Almmmate.StkMin     /* Max + Seg */
            t-report.Campo-F[5] = t-report.Campo-F[5] + T-GENER.Libre_d01   /* Disponible */
            t-report.Campo-F[6] = t-report.Campo-F[6] + T-GENER.Libre_d02   /* Tr�nsito */
            t-report.Campo-F[7] = t-report.Campo-F[7] + T-GENER.Libre_d04   /* Compra Tr�nsito */
            .
    END.
END.
FOR EACH t-report EXCLUSIVE-LOCK:
    IF TRUE <> (t-report.Llave-C > '') THEN DELETE t-report.
END.

FOR EACH t-report EXCLUSIVE-LOCK,
    FIRST T-GENER NO-LOCK WHERE T-GENER.Codigo = t-report.Llave-C
        AND T-GENER.Libre_L01 = YES
        AND T-GENER.Libre_C03 = t-report.Campo-C[10]:
    FIND Almmmate WHERE Almmmate.codcia = s-codcia
        AND Almmmate.codalm = T-GENER.Libre_C01
        AND Almmmate.codmat = T-GENER.Libre_C03
        NO-LOCK NO-ERROR.
    IF AVAILABLE Almmmate THEN t-report.Campo-F[10] = Almmmate.stkmax.
END.

FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.
HIDE FRAME F-Proceso.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE envio-excel B-table-Win 
PROCEDURE envio-excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE BUFFER b-tabgener FOR tabgener.
DEFINE BUFFER bt-gener FOR t-gener.
DEFINE BUFFER b-almmmate FOR almmmate.

DEFINE VAR lSec AS INT.
DEFINE VAR lSecx AS INT.
DEFINE VAR lTotZona AS DEC.
DEFINE VAR lTotal AS DEC.
DEFINE VAR lColorIndex AS INT.
DEFINE VAR lOtroArt AS LOG.


DEFINE VAR lIniColAlmacenes AS INT.

EMPTY TEMP-TABLE ttZonas.

SESSION:SET-WAIT-STATE('GENERAL').

/* Todas las ZONAS definidas */
FOR EACH b-tabgener WHERE b-tabgener.codcia= s-codcia AND
                            b-tabgener.clave = 'ZG' NO-LOCK:
    FIND FIRST ttZonas WHERE ttZonas.tcodzona = b-tabgener.codigo AND
                                ttZonas.tcodalm = b-tabgener.libre_c01 NO-ERROR.
    IF NOT AVAILABLE ttZonas THEN DO:
        CREATE ttZonas.
        ASSIGN  ttZonas.tcodzona    = b-tabgener.codigo
                ttZonas.tcodalm     = b-tabgener.libre_c01
                ttZonas.tmainalm    = IF(b-tabgener.libre_l01 = YES) THEN 'S' ELSE 'N'.
    END.
END.

/* Sacar aquellos que no tienen movimiento ??? */
FOR EACH bt-gener BREAK BY libre_c03 BY codigo BY libre_c01 :
    FIND FIRST ttZonas WHERE ttZonas.tcodzona = bt-gener.codigo AND 
                                ttZonas.tcodalm = bt-gener.libre_c01 NO-ERROR.
    IF AVAILABLE ttZonas THEN DO:
        ASSIGN ttZonas.tswok = ttZonas.tswok + 1.
    END.
END.
FOR EACH ttZonas BREAK BY tcodzona :
    IF ttZonas.tswok = 0 THEN DO:
        DELETE ttZonas.
    END.
END.

lIniColAlmacenes = 16.
lSec = lIniColAlmacenes.

/* Guardo las ubicaciones por cada bloque */

/* Disponible */
FOR EACH ttZonas BREAK BY tcodzona BY tcodalm :
    IF FIRST-OF(tcodzona) THEN DO:
        /**/
    END.
    lSec = lSec + 1.
    ASSIGN ttZonas.tposdispo =lSec.
END.
/* Maximo */
FOR EACH ttZonas BREAK BY tcodzona BY tcodalm :
    IF FIRST-OF(tcodzona) THEN DO:
        /**/
    END.
    lSec = lSec + 1.
    ASSIGN ttZonas.tposmax =lSec.
END.
/* Seguridad */
FOR EACH ttZonas BREAK BY tcodzona BY tcodalm :
    IF FIRST-OF(tcodzona) THEN DO:
        /**/
    END.
    lSec = lSec + 1.
    ASSIGN ttZonas.tposseg =lSec.
END.
/* Transito por transferencias */
FOR EACH ttZonas BREAK BY tcodzona BY tcodalm :
    IF FIRST-OF(tcodzona) THEN DO:
        /**/
    END.
    lSec = lSec + 1.
    ASSIGN ttZonas.tpostrans =lSec.
END.
/* Transito por Compras */
FOR EACH ttZonas BREAK BY tcodzona BY tcodalm :
    IF FIRST-OF(tcodzona) THEN DO:
        /**/
    END.
    lSec = lSec + 1.
    ASSIGN ttZonas.tposcompr =lSec.
END.

/* Detallado */
FOR EACH ttZonas BREAK BY tcodzona BY tcodalm :
    IF FIRST-OF(tcodzona) THEN DO:
        /**/
    END.
    lSec = lSec + 1.
    ASSIGN ttZonas.tposdeta =lSec.
END.

/* -----------------------------------------------------------------------------------------------  */
DEFINE VARIABLE lFileXls                 AS CHARACTER.
DEFINE VARIABLE lNuevoFile               AS LOG.
define VAR cValue as char.


lFileXls = "".          /* Nombre el archivo a abrir o crear, vacio es valido solo para nuevos */
lNuevoFile = YES.       /* YES : Si va crear un nuevo archivo o abrir */


{lib\excel-open-file.i}

chExcelApplication:Visible = NO.

lMensajeAlTerminar = YES. /*  */
lCerrarAlTerminar = NO. /* Si permanece abierto el Excel luego de concluir el proceso */


chWorkSheet = chExcelApplication:Sheets:Item(1).  

iRow = 2.
iColumn = lIniColAlmacenes.
cColumn = ''.
lColorIndex = 32.

DEFINE VAR cFiler1 AS CHAR INIT "".
DEFINE VAR cFiler2 AS CHAR INIT "".
DEFINE VAR cFiler3 AS CHAR INIT "".
DEFINE VAR cFiler4 AS CHAR INIT "".
DEFINE VAR cFiler5 AS CHAR INIT "".
DEFINE VAR cFiler6 AS CHAR INIT "".
DEFINE VAR cFiler1x AS CHAR INIT "".
DEFINE VAR cFiler2x AS CHAR INIT "".
DEFINE VAR cFiler3x AS CHAR INIT "".
DEFINE VAR cFiler4x AS CHAR INIT "".
DEFINE VAR cFiler5x AS CHAR INIT "".
DEFINE VAR cFiler6x AS CHAR INIT "".

/**/
cFiler1 = "".
cFiler2 = "".
cFiler3 = "".
cFiler4 = "".
cFiler5 = "".
cFiler6 = "".
cFiler1x = "".
cFiler2x = "".
cFiler3x = "".
cFiler4x = "".
cFiler5x = "".
cFiler6x = "".

FOR EACH ttZonas BREAK BY tcodzona BY tcodalm :
    cColumn = TRIM(STRING(iRow)).
    /* Disponible */
    iColumn = ttZonas.tposdispo.
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    cFiler1 = IF(cFiler1 = "") THEN cRange ELSE cFiler1.
    cFiler1x = cRange.
    /* Maximo */
    iColumn = ttZonas.tposmax.
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    cFiler2 = IF(cFiler2 = "") THEN cRange ELSE cFiler2.
    cFiler2x = cRange.
    /* Seguridad */
    iColumn = ttZonas.tposseg.    
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    cFiler3 = IF(cFiler3 = "") THEN cRange ELSE cFiler3.
    cFiler3x = cRange.
    /* Transito por Transferencias */
    iColumn = ttZonas.tpostrans.
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    cFiler4 = IF(cFiler4 = "") THEN cRange ELSE cFiler4.
    cFiler4x = cRange.
    /* Transito por Compras */
    iColumn = ttZonas.tposcompr.
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    cFiler5 = IF(cFiler5 = "") THEN cRange ELSE cFiler5.
    cFiler5x = cRange.
    /* Detallado */
    iColumn = ttZonas.tposdeta.
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    cFiler6 = IF(cFiler6 = "") THEN cRange ELSE cFiler6.
    cFiler6x = cRange.
END.
cRange = cFiler1 + ":" + cFiler1x.        
chWorkSheet:Range(cRange):SELECT.
chWorkSheet:Range(cRange):Merge.
chWorkSheet:Range(cRange):VALUE = "D I S P O N I B L E".
chWorkSheet:Range(cRange):HorizontalAlignment = -4108.
chWorkSheet:Range(cRange):Interior:ColorIndex = 20.
/**/
cRange = cFiler2 + ":" + cFiler2x.        
chWorkSheet:Range(cRange):SELECT.
chWorkSheet:Range(cRange):Merge.
chWorkSheet:Range(cRange):VALUE = " M A X I M O".
chWorkSheet:Range(cRange):HorizontalAlignment = -4108.
chWorkSheet:Range(cRange):Interior:ColorIndex = 19.
/**/
cRange = cFiler3 + ":" + cFiler3x.        
chWorkSheet:Range(cRange):SELECT.
chWorkSheet:Range(cRange):Merge.
chWorkSheet:Range(cRange):VALUE = "S E G U R I D A D".
chWorkSheet:Range(cRange):HorizontalAlignment = -4108.
chWorkSheet:Range(cRange):Interior:ColorIndex = 18.
/**/
cRange = cFiler4 + ":" + cFiler4x.        
chWorkSheet:Range(cRange):SELECT.
chWorkSheet:Range(cRange):Merge.
chWorkSheet:Range(cRange):VALUE = "T R A N S I T O   P O R   T R A N S F E R E N C I A S".
chWorkSheet:Range(cRange):HorizontalAlignment = -4108.
chWorkSheet:Range(cRange):Interior:ColorIndex = 17.
/**/
cRange = cFiler5 + ":" + cFiler5x.        
chWorkSheet:Range(cRange):SELECT.
chWorkSheet:Range(cRange):Merge.
chWorkSheet:Range(cRange):VALUE = "T R A N S I T O   P O R   C O M P R A S".
chWorkSheet:Range(cRange):HorizontalAlignment = -4108.
chWorkSheet:Range(cRange):Interior:ColorIndex = 16.
/**/
cRange = cFiler6 + ":" + cFiler6x.        
chWorkSheet:Range(cRange):SELECT.
chWorkSheet:Range(cRange):Merge.
chWorkSheet:Range(cRange):VALUE = "D E T A L L A D O".
chWorkSheet:Range(cRange):HorizontalAlignment = -4108.
chWorkSheet:Range(cRange):Interior:ColorIndex = 15.

/* GRUPOS */
iRow = iRow + 1.
FOR EACH ttZonas BREAK BY tcodzona :
    IF FIRST-OF(tcodzona) THEN DO:
        /**/
        lColorIndex = lColorIndex + 1.
        cFiler1 = "".
        cFiler2 = "".
        cFiler3 = "".
        cFiler4 = "".
        cFiler5 = "".
        cFiler6 = "".
        cFiler1x = "".
        cFiler2x = "".
        cFiler3x = "".
        cFiler4x = "".
        cFiler5x = "".
        cFiler6x = "".
    END.
    cColumn = TRIM(STRING(iRow)).
    /* Disponible */
    iColumn = fPosCelda(ttZonas.tcodzona, ttZonas.tcodalm,1).    
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    cFiler1 = IF(cFiler1 = "") THEN cRange ELSE cFiler1.
    cFiler1x = cRange.
    /* Maximo */
    iColumn = fPosCelda(ttZonas.tcodzona, ttZonas.tcodalm,2).    
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    cFiler2 = IF(cFiler2 = "") THEN cRange ELSE cFiler2.
    cFiler2x = cRange.
    /* Seguridad */
    iColumn = fPosCelda(ttZonas.tcodzona, ttZonas.tcodalm,3).    
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    cFiler3 = IF(cFiler3 = "") THEN cRange ELSE cFiler3.
    cFiler3x = cRange.
    /* Transito por transferencias */
    iColumn = fPosCelda(ttZonas.tcodzona, ttZonas.tcodalm,4).
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    cFiler4 = IF(cFiler4 = "") THEN cRange ELSE cFiler4.
    cFiler4x = cRange.
    /* Transito por Compras */
    iColumn = fPosCelda(ttZonas.tcodzona, ttZonas.tcodalm,5).
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    cFiler5 = IF(cFiler5 = "") THEN cRange ELSE cFiler5.
    cFiler5x = cRange.
    /* Detallado */
    iColumn = fPosCelda(ttZonas.tcodzona, ttZonas.tcodalm,6).
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    cFiler6 = IF(cFiler6 = "") THEN cRange ELSE cFiler6.
    cFiler6x = cRange.

    IF LAST-OF(tcodzona) THEN DO:
        cRange = cFiler1 + ":" + cFiler1x.        
        chWorkSheet:Range(cRange):SELECT.
        chWorkSheet:Range(cRange):Merge.
        chWorkSheet:Range(cRange):VALUE = tcodzona.
        chWorkSheet:Range(cRange):HorizontalAlignment = -4108.
        /**/
        cRange = cFiler2 + ":" + cFiler2x.        
        chWorkSheet:Range(cRange):SELECT.
        chWorkSheet:Range(cRange):Merge.
        chWorkSheet:Range(cRange):VALUE = tcodzona.
        chWorkSheet:Range(cRange):HorizontalAlignment = -4108.
        /**/
        cRange = cFiler3 + ":" + cFiler3x.        
        chWorkSheet:Range(cRange):SELECT.
        chWorkSheet:Range(cRange):Merge.
        chWorkSheet:Range(cRange):VALUE = tcodzona.
        chWorkSheet:Range(cRange):HorizontalAlignment = -4108.
        /**/
        cRange = cFiler4 + ":" + cFiler4x.        
        chWorkSheet:Range(cRange):SELECT.
        chWorkSheet:Range(cRange):Merge.
        chWorkSheet:Range(cRange):VALUE = tcodzona.
        chWorkSheet:Range(cRange):HorizontalAlignment = -4108.
        /**/
        cRange = cFiler5 + ":" + cFiler5x.        
        chWorkSheet:Range(cRange):SELECT.
        chWorkSheet:Range(cRange):Merge.
        chWorkSheet:Range(cRange):VALUE = tcodzona.
        chWorkSheet:Range(cRange):HorizontalAlignment = -4108.
        /**/
        cRange = cFiler6 + ":" + cFiler6x.        
        chWorkSheet:Range(cRange):SELECT.
        chWorkSheet:Range(cRange):Merge.
        chWorkSheet:Range(cRange):VALUE = tcodzona.
        chWorkSheet:Range(cRange):HorizontalAlignment = -4108.
    END.
END.

iRow = iRow + 1.
cColumn = TRIM(STRING(iRow)).

cRange = "A" + cColumn.
chWorkSheet:Range(cRange):VALUE = "CodMat".
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Producto".
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Linea".
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):VALUE = "SubLinea".
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Marca".
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Unidad".
cRange = "G" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Proveedor".
cRange = "H" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Volumen cm3".
cRange = "I" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Peso KG".
cRange = "J" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Costo Reposicion".
cRange = "K" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Clasf.General".
cRange = "L" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Clasf.Utilex".
cRange = "M" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Clasf.Mayorista".
cRange = "N" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Emp.Repo".
cRange = "O" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Emp.Master".
cRange = "P" + cColumn.
chWorkSheet:Range(cRange):VALUE = "Emp.Inner".

/* Almamcens */
lColorIndex = 32.
FOR EACH ttZonas BREAK BY tcodzona :
    IF FIRST-OF(tcodzona) THEN DO:
        /**/
        lColorIndex = lColorIndex + 1.
    END.
    cColumn = TRIM(STRING(iRow)).
    /* Disponible */
    iColumn = fPosCelda(ttZonas.tcodzona, ttZonas.tcodalm,1).    
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    chWorkSheet:Range(cRange):VALUE = "'" + ttZonas.tcodalm.
    chWorkSheet:Range(cRange):Interior:ColorIndex = lColorIndex.
    /* Maximo */
    iColumn = fPosCelda(ttZonas.tcodzona, ttZonas.tcodalm,2).    
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    chWorkSheet:Range(cRange):VALUE = "'" + ttZonas.tcodalm.
    chWorkSheet:Range(cRange):Interior:ColorIndex = lColorIndex.
    /* Seguridad */
    iColumn = fPosCelda(ttZonas.tcodzona, ttZonas.tcodalm,3).    
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    chWorkSheet:Range(cRange):VALUE = "'" + ttZonas.tcodalm.
    chWorkSheet:Range(cRange):Interior:ColorIndex = lColorIndex.
    /* Transito transferencias */
    iColumn = fPosCelda(ttZonas.tcodzona, ttZonas.tcodalm,4).
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    chWorkSheet:Range(cRange):VALUE = "'" + ttZonas.tcodalm.
    chWorkSheet:Range(cRange):Interior:ColorIndex = lColorIndex.
    /* Transito Compras*/
    iColumn = fPosCelda(ttZonas.tcodzona, ttZonas.tcodalm,5).
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    chWorkSheet:Range(cRange):VALUE = "'" + ttZonas.tcodalm.
    chWorkSheet:Range(cRange):Interior:ColorIndex = lColorIndex.
    /* Detallado */
    iColumn = fPosCelda(ttZonas.tcodzona, ttZonas.tcodalm,6).
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    chWorkSheet:Range(cRange):VALUE = "'" + ttZonas.tcodalm.
    chWorkSheet:Range(cRange):Interior:ColorIndex = lColorIndex.

    IF LAST-OF(tcodzona) THEN DO:

    END.
END.

/* Total x Zonas */
lSecx = lSec.
FOR EACH ttZonas BREAK BY tcodzona :
    cColumn = TRIM(STRING(iRow)).
    IF FIRST-OF(tcodzona) THEN DO:
        lSecx = lSecx + 1.
        cRange = ENTRY(lSecx,cColList,',') + cColumn.
        chWorkSheet:Range(cRange):VALUE = "TOTAL GRUPO " + ttZonas.tcodzona.        
    END.
END.

lTotZona = 0.
/* Articulo, Zona, Almacen */
FOR EACH bt-gener BREAK BY libre_c03 BY codigo BY libre_c01 :
    IF FIRST-OF(bt-gener.libre_c03) THEN DO:
        lSecx = lSec.
        iRow = iRow + 1.
        lOtroArt = YES.
        cColumn = TRIM(STRING(iRow)).
        cRange = "A" + cColumn.
        chWorkSheet:Range(cRange):VALUE = "'" + bt-gener.libre_c03.
        /* - ---  */
        FIND FIRST almmmatg WHERE almmmatg.codcia = s-codcia AND 
                                    almmmatg.codmat = bt-gener.libre_c03
                                    NO-LOCK NO-ERROR.
        IF AVAILABLE almmmatg THEN DO:
            FIND FIRST almtfam OF almmmatg NO-LOCK.
            FIND FIRST almsfam OF almmmatg NO-LOCK.
            cRange = "B" + cColumn.
            chWorkSheet:Range(cRange):VALUE = "'" + almmmatg.desmat.
            cRange = "C" + cColumn.
            chWorkSheet:Range(cRange):VALUE = "'" + almmmatg.codfam + " " + almtfam.desfam.
            cRange = "D" + cColumn.
            chWorkSheet:Range(cRange):VALUE = "'" + almmmatg.subfam + " " + almsfam.dessub.
            cRange = "E" + cColumn.
            chWorkSheet:Range(cRange):VALUE = "'" + almmmatg.desmar.
            cRange = "F" + cColumn.
            chWorkSheet:Range(cRange):VALUE = "'" + almmmatg.undstk.
            /**/
            FIND FIRST gn-prov WHERE  gn-prov.codcia = 0 AND 
                                        gn-prov.codpro = almmmatg.codpr1 NO-LOCK NO-ERROR.
            IF AVAILABLE gn-prov THEN DO:
                cRange = "G" + cColumn.
                chWorkSheet:Range(cRange):VALUE = "'" + TRIM(gn-prov.nompro).
            END.
            cRange = "H" + cColumn.
            chWorkSheet:Range(cRange):VALUE = almmmatg.libre_d02.
            cRange = "I" + cColumn.
            chWorkSheet:Range(cRange):VALUE = almmmatg.pesmat.
            cRange = "J" + cColumn.
            chWorkSheet:Range(cRange):VALUE = almmmatg.ctolis.

            cRange = "O" + cColumn.
            chWorkSheet:Range(cRange):VALUE = almmmatg.canemp.
            cRange = "P" + cColumn.
            chWorkSheet:Range(cRange):VALUE = almmmatg.stkrep.

        END.

        /* Ranking */
        FIND FIRST factabla WHERE factabla.codcia = s-codcia AND 
                                    factabla.tabla = 'RANKVTA' AND 
                                    factabla.codigo = bt-gener.libre_c03 NO-LOCK NO-ERROR.
        IF AVAILABLE factabla THEN DO:
            cRange = "K" + cColumn.
            chWorkSheet:Range(cRange):VALUE = "'" + factabla.campo-c[1].
            cRange = "L" + cColumn.
            chWorkSheet:Range(cRange):VALUE = "'" + factabla.campo-c[2].
            cRange = "M" + cColumn.
            chWorkSheet:Range(cRange):VALUE = "'" + factabla.campo-c[3].
        END.

    END.    
    IF FIRST-OF(bt-gener.codigo) THEN DO:
        lTotZona = 0.
    END.

    FIND FIRST b-almmmate WHERE b-almmmate.codcia = s-codcia AND 
                                b-almmmate.codalm = bt-gener.libre_c01 AND
                                b-almmmate.codmat = bt-gener.libre_c03 NO-LOCK NO-ERROR.

    /* Emapque Reposicion */
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):VALUE = IF (AVAILABLE b-almmmate) THEN b-almmmate.stkmax ELSE 0.

    /* Disponible */
    iColumn = fPosCelda(bt-gener.codigo,bt-gener.libre_c01,1).    
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    chWorkSheet:Range(cRange):VALUE = bt-gener.libre_d01.
    /* Maximo */
    iColumn = fPosCelda(bt-gener.codigo,bt-gener.libre_c01,2).    
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    chWorkSheet:Range(cRange):VALUE = if( AVAILABLE b-almmmate) THEN  b-almmmate.StockMax ELSE 0.
    /* Seguridad */
    iColumn = fPosCelda(bt-gener.codigo,bt-gener.libre_c01,3).
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    chWorkSheet:Range(cRange):VALUE = if(AVAILABLE b-almmmate) THEN b-almmmate.StockSeg ELSE 0.
    /* Transito Transito */
    iColumn = fPosCelda(bt-gener.codigo,bt-gener.libre_c01,4).
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    chWorkSheet:Range(cRange):VALUE = bt-gener.libre_d02.
    /* Transito Compras */
    iColumn = fPosCelda(bt-gener.codigo,bt-gener.libre_c01,5).
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    chWorkSheet:Range(cRange):VALUE = bt-gener.libre_d04.

    lTotal = if( AVAILABLE b-almmmate) THEN b-almmmate.StockMax ELSE 0.
    lTotal = lTotal * -1.
    lTotal = lTotal - (if(AVAILABLE b-almmmate) THEN b-almmmate.StockSeg ELSE 0).
    lTotal = lTotal + bt-gener.libre_d01 + bt-gener.libre_d02 + bt-gener.libre_d04.

    /* detallado */
    iColumn = fPosCelda(bt-gener.codigo,bt-gener.libre_c01,6).
    cRange = ENTRY(iColumn,cColList,',') + cColumn.
    chWorkSheet:Range(cRange):VALUE = lTotal.

    lTotZona = lTotZona + lTotal.

    IF LAST-OF(bt-gener.codigo) THEN DO:
        lSecx = lSecx + 1.
        cRange = ENTRY(lSecx,cColList,',') + cColumn.
        chWorkSheet:Range(cRange):VALUE = lTotZona.
    END.

    /**/
    lOtroArt = NO.
END.

SESSION:SET-WAIT-STATE('').

chExcelApplication:Visible = YES.

{lib\excel-close-file.i} 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-Campana B-table-Win 
PROCEDURE Excel-Campana :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR lNuevoFile AS LOG INIT YES NO-UNDO.
    DEF VAR lFileXls AS CHAR NO-UNDO.

    DEF VAR t-Row    AS INT NO-UNDO.
    DEF VAR t-Column AS INT NO-UNDO.

    {lib/excel-open-file.i}

    /* set the column names for the Worksheet */
    ASSIGN
        chWorkSheet:Range("A1"):Value = "ALMACEN"
        chWorkSheet:Range("B1"):Value = "ARTICULO"
        chWorkSheet:Range("C1"):Value = "CAMAPA�A"
        chWorkSheet:Range("D1"):Value = "NO CAMPA�A"
        chWorkSheet:Range("E1"):Value = "EMPAQUE"
        chWorkSheet:Range("F1"):Value = "DESCRIPCION ARTICULO"
        chWorkSheet:Range("G1"):Value = "MARCA"
        chWorkSheet:Range("H1"):Value = "UNIDAD BASICA"
        .
    ASSIGN
        chWorkSheet:COLUMNS("A"):NumberFormat = "@"
        chWorkSheet:COLUMNS("B"):NumberFormat = "@".
    ASSIGN
        t-Row = 1.

    GET FIRST {&BROWSE-NAME}.
    REPEAT WHILE AVAILABLE Almmmatg:
        FOR EACH Almmmate OF Almmmatg NO-LOCK,
            FIRST Almacen OF Almmmate NO-LOCK,
            EACH TabGener WHERE TabGener.CodCia = Almmmate.CodCia
            AND TabGener.Libre_c01 = Almmmate.CodAlm
            AND TabGener.Clave = "ZG" NO-LOCK,
            FIRST almtabla WHERE almtabla.Tabla = TabGener.Clave
            AND almtabla.Codigo = TabGener.Codigo NO-LOCK:
            ASSIGN
                t-Column = 0
                t-Row    = t-Row + 1.
            ASSIGN
                t-Column = t-Column + 1
                chWorkSheet:Cells(t-Row, t-Column):VALUE = Almmmate.codalm.
            ASSIGN
                t-Column = t-Column + 1
                chWorkSheet:Cells(t-Row, t-Column):VALUE = Almmmatg.codmat.
            ASSIGN
                t-Column = t-Column + 1
                chWorkSheet:Cells(t-Row, t-Column):VALUE = Almmmate.VCtMn1.
            ASSIGN
                t-Column = t-Column + 1
                chWorkSheet:Cells(t-Row, t-Column):VALUE = Almmmate.VCtMn2.
            ASSIGN
                t-Column = t-Column + 1
                chWorkSheet:Cells(t-Row, t-Column):VALUE = Almmmatg.canemp.
            ASSIGN
                t-Column = t-Column + 1
                chWorkSheet:Cells(t-Row, t-Column):VALUE = Almmmatg.desmat.
            ASSIGN
                t-Column = t-Column + 1
                chWorkSheet:Cells(t-Row, t-Column):VALUE = Almmmatg.desmar.
            ASSIGN
                t-Column = t-Column + 1
                chWorkSheet:Cells(t-Row, t-Column):VALUE = Almmmatg.undbas.
        END.
        GET NEXT {&BROWSE-NAME}.
    END.

    {lib/excel-close-file.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Informacion B-table-Win 
PROCEDURE Graba-Informacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* ************************************* */
/* ***** RHC 30/11/2015 NO REPLICA ***** */
DISABLE TRIGGERS FOR LOAD OF Almmmate.
/* ************************************* */
/* ************************************* */

PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN ERROR ON STOP UNDO, RETURN ERROR:
    EMPTY TEMP-TABLE T-MATE.
    {alm/i-actualiza-minmaxv41.i}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  FOR EACH Almtfami NO-LOCK WHERE Almtfami.codcia = s-codcia WITH FRAME {&FRAME-NAME}:
      COMBO-BOX-CodFam:ADD-LAST( Almtfami.codfam + ' - ' + REPLACE(Almtfami.desfam, ',', ' ')
                                 , Almtfami.codfam).
  END.
  FIND Almcfggn WHERE Almcfggn.codcia = s-codcia NO-LOCK NO-ERROR.
  CASE TRUE:
      WHEN NOT AVAILABLE Almcfggn OR AlmCfgGn.Temporada = '' THEN FILL-IN-Temporada = "NO DEFINIDA".
      WHEN Almcfggn.Temporada = "C" THEN FILL-IN-Temporada = "CAMPA�A".
      WHEN Almcfggn.Temporada = "NC" THEN FILL-IN-Temporada = "NO CAMPA�A".
  END CASE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recargar-Maximos B-table-Win 
PROCEDURE Recargar-Maximos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FIND Almcfggn WHERE Almcfggn.codcia = s-codcia NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almcfggn OR LOOKUP(AlmCfgGn.Temporada, 'C,NC') = 0 THEN DO:
    MESSAGE 'NO se ha definido la TEMPORADA' SKIP 'Proceso abortado'
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

pTipo = AlmCfgGn.Temporada.     /* OJO */

MESSAGE 'Se va aproceder a actualizar el M�ximo + Seguridad con los datos de'
    (IF pTipo = "C" THEN "CAMPA�A" ELSE "NO CAMPA�A") SKIP
    'Continuamos con el proceso?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE rpta AS LOG.
IF rpta = NO THEN RETURN.

SESSION:SET-WAIT-STATE('GENERAL').
GET FIRST {&BROWSE-NAME}.
DO WHILE AVAILABLE Almmmatg TRANSACTION ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
    /* Verificamos si al menos un registro ha sido actualizado */
    FIND FIRST B-MATE WHERE B-MATE.codcia = Almmmatg.codcia
        AND B-MATE.codmat = Almmmatg.codmat
        NO-LOCK NO-ERROR.
    IF AVAILABLE B-MATE THEN DO:
        FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
            "*** Procesando C�digo: " + Almmmatg.codmat.

        RUN Graba-Informacion NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE 'NO se pudo actualizar el c�digo' Almmmatg.codmat SKIP
                'Se continuar� con el siguiente c�digo'
                VIEW-AS ALERT-BOX WARNING.
        END.
    END.
    GET NEXT {&BROWSE-NAME}.
END.
IF AVAILABLE(Almmmate) THEN RELEASE Almmmate.

FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".
SESSION:SET-WAIT-STATE('').

RUN dispatch IN THIS-PROCEDURE ('open-query':U).
MESSAGE 'Proceso Terminado' VIEW-AS ALERT-BOX INFORMATION.
APPLY 'CHOOSE':U TO BUTTON-10.

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
  {src/adm/template/snd-list.i "T-MATG"}
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fComprasTransito B-table-Win 
FUNCTION fComprasTransito RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VAR lRetVal AS DEC.

FOR EACH OOComPend WHERE OOComPend.CodAlm = Almmmate.codalm AND 
    OOComPend.CodMat = Almmmate.codmat NO-LOCK:
    lRetVal = lRetval + (OOComPend.CanPed - OOComPend.CanAte).
END.

RETURN lRetval.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fPosCelda B-table-Win 
FUNCTION fPosCelda RETURNS INTEGER
  ( INPUT cCodZona AS CHAR, INPUT cAlm AS CHAR, INPUT nBloque AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VAR lRetVal AS INT INIT -99.

  FIND FIRST ttZonas WHERE ttZonas.tcodzona = cCodZona AND
                            ttZonas.tcodalm = cAlm NO-LOCK NO-ERROR.
  IF AVAILABLE ttZonas THEN DO:
      CASE nBloque:
          WHEN 1 THEN lRetVal = ttZonas.tposdispo.
          WHEN 2 THEN lRetVal = ttZonas.tposmax.
          WHEN 3 THEN lRetVal = ttZonas.tposseg.
          WHEN 4 THEN lRetVal = ttZonas.tpostrans.
          WHEN 5 THEN lRetVal = ttZonas.tposcompr.
          WHEN 6 THEN lRetVal = ttZonas.tposdeta.
      END CASE.
  END.

  RETURN lRetVal.   /* Function return value. */

END FUNCTION.

/*
DEFINE TEMP-TABLE ttZonas
    FIELD   tcodzona    AS  CHAR    FORMAT 'x(25)'
    FIELD   tcodalm     AS  CHAR    FORMAT 'x(5)'
    FIELD   tmainalm    AS  CHAR    FORMAT 'x(1)'
    FIELD   tposdispo   AS  INT     FORMAT '>>,>>>' INIT 0
    FIELD   tposmax     AS  INT     FORMAT '>>,>>>' INIT 0
    FIELD   tposseg     AS  INT     FORMAT '>>,>>>' INIT 0
    FIELD   tpostrans   AS  INT     FORMAT '>>,>>>' INIT 0
    FIELD   tposdeta    AS  INT     FORMAT '>>,>>>' INIT 0
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fStockComprometido B-table-Win 
FUNCTION fStockComprometido RETURNS DECIMAL
  ( INPUT pCodMat AS CHAR, INPUT pCodAlm AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR pStkComprometido AS DEC.

  RUN gn/stock-comprometido-v2.p (pCodMat, pCodAlm, NO, OUTPUT pStkComprometido).

  RETURN pStkComprometido.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fStockTransito B-table-Win 
FUNCTION fStockTransito RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    /* En Tr�nsito */
    DEF VAR x-Total AS DEC NO-UNDO.
    RUN alm\p-articulo-en-transito (
        Almmmate.CodCia,
        Almmmate.CodAlm,
        Almmmate.CodMat,
        INPUT-OUTPUT TABLE tmp-tabla,
        OUTPUT x-Total).

    RETURN x-Total.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

