&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE w-report-film NO-UNDO LIKE w-report.
DEFINE TEMP-TABLE w-report-od NO-UNDO LIKE w-report.



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

DEFINE SHARED VAR s-coddiv AS CHAR.
DEFINE SHARED VAR pv-codcia AS INTE.
DEFINE SHARED VAR cl-codcia AS INTE.
DEFINE SHARED VAR s-codcia AS INT.

DEFINE BUFFER x-faccpedi FOR faccpedi.

DEFINE VAR x-vtatabla AS CHAR INIT "CONFIG-VTAS".
DEFINE VAR x-llave_c1 AS CHAR INIT "LOGISTICA".
DEFINE VAR x-llave_c2 AS CHAR INIT "PICKING".
DEFINE VAR x-llave_c3 AS CHAR INIT "METROSXROLLO".

DEFINE VAR x-Directorio AS CHAR.

define stream REPORT-txt.

DEFINE VAR x-total-ordenes AS INT.
DEFINE VAR x-total-metros AS DEC.
DEFINE VAR x-total-rollos AS DEC.
DEFINE VAR x-total-bultos AS DEC.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES w-report-film w-report-od

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 w-report-film.Campo-C[1] ~
w-report-film.Campo-D[1] w-report-film.Campo-C[2] w-report-film.Campo-F[1] ~
w-report-film.Campo-F[2] w-report-film.Campo-I[1] w-report-film.Campo-F[3] ~
w-report-film.Campo-C[3] w-report-film.Campo-C[4] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH w-report-film NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH w-report-film NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 w-report-film
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 w-report-film


/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table w-report-od.Campo-D[1] ~
w-report-od.Campo-C[5] w-report-od.Campo-C[1] w-report-od.Campo-C[2] ~
w-report-od.Campo-F[1] w-report-od.Campo-F[2] w-report-od.Campo-F[3] ~
w-report-od.Campo-C[3] w-report-od.Campo-C[18] w-report-od.Campo-C[4] ~
w-report-od.Campo-I[1] w-report-od.Campo-C[10] w-report-od.Campo-C[11] ~
w-report-od.Campo-C[12] w-report-od.Campo-C[13] w-report-od.Campo-C[14] ~
w-report-od.Campo-C[15] w-report-od.Campo-C[16] w-report-od.Campo-C[17] ~
w-report-od.Campo-C[19] w-report-od.Campo-C[20] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH w-report-od WHERE ~{&KEY-PHRASE} NO-LOCK ~
    BY w-report-od.Campo-D[1] DESCENDING
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH w-report-od WHERE ~{&KEY-PHRASE} NO-LOCK ~
    BY w-report-od.Campo-D[1] DESCENDING.
&Scoped-define TABLES-IN-QUERY-br_table w-report-od
&Scoped-define FIRST-TABLE-IN-QUERY-br_table w-report-od


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-2 FILL-IN-nroorden RADIO-SET-coddoc ~
BUTTON-txt FILL-IN-desde FILL-IN-hasta FILL-IN-metros BUTTON-2 br_table ~
RECT-1 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-nroorden RADIO-SET-coddoc ~
FILL-IN-divdes FILL-IN-tot-od FILL-IN-desde FILL-IN-hasta FILL-IN-metros ~
FILL-IN-tot-metros FILL-IN-tot-rollos FILL-IN-tot-bultos 

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
DEFINE BUTTON BUTTON-2 
     LABEL "Procesar" 
     SIZE 14 BY .96.

DEFINE BUTTON BUTTON-txt 
     LABEL "Enviar a txt" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE FILL-IN-desde AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-divdes AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 9.86 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-hasta AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-metros AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Metros x rollo" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-nroorden AS CHARACTER FORMAT "X(10)":U 
     LABEL "Orden" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-tot-bultos AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Total bultos" 
     VIEW-AS FILL-IN 
     SIZE 9.43 BY .81
     BGCOLOR 15 FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-tot-metros AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Total metros" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 15 FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-tot-od AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Cant. Ordenes" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .81
     BGCOLOR 15 FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-tot-rollos AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Total rollos" 
     VIEW-AS FILL-IN 
     SIZE 9.43 BY .81
     BGCOLOR 15 FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE RADIO-SET-coddoc AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "O/D", "O/D",
"OTR", "OTR"
     SIZE 13 BY .77
     FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82.72 BY 1.2.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      w-report-film SCROLLING.

DEFINE QUERY br_table FOR 
      w-report-od SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 B-table-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      w-report-film.Campo-C[1] COLUMN-LABEL "Film" FORMAT "X(80)":U
            WIDTH 42.43
      w-report-film.Campo-D[1] COLUMN-LABEL "Consumo!Fecha" FORMAT "99/99/9999":U
            WIDTH 11.43
      w-report-film.Campo-C[2] COLUMN-LABEL "Consumo!Hora" FORMAT "X(8)":U
      w-report-film.Campo-F[1] COLUMN-LABEL "Por cada!unidad(es)" FORMAT "->>>,>>>,>>9.9999":U
            WIDTH 8.72
      w-report-film.Campo-F[2] COLUMN-LABEL "Film Sugerido!(metros)" FORMAT "->>>,>>>,>>9.9999":U
            WIDTH 10.43
      w-report-film.Campo-I[1] COLUMN-LABEL "Bultos" FORMAT "->>>,>>>,>>9":U
            WIDTH 7.43
      w-report-film.Campo-F[3] COLUMN-LABEL "Film!Total usado" FORMAT "->>>,>>>,>>9.9999":U
            WIDTH 11.43
      w-report-film.Campo-C[3] COLUMN-LABEL "Cod!Ref" FORMAT "X(5)":U
      w-report-film.Campo-C[4] COLUMN-LABEL "Nro!Referencia" FORMAT "X(18)":U
            WIDTH 12.14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 125 BY 7.54
         FONT 4
         TITLE "Film consumido" FIT-LAST-COLUMN.

DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      w-report-od.Campo-D[1] COLUMN-LABEL "Cierre de!Orden" FORMAT "99/99/9999":U
      w-report-od.Campo-C[5] COLUMN-LABEL "Div!despacho" FORMAT "X(8)":U
            WIDTH 7.14
      w-report-od.Campo-C[1] COLUMN-LABEL "Cod!Orden" FORMAT "X(8)":U
            WIDTH 5.29
      w-report-od.Campo-C[2] COLUMN-LABEL "Nro.!Orden" FORMAT "X(10)":U
            WIDTH 10.29
      w-report-od.Campo-F[1] COLUMN-LABEL "Metraje!Utilizado" FORMAT "->>>,>>>,>>9.99":U
      w-report-od.Campo-F[2] COLUMN-LABEL "Cantidad!Rollos" FORMAT "->>,>>9.99":U
            WIDTH 10
      w-report-od.Campo-F[3] COLUMN-LABEL "Peso kg" FORMAT ">>>,>>9.99":U
      w-report-od.Campo-C[3] COLUMN-LABEL "Procs" FORMAT "X(5)":U
      w-report-od.Campo-C[18] COLUMN-LABEL "Cross!Dock" FORMAT "x(5)":U
      w-report-od.Campo-C[4] COLUMN-LABEL "Cliente" FORMAT "X(80)":U
            WIDTH 35.72
      w-report-od.Campo-I[1] COLUMN-LABEL "Bultos" FORMAT "->>>,>>>,>>9":U
      w-report-od.Campo-C[10] COLUMN-LABEL "!Direcci�n" FORMAT "X(60)":U
      w-report-od.Campo-C[11] COLUMN-LABEL "DESTINO INICIAL!Departamento" FORMAT "X(20)":U
      w-report-od.Campo-C[12] COLUMN-LABEL "!Provincia" FORMAT "X(20)":U
      w-report-od.Campo-C[13] COLUMN-LABEL "!Distrito" FORMAT "X(20)":U
      w-report-od.Campo-C[14] COLUMN-LABEL "!DIrecci�n" FORMAT "X(60)":U
      w-report-od.Campo-C[15] COLUMN-LABEL "DESTINO FINAL!Departamento" FORMAT "X(20)":U
      w-report-od.Campo-C[16] COLUMN-LABEL "!Provincia" FORMAT "X(20)":U
      w-report-od.Campo-C[17] COLUMN-LABEL "!Distrito" FORMAT "X(20)":U
      w-report-od.Campo-C[19] COLUMN-LABEL "Vendedor" FORMAT "X(30)":U
      w-report-od.Campo-C[20] COLUMN-LABEL "Div. de Venta" FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 127.57 BY 9.88
         FONT 4 ROW-HEIGHT-CHARS .54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-2 AT ROW 13.38 COL 1.86 WIDGET-ID 200
     FILL-IN-nroorden AT ROW 2.23 COL 48.86 COLON-ALIGNED WIDGET-ID 34
     RADIO-SET-coddoc AT ROW 2.27 COL 32 NO-LABEL WIDGET-ID 36
     BUTTON-txt AT ROW 21.12 COL 103 WIDGET-ID 24
     FILL-IN-divdes AT ROW 2.54 COL 26.72 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     FILL-IN-tot-od AT ROW 21.38 COL 17.57 COLON-ALIGNED WIDGET-ID 18
     FILL-IN-desde AT ROW 1.27 COL 15 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-hasta AT ROW 2.08 COL 15 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-metros AT ROW 1.27 COL 39 COLON-ALIGNED WIDGET-ID 6
     BUTTON-2 AT ROW 1.19 COL 68.86 WIDGET-ID 8
     br_table AT ROW 3.5 COL 1.43
     FILL-IN-tot-metros AT ROW 21.38 COL 35.43 COLON-ALIGNED WIDGET-ID 20
     FILL-IN-tot-rollos AT ROW 21.38 COL 56.14 COLON-ALIGNED WIDGET-ID 22
     FILL-IN-tot-bultos AT ROW 21.38 COL 75.29 COLON-ALIGNED WIDGET-ID 26
     "Fecha de" VIEW-AS TEXT
          SIZE 7.29 BY .5 AT ROW 1.31 COL 3.57 WIDGET-ID 10
     "cierre de" VIEW-AS TEXT
          SIZE 7.29 BY .5 AT ROW 1.81 COL 3.57 WIDGET-ID 12
     "Orden" VIEW-AS TEXT
          SIZE 7.29 BY .5 AT ROW 2.35 COL 3.57 WIDGET-ID 14
     "PXR: Picking por Rutas" VIEW-AS TEXT
          SIZE 19 BY .5 AT ROW 2.27 COL 73.86 WIDGET-ID 28
          BGCOLOR 9 FGCOLOR 15 
     "PXS: Picking por Sectores" VIEW-AS TEXT
          SIZE 19 BY .5 AT ROW 2.85 COL 73.86 WIDGET-ID 30
          BGCOLOR 9 FGCOLOR 15 
     RECT-1 AT ROW 21.19 COL 7.29 WIDGET-ID 32
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
      TABLE: w-report-film T "?" NO-UNDO INTEGRAL w-report
      TABLE: w-report-od T "?" NO-UNDO INTEGRAL w-report
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
         HEIGHT             = 21.58
         WIDTH              = 128.86.
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
/* BROWSE-TAB BROWSE-2 1 F-Main */
/* BROWSE-TAB br_table BUTTON-2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 4.

/* SETTINGS FOR FILL-IN FILL-IN-divdes IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-divdes:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-tot-bultos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-tot-metros IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-tot-od IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-tot-rollos IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "Temp-Tables.w-report-film"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.w-report-film.Campo-C[1]
"Campo-C[1]" "Film" "X(80)" "character" ? ? ? ? ? ? no ? no no "42.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.w-report-film.Campo-D[1]
"Campo-D[1]" "Consumo!Fecha" ? "date" ? ? ? ? ? ? no ? no no "11.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.w-report-film.Campo-C[2]
"Campo-C[2]" "Consumo!Hora" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.w-report-film.Campo-F[1]
"Campo-F[1]" "Por cada!unidad(es)" ? "decimal" ? ? ? ? ? ? no ? no no "8.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.w-report-film.Campo-F[2]
"Campo-F[2]" "Film Sugerido!(metros)" ? "decimal" ? ? ? ? ? ? no ? no no "10.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.w-report-film.Campo-I[1]
"Campo-I[1]" "Bultos" ? "integer" ? ? ? ? ? ? no ? no no "7.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.w-report-film.Campo-F[3]
"Campo-F[3]" "Film!Total usado" ? "decimal" ? ? ? ? ? ? no ? no no "11.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.w-report-film.Campo-C[3]
"Campo-C[3]" "Cod!Ref" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.w-report-film.Campo-C[4]
"Campo-C[4]" "Nro!Referencia" "X(18)" "character" ? ? ? ? ? ? no ? no no "12.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "Temp-Tables.w-report-od"
     _Options          = "NO-LOCK KEY-PHRASE"
     _OrdList          = "Temp-Tables.w-report-od.Campo-D[1]|no"
     _FldNameList[1]   > Temp-Tables.w-report-od.Campo-D[1]
"w-report-od.Campo-D[1]" "Cierre de!Orden" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.w-report-od.Campo-C[5]
"w-report-od.Campo-C[5]" "Div!despacho" ? "character" ? ? ? ? ? ? no ? no no "7.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.w-report-od.Campo-C[1]
"w-report-od.Campo-C[1]" "Cod!Orden" ? "character" ? ? ? ? ? ? no ? no no "5.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.w-report-od.Campo-C[2]
"w-report-od.Campo-C[2]" "Nro.!Orden" "X(10)" "character" ? ? ? ? ? ? no ? no no "10.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.w-report-od.Campo-F[1]
"w-report-od.Campo-F[1]" "Metraje!Utilizado" "->>>,>>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.w-report-od.Campo-F[2]
"w-report-od.Campo-F[2]" "Cantidad!Rollos" "->>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.w-report-od.Campo-F[3]
"w-report-od.Campo-F[3]" "Peso kg" ">>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.w-report-od.Campo-C[3]
"w-report-od.Campo-C[3]" "Procs" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.w-report-od.Campo-C[18]
"w-report-od.Campo-C[18]" "Cross!Dock" "x(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.w-report-od.Campo-C[4]
"w-report-od.Campo-C[4]" "Cliente" "X(80)" "character" ? ? ? ? ? ? no ? no no "35.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.w-report-od.Campo-I[1]
"w-report-od.Campo-I[1]" "Bultos" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.w-report-od.Campo-C[10]
"w-report-od.Campo-C[10]" "!Direcci�n" "X(60)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.w-report-od.Campo-C[11]
"w-report-od.Campo-C[11]" "DESTINO INICIAL!Departamento" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.w-report-od.Campo-C[12]
"w-report-od.Campo-C[12]" "!Provincia" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.w-report-od.Campo-C[13]
"w-report-od.Campo-C[13]" "!Distrito" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.w-report-od.Campo-C[14]
"w-report-od.Campo-C[14]" "!DIrecci�n" "X(60)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.w-report-od.Campo-C[15]
"w-report-od.Campo-C[15]" "DESTINO FINAL!Departamento" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Temp-Tables.w-report-od.Campo-C[16]
"w-report-od.Campo-C[16]" "!Provincia" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > Temp-Tables.w-report-od.Campo-C[17]
"w-report-od.Campo-C[17]" "!Distrito" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > Temp-Tables.w-report-od.Campo-C[19]
"w-report-od.Campo-C[19]" "Vendedor" "X(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > Temp-Tables.w-report-od.Campo-C[20]
"w-report-od.Campo-C[20]" "Div. de Venta" "X(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

    DEFINE VAR xCodDoc AS CHAR.
    DEFINE VAR xNroDoc AS CHAR.

    IF AVAILABLE w-report-od THEN DO:
        xCodDoc = w-report-od.campo-c[1].
        xNroDoc = w-report-od.campo-c[2].
    END.
    ELSE DO:
        xCodDoc = "***".
        xNroDoc = "*****???**".
    END.
    RUN cargar-film (xCodDoc, xNroDoc).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 B-table-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Procesar */
DO:
  ASSIGN fill-in-metros fill-in-desde fill-in-hasta fill-in-nroorden radio-set-coddoc.
  IF fill-in-metros <= 0  THEN DO:
      MESSAGE "Metros x rollo debe tener un valor mayor a cero" VIEW-AS ALERT-BOX INFORMATION.
      RETURN NO-APPLY.
  END.
  IF fill-in-desde = ? THEN DO:
      MESSAGE "Ingrese la fecha desde" VIEW-AS ALERT-BOX INFORMATION.
      RETURN NO-APPLY.
  END.
  IF fill-in-hasta = ? THEN DO:
      MESSAGE "Ingrese la fecha hasta" VIEW-AS ALERT-BOX INFORMATION.
      RETURN NO-APPLY.
  END.

  IF fill-in-desde > fill-in-hasta THEN DO:
      MESSAGE "Fecha desde debe ser menor/igual a hasta" VIEW-AS ALERT-BOX INFORMATION.
      RETURN NO-APPLY.
  END.


  RUN procesar.

  FIND FIRST vtatabla WHERE vtatabla.codcia = s-codcia AND
                            vtatabla.tabla = x-vtatabla AND
                            vtatabla.llave_c1 = x-llave_c1 AND
                            vtatabla.llave_c2 = x-llave_c2 AND
                            vtatabla.llave_c3 = x-llave_c3 EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE vtatabla THEN DO:
    CREATE vtatabla.
        ASSIGN vtatabla.codcia = s-codcia
                vtatabla.tabla = x-vtatabla
                vtatabla.llave_c1 = x-llave_c1
                vtatabla.llave_c2 = x-llave_c2
                vtatabla.llave_c3 = x-llave_c3.
  END.
  ASSIGN vtatabla.valor[1] = fill-in-metros.
  RELEASE vtatabla NO-ERROR.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-txt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-txt B-table-Win
ON CHOOSE OF BUTTON-txt IN FRAME F-Main /* Enviar a txt */
DO:

        x-Directorio = "".

        SYSTEM-DIALOG GET-DIR x-Directorio  
           RETURN-TO-START-DIR 
           TITLE 'Elija el directorio'.
        IF x-Directorio = "" THEN DO:
        RETURN NO-APPLY.
    END.
  
    RUN procesar-txt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargar-film B-table-Win 
PROCEDURE cargar-film :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pCodDoc AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER pNroDoc AS CHAR NO-UNDO.

EMPTY TEMP-TABLE w-report-film.

FOR EACH facdliqu WHERE facdliqu.codcia = 1 AND facdliqu.coddoc = pCodDoc AND
                        facdliqu.nrodoc = pNroDoc NO-LOCK:
    CREATE w-report-film.
    ASSIGN w-report-film.campo-c[1] = facdliqu.undvta
            w-report-film.campo-d[1] = facdliqu.fchdoc
            w-report-film.campo-c[2] = facdliqu.flg_factor
            w-report-film.campo-f[1] = facdliqu.prevta[1]
            w-report-film.campo-f[2] = facdliqu.prevta[2]            
            w-report-film.campo-i[1] = facdliqu.prevta[3]
            w-report-film.campo-f[3] = facdliqu.implin
            w-report-film.campo-c[3] = facdliqu.codref
            w-report-film.campo-c[4] = facdliqu.nroref.
            
END.

{&open-query-browse-2}

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

  FILL-IN-desde:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY - 5,"99/99/9999").
  FILL-IN-hasta:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY,"99/99/9999").
  FILL-IN-divdes:SCREEN-VALUE IN FRAME {&FRAME-NAME} = s-coddiv.
  FILL-IN-metros:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0.00".

  FIND FIRST vtatabla WHERE vtatabla.codcia = s-codcia AND
                            vtatabla.tabla = x-vtatabla AND
                            vtatabla.llave_c1 = x-llave_c1 AND
                            vtatabla.llave_c2 = x-llave_c2 AND
                            vtatabla.llave_c3 = x-llave_c3 NO-LOCK NO-ERROR.
  IF AVAILABLE vtatabla THEN DO:
        FILL-IN-metros:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(vtatabla.valor[1],">>,>>9.99").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesar B-table-Win 
PROCEDURE procesar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE w-report-od.
EMPTY TEMP-TABLE w-report-film.

SESSION:SET-WAIT-STATE("GENERAL").

RUN procesar-picking.

{&open-query-br_table}
{&open-query-browse-2}
/*
FIND FIRST w-report-od NO-LOCK NO-ERROR.
IF AVAILABLE w-report-od THEN DO:
    BROWSE-2:SELECT-ROW (1).
END.
/*GET FIRST br_Table.*/
*/
APPLY "VALUE-CHANGED" TO BROWSE br_table.


SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesar-picking B-table-Win 
PROCEDURE procesar-picking :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR x-cod-ordenes AS CHAR.
DEFINE VAR x-cod-orden AS CHAR.
DEFINE VAR x-sec AS INT.
DEFINE VAR x-metros AS DEC.
DEFINE VAR x-fecha-cierre AS DATE.
DEFINE VAR x-fecha AS DATE.


DEFINE VAR x-coddiv AS CHAR.
DEFINE VAR x-proceso AS CHAR.

x-cod-ordenes = "O/D,O/M".

x-total-ordenes = 0.
x-total-metros = 0.
x-total-rollos = 0.
x-total-bultos = 0.

IF TRUE <> (fill-in-nroorden > "") THEN DO:
    REPEAT x-fecha = fill-in-desde TO fill-in-hasta:
        REPEAT x-sec = 1 TO NUM-ENTRIES(x-cod-ordenes,","):
            x-cod-orden = ENTRY(x-sec,x-cod-ordenes,",").
            /* OJO: se va a barrer TODAS las divisiones marcadas con EMBALADO ESPECIAL */
            FOR EACH x-faccpedi NO-LOCK WHERE x-faccpedi.codcia = s-codcia AND
                    x-faccpedi.fchchq = x-fecha AND
                    x-faccpedi.coddoc = x-cod-orden AND                                    
                    x-faccpedi.empaqespec = YES:

                RUN procesar-picking-carga-tempo.

            END.
        END.
    END.
END.
ELSE DO:
    x-cod-orden = radio-set-coddoc.
    FOR EACH x-faccpedi NO-LOCK WHERE x-faccpedi.codcia = s-codcia AND
        x-faccpedi.coddoc = x-cod-orden AND                                    
        x-faccpedi.nroped = fill-in-nroorden:
        /*x-faccpedi.empaqespec = YES:*/

        RUN procesar-picking-carga-tempo.
    END.
END.

DO WITH FRAME {&FRAME-NAME}:
    fill-in-tot-od:SCREEN-VALUE = STRING(x-total-ordenes,">,>>>,>>9").
    fill-in-tot-metros:SCREEN-VALUE = STRING(x-total-metros,">,>>>,>>9.99").
    fill-in-tot-rollos:SCREEN-VALUE = STRING(x-total-rollos,">,>>>,>>9.99").
    fill-in-tot-bultos:SCREEN-VALUE = STRING(x-total-bultos,">>,>>>,>>9").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesar-picking-carga-tempo B-table-Win 
PROCEDURE procesar-picking-carga-tempo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR x-metros AS DEC.            
DEFINE VAR x-fecha-cierre AS DATE.
DEFINE VAR x-fecha AS DATE.

DEFINE VAR x-coddiv AS CHAR.
DEFINE VAR x-proceso AS CHAR.

    x-metros = 0.
    x-fecha-cierre = ?.
    /* *********************************************************** */
    /* 25/01/2024: El tipo de proceso sale de la divisi�n despacho */
    /* *********************************************************** */
    x-proceso = "PXS".
    FIND gn-divi WHERE gn-divi.codcia = s-codcia AND
        gn-divi.coddiv = x-Faccpedi.divdes NO-LOCK NO-ERROR.
    IF AVAILABLE gn-divi THEN DO:
        x-Proceso = "".
        IF gn-divi.campo-char[6] = "PR" THEN x-Proceso = "PXR".
        IF gn-divi.campo-char[6] = "SE" THEN x-Proceso = "PXS".
    END.
    /* *********************************************************** */
    /* *********************************************************** */
    CASE x-Proceso:
        WHEN "PXR" THEN DO:
            /* Es Picking x Ruta */
            FIND FIRST vtacdocu WHERE vtacdocu.codcia = s-codcia AND 
                vtacdocu.codref = x-faccpedi.coddoc AND
                vtacdocu.nroref = x-faccpedi.nroped AND
                vtacdocu.codped = 'HPK' AND
                vtacdocu.flgest <> 'A' NO-LOCK NO-ERROR.
            IF AVAILABLE vtacdocu THEN DO:
                FECHACIERRE:
                FOR EACH vtacdocu WHERE vtacdocu.codcia = s-codcia AND 
                    vtacdocu.codref = x-faccpedi.coddoc AND
                    vtacdocu.nroref = x-faccpedi.nroped AND
                    vtacdocu.codped = 'HPK' AND
                    vtacdocu.flgest <> 'A' NO-LOCK:
                    x-metros = x-metros + VtaCDocu.Importe[3].
                    IF Vtacdocu.fecsac = ? THEN DO:
                        x-fecha-cierre = ?.
                        LEAVE FECHACIERRE.
                    END.
                    IF x-fecha-cierre = ? THEN x-fecha-cierre = Vtacdocu.fecsac.
                    IF Vtacdocu.fecsac > x-fecha-cierre THEN x-fecha-cierre = Vtacdocu.fecsac.                    
                END.
            END.
            /* La orden no esta concluido el checking */
            IF x-fecha-cierre = ? THEN NEXT.
        END.
        /*WHEN "PXS" THEN DO:*/
        OTHERWISE DO:
            /* Es picking x sector */
            x-metros = x-faccpedi.acubon[7].
            x-fecha-cierre = x-faccpedi.fchchq.
            IF x-fecha-cierre = ? THEN NEXT.
            x-Proceso = "PXS".      /* OJO */
        END.
    END CASE.
    /*  */
    CREATE w-report-od.
    ASSIGN 
        w-report-od.campo-d[1] = x-fecha-cierre
        w-report-od.campo-c[1] = x-faccpedi.coddoc
        w-report-od.campo-c[2] = x-faccpedi.nroped
        w-report-od.campo-f[1] = x-metros
        w-report-od.campo-f[2] = w-report-od.campo-f[1] / fill-in-metros
        w-report-od.campo-f[3] = x-faccpedi.peso
        w-report-od.campo-c[3] = x-proceso
        w-report-od.campo-c[4] = x-faccpedi.nomcli
        w-report-od.campo-c[5] = x-faccpedi.divdes
        w-report-od.campo-i[1] = 0
        x-total-ordenes = x-total-ordenes + 1
        x-total-metros = x-total-metros + w-report-od.campo-f[1]
        x-total-rollos = x-total-rollos + w-report-od.campo-f[2]
        .
    /* Cross Docking */
    IF x-Faccpedi.DT = YES THEN w-report-od.campo-c[18] = "SI".
    ELSE w-report-od.campo-c[18] = "NO".
    /* Bultos */
    FOR EACH Ccbcbult NO-LOCK WHERE ccbcbult.codcia = s-codcia AND
        ccbcbult.coddoc = x-faccpedi.coddoc AND
        ccbcbult.nrodoc = x-faccpedi.nroped:
        ASSIGN w-report-od.campo-i[1] = w-report-od.campo-i[1] + ccbcbult.bultos.
    END.
    x-total-bultos = x-total-bultos + w-report-od.campo-i[1].
    /* 14/03/2023 M.Ramos */
    /* Destino Final */
    FIND Gn-ClieD WHERE Gn-ClieD.CodCia = cl-codcia AND
        Gn-ClieD.CodCli = x-faccpedi.codcli AND 
        Gn-ClieD.Sede = x-faccpedi.Sede
        NO-LOCK NO-ERROR.
    IF AVAILABLE Gn-ClieD THEN DO:
        w-report-od.campo-c[14] = Gn-ClieD.DirCli.
        FIND TabDepto WHERE TabDepto.CodDepto = Gn-ClieD.CodDept NO-LOCK NO-ERROR.
        IF AVAILABLE TabDepto THEN w-report-od.campo-c[15] = TabDepto.NomDepto.
        FIND TabProvi WHERE TabProvi.CodDepto = Gn-ClieD.CodDept AND
            TabProvi.CodProvi = Gn-ClieD.CodProv NO-LOCK NO-ERROR.
        IF AVAILABLE TabProvi THEN w-report-od.campo-c[16] = TabProvi.NomProvi.
        FIND TabDistr WHERE TabDistr.CodDepto = Gn-ClieD.CodDept AND
            TabDistr.CodProvi = Gn-ClieD.CodProv AND
            TabDistr.CodDistr = Gn-ClieD.CodDist NO-LOCK NO-ERROR.
        IF AVAILABLE TabDistr THEN w-report-od.campo-c[17] = TabDistr.NomDistr.
    END.
    /* Destino Inicial */
    FIND CcbADocu WHERE CcbADocu.CodCia = x-faccpedi.codcia AND
        CcbADocu.CodDiv = x-faccpedi.coddiv AND
        CcbADocu.CodDoc = x-faccpedi.coddoc AND
        CcbADocu.NroDoc = x-faccpedi.nroped AND 
        CAN-FIND(FIRST gn-prov WHERE gn-prov.CodCia = PV-CODCIA AND 
                 gn-prov.CodPro  = CcbADocu.Libre_C[9] NO-LOCK)
        NO-LOCK NO-ERROR.
    IF AVAILABLE CcbADocu THEN DO:
        FIND FIRST gn-prov WHERE gn-prov.CodCia = PV-CODCIA AND 
            gn-prov.CodPro  = CcbADocu.Libre_C[9] NO-LOCK.
        w-report-od.campo-c[10] = gn-prov.DirPro.
        FIND TabDepto WHERE TabDepto.CodDepto = gn-prov.CodDept NO-LOCK NO-ERROR.
        IF AVAILABLE TabDepto THEN w-report-od.campo-c[11] = TabDepto.NomDepto.
        FIND TabProvi WHERE TabProvi.CodDepto = gn-prov.CodDept AND
            TabProvi.CodProvi = gn-prov.CodProv NO-LOCK NO-ERROR.
        IF AVAILABLE TabProvi THEN w-report-od.campo-c[12] = TabProvi.NomProvi.
        FIND TabDistr WHERE TabDistr.CodDepto = gn-prov.CodDept AND
            TabDistr.CodProvi = gn-prov.CodProv AND
            TabDistr.CodDistr = gn-prov.CodDist NO-LOCK NO-ERROR.
        IF AVAILABLE TabDistr THEN w-report-od.campo-c[13] = TabDistr.NomDistr.
    END.
    /* Vendedor */
    FIND gn-ven WHERE gn-ven.CodCia = s-codcia AND 
        gn-ven.CodVen = x-faccpedi.codven
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-ven THEN w-report-od.campo-c[19] = CAPS(gn-ven.NomVen).
    /* Divisi�n de venta */
    FIND gn-divi WHERE GN-DIVI.CodCia = s-codcia AND
        GN-DIVI.CodDiv = x-faccpedi.coddiv NO-LOCK NO-ERROR.
    IF AVAILABLE gn-divi THEN w-report-od.campo-c[20] = CAPS(GN-DIVI.CodDiv + " " + GN-DIVI.DesDiv).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesar-picking-old B-table-Win 
PROCEDURE procesar-picking-old :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR x-cod-ordenes AS CHAR.
DEFINE VAR x-cod-orden AS CHAR.
DEFINE VAR x-sec AS INT.
DEFINE VAR x-metros AS DEC.
DEFINE VAR x-fecha-cierre AS DATE.
DEFINE VAR x-fecha AS DATE.

DEFINE VAR x-total-ordenes AS INT.
DEFINE VAR x-total-metros AS DEC.
DEFINE VAR x-total-rollos AS DEC.
DEFINE VAR x-total-bultos AS DEC.

DEFINE VAR x-coddiv AS CHAR.
DEFINE VAR x-proceso AS CHAR.

x-cod-ordenes = "O/D,O/M".

/* */
REPEAT x-fecha = fill-in-desde TO fill-in-hasta:
    REPEAT x-sec = 1 TO NUM-ENTRIES(x-cod-ordenes,","):
        x-cod-orden = ENTRY(x-sec,x-cod-ordenes,",").
        /* OJO: se va a barrer TODAS las divisiones marcadas con EMBALADO ESPECIAL */
        FOR EACH x-faccpedi NO-LOCK WHERE x-faccpedi.codcia = s-codcia AND
                x-faccpedi.fchchq = x-fecha AND
                x-faccpedi.coddoc = x-cod-orden AND                                    
                x-faccpedi.empaqespec = YES:
            x-metros = 0.
            x-fecha-cierre = ?.
            /* *********************************************************** */
            /* 25/01/2024: El tipo de proceso sale de la divisi�n despacho */
            /* *********************************************************** */
            x-proceso = "PXS".
            FIND gn-divi WHERE gn-divi.codcia = s-codcia AND
                gn-divi.coddiv = x-Faccpedi.divdes NO-LOCK NO-ERROR.
            IF AVAILABLE gn-divi THEN DO:
                x-Proceso = "".
                IF gn-divi.campo-char[6] = "PR" THEN x-Proceso = "PXR".
                IF gn-divi.campo-char[6] = "SE" THEN x-Proceso = "PXS".
            END.
            /* *********************************************************** */
            /* *********************************************************** */
            CASE x-Proceso:
                WHEN "PXR" THEN DO:
                    /* Es Picking x Ruta */
                    FIND FIRST vtacdocu WHERE vtacdocu.codcia = s-codcia AND 
                        vtacdocu.codref = x-faccpedi.coddoc AND
                        vtacdocu.nroref = x-faccpedi.nroped AND
                        vtacdocu.codped = 'HPK' AND
                        vtacdocu.flgest <> 'A' NO-LOCK NO-ERROR.
                    IF AVAILABLE vtacdocu THEN DO:
                        FECHACIERRE:
                        FOR EACH vtacdocu WHERE vtacdocu.codcia = s-codcia AND 
                            vtacdocu.codref = x-faccpedi.coddoc AND
                            vtacdocu.nroref = x-faccpedi.nroped AND
                            vtacdocu.codped = 'HPK' AND
                            vtacdocu.flgest <> 'A' NO-LOCK:
                            x-metros = x-metros + VtaCDocu.Importe[3].
                            IF Vtacdocu.fecsac = ? THEN DO:
                                x-fecha-cierre = ?.
                                LEAVE FECHACIERRE.
                            END.
                            IF x-fecha-cierre = ? THEN x-fecha-cierre = Vtacdocu.fecsac.
                            IF Vtacdocu.fecsac > x-fecha-cierre THEN x-fecha-cierre = Vtacdocu.fecsac.                    
                        END.
                    END.
                    /* La orden no esta concluido el checking */
                    IF x-fecha-cierre = ? THEN NEXT.
                END.
                /*WHEN "PXS" THEN DO:*/
                OTHERWISE DO:
                    /* Es picking x sector */
                    x-metros = x-faccpedi.acubon[7].
                    x-fecha-cierre = x-faccpedi.fchchq.
                    IF x-fecha-cierre = ? THEN NEXT.
                    x-Proceso = "PXS".      /* OJO */
                END.
            END CASE.
            /*  */
            CREATE w-report-od.
            ASSIGN 
                w-report-od.campo-d[1] = x-fecha-cierre
                w-report-od.campo-c[1] = x-faccpedi.coddoc
                w-report-od.campo-c[2] = x-faccpedi.nroped
                w-report-od.campo-f[1] = x-metros
                w-report-od.campo-f[2] = w-report-od.campo-f[1] / fill-in-metros
                w-report-od.campo-f[3] = x-faccpedi.peso
                w-report-od.campo-c[3] = x-proceso
                w-report-od.campo-c[4] = x-faccpedi.nomcli
                w-report-od.campo-c[5] = x-faccpedi.divdes
                w-report-od.campo-i[1] = 0
                x-total-ordenes = x-total-ordenes + 1
                x-total-metros = x-total-metros + w-report-od.campo-f[1]
                x-total-rollos = x-total-rollos + w-report-od.campo-f[2]
                .
            /* Cross Docking */
            IF x-Faccpedi.DT = YES THEN w-report-od.campo-c[18] = "SI".
            ELSE w-report-od.campo-c[18] = "NO".
            /* Bultos */
            FOR EACH Ccbcbult NO-LOCK WHERE ccbcbult.codcia = s-codcia AND
                ccbcbult.coddoc = x-faccpedi.coddoc AND
                ccbcbult.nrodoc = x-faccpedi.nroped:
                ASSIGN w-report-od.campo-i[1] = w-report-od.campo-i[1] + ccbcbult.bultos.
            END.
            x-total-bultos = x-total-bultos + w-report-od.campo-i[1].
            /* 14/03/2023 M.Ramos */
            /* Destino Final */
            FIND Gn-ClieD WHERE Gn-ClieD.CodCia = cl-codcia AND
                Gn-ClieD.CodCli = x-faccpedi.codcli AND 
                Gn-ClieD.Sede = x-faccpedi.Sede
                NO-LOCK NO-ERROR.
            IF AVAILABLE Gn-ClieD THEN DO:
                w-report-od.campo-c[14] = Gn-ClieD.DirCli.
                FIND TabDepto WHERE TabDepto.CodDepto = Gn-ClieD.CodDept NO-LOCK NO-ERROR.
                IF AVAILABLE TabDepto THEN w-report-od.campo-c[15] = TabDepto.NomDepto.
                FIND TabProvi WHERE TabProvi.CodDepto = Gn-ClieD.CodDept AND
                    TabProvi.CodProvi = Gn-ClieD.CodProv NO-LOCK NO-ERROR.
                IF AVAILABLE TabProvi THEN w-report-od.campo-c[16] = TabProvi.NomProvi.
                FIND TabDistr WHERE TabDistr.CodDepto = Gn-ClieD.CodDept AND
                    TabDistr.CodProvi = Gn-ClieD.CodProv AND
                    TabDistr.CodDistr = Gn-ClieD.CodDist NO-LOCK NO-ERROR.
                IF AVAILABLE TabDistr THEN w-report-od.campo-c[17] = TabDistr.NomDistr.
            END.
            /* Destino Inicial */
            FIND CcbADocu WHERE CcbADocu.CodCia = x-faccpedi.codcia AND
                CcbADocu.CodDiv = x-faccpedi.coddiv AND
                CcbADocu.CodDoc = x-faccpedi.coddoc AND
                CcbADocu.NroDoc = x-faccpedi.nroped AND 
                CAN-FIND(FIRST gn-prov WHERE gn-prov.CodCia = PV-CODCIA AND 
                         gn-prov.CodPro  = CcbADocu.Libre_C[9] NO-LOCK)
                NO-LOCK NO-ERROR.
            IF AVAILABLE CcbADocu THEN DO:
                FIND FIRST gn-prov WHERE gn-prov.CodCia = PV-CODCIA AND 
                    gn-prov.CodPro  = CcbADocu.Libre_C[9] NO-LOCK.
                w-report-od.campo-c[10] = gn-prov.DirPro.
                FIND TabDepto WHERE TabDepto.CodDepto = gn-prov.CodDept NO-LOCK NO-ERROR.
                IF AVAILABLE TabDepto THEN w-report-od.campo-c[11] = TabDepto.NomDepto.
                FIND TabProvi WHERE TabProvi.CodDepto = gn-prov.CodDept AND
                    TabProvi.CodProvi = gn-prov.CodProv NO-LOCK NO-ERROR.
                IF AVAILABLE TabProvi THEN w-report-od.campo-c[12] = TabProvi.NomProvi.
                FIND TabDistr WHERE TabDistr.CodDepto = gn-prov.CodDept AND
                    TabDistr.CodProvi = gn-prov.CodProv AND
                    TabDistr.CodDistr = gn-prov.CodDist NO-LOCK NO-ERROR.
                IF AVAILABLE TabDistr THEN w-report-od.campo-c[13] = TabDistr.NomDistr.
            END.
            /* Vendedor */
            FIND gn-ven WHERE gn-ven.CodCia = s-codcia AND 
                gn-ven.CodVen = x-faccpedi.codven
                NO-LOCK NO-ERROR.
            IF AVAILABLE gn-ven THEN w-report-od.campo-c[19] = CAPS(gn-ven.NomVen).
            /* Divisi�n de venta */
            FIND gn-divi WHERE GN-DIVI.CodCia = s-codcia AND
                GN-DIVI.CodDiv = x-faccpedi.coddiv NO-LOCK NO-ERROR.
            IF AVAILABLE gn-divi THEN w-report-od.campo-c[20] = CAPS(GN-DIVI.CodDiv + " " + GN-DIVI.DesDiv).
        END.
    END.
END.

DO WITH FRAME {&FRAME-NAME}:
    fill-in-tot-od:SCREEN-VALUE = STRING(x-total-ordenes,">,>>>,>>9").
    fill-in-tot-metros:SCREEN-VALUE = STRING(x-total-metros,">,>>>,>>9.99").
    fill-in-tot-rollos:SCREEN-VALUE = STRING(x-total-rollos,">,>>>,>>9.99").
    fill-in-tot-bultos:SCREEN-VALUE = STRING(x-total-bultos,">>,>>>,>>9").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesar-txt B-table-Win 
PROCEDURE procesar-txt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR x-archivo AS CHAR.

x-archivo = x-directorio + "\embalaj_especial.txt".

OUTPUT STREAM REPORT-TXT TO VALUE(x-Archivo).

PUT STREAM REPORT-txt
    "Cierre de Orden|" 
    "Div despacho|"
    "Cod Orden|"
    "Nro. Orden|"
    "Metraje utilizado|"
    "Cantidad rollos|"
    "Peso|"
    "Procs|"
    "CrossDock|"
    "Cliente|"
    "Bultos"          "|"
    "Direcci�n"       "|"
    "Departamento"    "|"
    "Provincia"       "|"
    "Distrito"        "|"
    "Direcci�n"       "|"
    "Departamento"    "|"
    "Provincia"       "|"
    "Distrito"        "|"
    "Chequeador"      "|"
    "Vendedor"        "|"
    "Div. de Venta"
    SKIP.
FOR EACH w-report-od:
  PUT STREAM REPORT-txt
      w-report-od.campo-d[1] "|"
      w-report-od.campo-c[5] "|"
      w-report-od.campo-c[1] "|"
      w-report-od.campo-c[2] FORMAT 'x(15)' "|"
      w-report-od.campo-f[1] "|"
      w-report-od.campo-f[2] "|"
      w-report-od.campo-f[3] "|"
      w-report-od.campo-c[3] "|"
      w-report-od.campo-c[18] "|"
      w-report-od.campo-c[4] FORMAT 'x(80)' "|"
      w-report-od.campo-i[1] FORMAT '>>,>>>,>>9'  "|"
      w-report-od.campo-c[10] FORMAT "x(60)"      "|"
      w-report-od.campo-c[11] FORMAT "x(20)"      "|"
      w-report-od.campo-c[12] FORMAT "x(20)"      "|"
      w-report-od.campo-c[13] FORMAT "x(20)"      "|"
      w-report-od.campo-c[14] FORMAT "x(60)"      "|"
      w-report-od.campo-c[15] FORMAT "x(20)"      "|"
      w-report-od.campo-c[16] FORMAT "x(20)"      "|"
      w-report-od.campo-c[17] FORMAT "x(20)"      "|"
      w-report-od.campo-c[18] FORMAT "x(30)"      "|"
      w-report-od.campo-c[19] FORMAT "x(30)"      "|"
      w-report-od.campo-c[20] FORMAT "x(40)"
      SKIP.
END.

OUTPUT STREAM REPORT-txt CLOSE.
MESSAGE 'Archivo creando en :' SKIP
    x-archivo
     VIEW-AS ALERT-BOX INFORMATION.


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
  {src/adm/template/snd-list.i "w-report-od"}
  {src/adm/template/snd-list.i "w-report-film"}

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

