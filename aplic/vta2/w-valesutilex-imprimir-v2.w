&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
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

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
          
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

DEF VAR RB-REPORT-LIBRARY AS CHAR.  /* Archivo PRL a usar */
DEF VAR RB-REPORT-NAME AS CHAR.     /* Nombre del reporte */
DEF VAR RB-INCLUDE-RECORDS AS CHAR. /* "O" si necesita filtro */
DEF VAR RB-FILTER AS CHAR.  /* Filtro de impresion */
DEF VAR RB-OTHER-PARAMETERS AS CHAR INITIAL "".     /* Otros parametros */

DEFINE VARIABLE s-task-no AS INTEGER     NO-UNDO.

DEFINE SHARED VAR s-codcia AS INT.
DEFINE SHARED VAR cl-codcia AS INT.
DEFINE SHARED VAR s-coddiv AS CHAR.
DEFINE VAR cCodDoc  AS CHAR INIT "VAL".

DEFINE VARIABLE iLin      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCol      AS INTEGER     NO-UNDO.

DEF BUFFER B-vtatabla FOR vtatabla.

cCodDoc = 'VAL'.            /* CodDoc VALES */
s-coddiv = '00023'.         /* Utilex */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES VtaTabla

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 VtaTabla.Llave_c1 VtaTabla.Llave_c2 ~
VtaTabla.LLave_c3 VtaTabla.Rango_fecha[1] VtaTabla.Libre_c01 ~
VtaTabla.Libre_c02 VtaTabla.Valor[1] VtaTabla.Valor[7] VtaTabla.Valor[2] ~
VtaTabla.Valor[3] VtaTabla.Valor[4] VtaTabla.Valor[5] VtaTabla.Valor[6] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH VtaTabla ~
      WHERE VtaTabla.tabla = 'VUTILEX' and VtaTabla.Libre_c03 = "GENERADO" NO-LOCK ~
    BY VtaTabla.Rango_fecha[2] INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH VtaTabla ~
      WHERE VtaTabla.tabla = 'VUTILEX' and VtaTabla.Libre_c03 = "GENERADO" NO-LOCK ~
    BY VtaTabla.Rango_fecha[2] INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 VtaTabla
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 VtaTabla


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 BROWSE-2 txt-codcli 
&Scoped-Define DISPLAYED-OBJECTS txt-codpro cboProvProd txt-fecha txtProd ~
txt-codcli txt-nomcli txtImporte txtCuantos txtCuantos-7 txtImporte-7 ~
txtCuantos-2 txtImporte-2 txtImporte-3 txtCuantos-3 txtImporte-4 ~
txtCuantos-4 txtImporte-5 txtCuantos-5 txtImporte-6 txtCuantos-6 txtTotal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "img/print (2).ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Grabar" 
     SIZE 13 BY 2.69.

DEFINE VARIABLE cboProvProd AS CHARACTER FORMAT "X(256)":U 
     LABEL "Proveedor (Producto)" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Continental-CON Textos Escolares ","10003814-0001",
                     "Continental-SIN Texto Escolares","10003814-0002",
                     "Continental-Personal Continental","10003814-0003",
                     "Continental-Campa�a 2015","10003814-0004",
                     "Continental-Campa�a 2016","10003814-0005",
                     "Continental-Campa�a 2017","10003814-0006",
                     "Continental-Campa�a Nakamoto","10003814-0007",
                     "EdenRed","50763447-02",
                     "Dodexho","50785254-730"
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE txt-codcli AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 8 FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE txt-codpro AS CHARACTER FORMAT "X(256)":U INITIAL "50763447" 
     LABEL "Prov." 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 8 FGCOLOR 9 FONT 6 NO-UNDO.

DEFINE VARIABLE txt-fecha AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Vencimiento" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE txt-nomcli AS CHARACTER FORMAT "X(30)":U 
     LABEL "Nombre" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

DEFINE VARIABLE txtCuantos AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .77 NO-UNDO.

DEFINE VARIABLE txtCuantos-2 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .77 NO-UNDO.

DEFINE VARIABLE txtCuantos-3 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .77 NO-UNDO.

DEFINE VARIABLE txtCuantos-4 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .77 NO-UNDO.

DEFINE VARIABLE txtCuantos-5 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .77 NO-UNDO.

DEFINE VARIABLE txtCuantos-6 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .77 NO-UNDO.

DEFINE VARIABLE txtCuantos-7 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY .77 NO-UNDO.

DEFINE VARIABLE txtImporte AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "S/." 
     VIEW-AS FILL-IN 
     SIZE 30.72 BY 1.35
     FONT 8 NO-UNDO.

DEFINE VARIABLE txtImporte-2 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "S/." 
     VIEW-AS FILL-IN 
     SIZE 30.72 BY 1.31
     FONT 8 NO-UNDO.

DEFINE VARIABLE txtImporte-3 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "S/." 
     VIEW-AS FILL-IN 
     SIZE 30.72 BY 1.35
     FONT 8 NO-UNDO.

DEFINE VARIABLE txtImporte-4 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "S/." 
     VIEW-AS FILL-IN 
     SIZE 30.72 BY 1.27
     FONT 8 NO-UNDO.

DEFINE VARIABLE txtImporte-5 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "S/." 
     VIEW-AS FILL-IN 
     SIZE 30.72 BY 1.38
     FONT 8 NO-UNDO.

DEFINE VARIABLE txtImporte-6 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "S/." 
     VIEW-AS FILL-IN 
     SIZE 30.72 BY 1.35
     FONT 8 NO-UNDO.

DEFINE VARIABLE txtImporte-7 AS INTEGER FORMAT ">>>,>>9":U INITIAL 0 
     LABEL "S/." 
     VIEW-AS FILL-IN 
     SIZE 30.72 BY 1.31
     FONT 8 NO-UNDO.

DEFINE VARIABLE txtProd AS CHARACTER FORMAT "X(256)":U 
     LABEL "Prod." 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE txtTotal AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     LABEL "Importe Total" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1.35
     FGCOLOR 9 FONT 8 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      VtaTabla SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 W-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      VtaTabla.Llave_c1 COLUMN-LABEL "Control" FORMAT "x(12)":U
            WIDTH 15.43
      VtaTabla.Llave_c2 COLUMN-LABEL "Proveedor" FORMAT "x(10)":U
      VtaTabla.LLave_c3 COLUMN-LABEL "Producto" FORMAT "x(8)":U
      VtaTabla.Rango_fecha[1] COLUMN-LABEL "Vcto" FORMAT "99/99/9999":U
      VtaTabla.Libre_c01 COLUMN-LABEL "CodClie" FORMAT "x(12)":U
      VtaTabla.Libre_c02 COLUMN-LABEL "NomClie" FORMAT "x(30)":U
      VtaTabla.Valor[1] COLUMN-LABEL "S/. 10" FORMAT "->>>,>>>,>>9":U
      VtaTabla.Valor[7] COLUMN-LABEL "S/. 15" FORMAT "->>>,>>>,>>9":U
      VtaTabla.Valor[2] COLUMN-LABEL "S/. 20" FORMAT "->>>,>>>,>>9":U
      VtaTabla.Valor[3] COLUMN-LABEL "S/. 25" FORMAT "->>>,>>>,>>9":U
      VtaTabla.Valor[4] COLUMN-LABEL "S/. 30" FORMAT "->>>,>>>,>>9":U
      VtaTabla.Valor[5] COLUMN-LABEL "S/. 50" FORMAT "->>>,>>>,>>9":U
      VtaTabla.Valor[6] COLUMN-LABEL "S/. 100" FORMAT "->>>,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 107.29 BY 5.69 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-1 AT ROW 9.58 COL 92.29 WIDGET-ID 2
     BROWSE-2 AT ROW 2.54 COL 2 WIDGET-ID 200
     txt-codpro AT ROW 8.62 COL 73.57 COLON-ALIGNED WIDGET-ID 10
     cboProvProd AT ROW 8.65 COL 26.43 COLON-ALIGNED WIDGET-ID 22
     txt-fecha AT ROW 9.77 COL 26.29 COLON-ALIGNED WIDGET-ID 4
     txtProd AT ROW 9.88 COL 60.29 COLON-ALIGNED WIDGET-ID 24
     txt-codcli AT ROW 11 COL 26.29 COLON-ALIGNED WIDGET-ID 16
     txt-nomcli AT ROW 12.04 COL 26.29 COLON-ALIGNED WIDGET-ID 18
     txtImporte AT ROW 13.23 COL 76.29 COLON-ALIGNED WIDGET-ID 54
     txtCuantos AT ROW 13.46 COL 59.29 COLON-ALIGNED NO-LABEL WIDGET-ID 42
     txtCuantos-7 AT ROW 14.73 COL 59.29 COLON-ALIGNED NO-LABEL WIDGET-ID 70
     txtImporte-7 AT ROW 14.77 COL 76.29 COLON-ALIGNED WIDGET-ID 72
     txtCuantos-2 AT ROW 16.15 COL 59.29 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     txtImporte-2 AT ROW 16.19 COL 76.29 COLON-ALIGNED WIDGET-ID 56
     txtImporte-3 AT ROW 17.62 COL 76.29 COLON-ALIGNED WIDGET-ID 58
     txtCuantos-3 AT ROW 17.81 COL 59.29 COLON-ALIGNED NO-LABEL WIDGET-ID 46
     txtImporte-4 AT ROW 19.08 COL 76.29 COLON-ALIGNED WIDGET-ID 60
     txtCuantos-4 AT ROW 19.27 COL 59.29 COLON-ALIGNED NO-LABEL WIDGET-ID 48
     txtImporte-5 AT ROW 20.5 COL 76.29 COLON-ALIGNED WIDGET-ID 62
     txtCuantos-5 AT ROW 20.73 COL 59.29 COLON-ALIGNED NO-LABEL WIDGET-ID 50
     txtImporte-6 AT ROW 22 COL 76.29 COLON-ALIGNED WIDGET-ID 64
     txtCuantos-6 AT ROW 22.15 COL 59.29 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     txtTotal AT ROW 24.42 COL 68 COLON-ALIGNED WIDGET-ID 66
     "       2.- Generacion de Vales Utilex - Detalle de los vales" VIEW-AS TEXT
          SIZE 111.57 BY 1.27 AT ROW 1.12 COL 1.43 WIDGET-ID 74
          BGCOLOR 15 FGCOLOR 2 FONT 11
     "2.- Vales con valor de Quince Nuevos Soles (S/. 15.00)" VIEW-AS TEXT
          SIZE 46 BY .62 AT ROW 14.96 COL 4.29 WIDGET-ID 68
     "1.- Vales con valor de Diez Nuevos Soles (S/. 10.00)" VIEW-AS TEXT
          SIZE 46 BY .62 AT ROW 13.46 COL 4.29 WIDGET-ID 26
     "7.- Vales con valor de Cien Nuevos Soles (S/. 100.00)" VIEW-AS TEXT
          SIZE 51 BY .62 AT ROW 22.15 COL 4.29 WIDGET-ID 36
     "6.- Vales con valor de Cincuenta Nuevos Soles (S/. 50.00)" VIEW-AS TEXT
          SIZE 51 BY .62 AT ROW 20.81 COL 4.29 WIDGET-ID 34
     "5.- Vales con valor de Treinta Nuevos Soles (S/. 30.00)" VIEW-AS TEXT
          SIZE 51 BY .62 AT ROW 19.27 COL 4.29 WIDGET-ID 32
     "4.- Vales con valor de Veinticinco Nuevos Soles (S/. 25.00)" VIEW-AS TEXT
          SIZE 51 BY .62 AT ROW 17.81 COL 4.29 WIDGET-ID 30
     "3.- Vales con valor de Veinte Nuevos Soles (S/. 20.00)" VIEW-AS TEXT
          SIZE 46 BY .62 AT ROW 16.38 COL 4.29 WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 122.72 BY 25.38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "2.- Generacion de Vales - detalle de cada vale"
         HEIGHT             = 25.42
         WIDTH              = 113
         MAX-HEIGHT         = 25.42
         MAX-WIDTH          = 122.72
         VIRTUAL-HEIGHT     = 25.42
         VIRTUAL-WIDTH      = 122.72
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 TEXT-2 F-Main */
/* SETTINGS FOR COMBO-BOX cboProvProd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txt-codpro IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txt-fecha IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txt-nomcli IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtCuantos IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtCuantos-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtCuantos-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtCuantos-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtCuantos-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtCuantos-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtCuantos-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtImporte IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtImporte-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtImporte-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtImporte-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtImporte-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtImporte-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtImporte-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtProd IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtTotal IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "INTEGRAL.VtaTabla"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "INTEGRAL.VtaTabla.Rango_fecha[2]|yes"
     _Where[1]         = "VtaTabla.tabla = 'VUTILEX' and VtaTabla.Libre_c03 = ""GENERADO"""
     _FldNameList[1]   > INTEGRAL.VtaTabla.Llave_c1
"VtaTabla.Llave_c1" "Control" "x(12)" "character" ? ? ? ? ? ? no ? no no "15.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.VtaTabla.Llave_c2
"VtaTabla.Llave_c2" "Proveedor" "x(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.VtaTabla.LLave_c3
"VtaTabla.LLave_c3" "Producto" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.VtaTabla.Rango_fecha[1]
"VtaTabla.Rango_fecha[1]" "Vcto" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > INTEGRAL.VtaTabla.Libre_c01
"VtaTabla.Libre_c01" "CodClie" "x(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > INTEGRAL.VtaTabla.Libre_c02
"VtaTabla.Libre_c02" "NomClie" "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > INTEGRAL.VtaTabla.Valor[1]
"VtaTabla.Valor[1]" "S/. 10" "->>>,>>>,>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > INTEGRAL.VtaTabla.Valor[7]
"VtaTabla.Valor[7]" "S/. 15" "->>>,>>>,>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > INTEGRAL.VtaTabla.Valor[2]
"VtaTabla.Valor[2]" "S/. 20" "->>>,>>>,>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > INTEGRAL.VtaTabla.Valor[3]
"VtaTabla.Valor[3]" "S/. 25" "->>>,>>>,>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > INTEGRAL.VtaTabla.Valor[4]
"VtaTabla.Valor[4]" "S/. 30" "->>>,>>>,>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > INTEGRAL.VtaTabla.Valor[5]
"VtaTabla.Valor[5]" "S/. 50" "->>>,>>>,>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > INTEGRAL.VtaTabla.Valor[6]
"VtaTabla.Valor[6]" "S/. 100" "->>>,>>>,>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* 2.- Generacion de Vales - detalle de cada vale */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* 2.- Generacion de Vales - detalle de cada vale */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 W-Win
ON MOUSE-SELECT-CLICK OF BROWSE-2 IN FRAME F-Main
DO:
  txtCuantos:SCREEN-VALUE = STRING(vtatabla.valor[1],">,>>>,>>9").
  txtCuantos-2:SCREEN-VALUE = STRING(vtatabla.valor[2],">,>>>,>>9").
  txtCuantos-3:SCREEN-VALUE = STRING(vtatabla.valor[3],">,>>>,>>9").
  txtCuantos-4:SCREEN-VALUE = STRING(vtatabla.valor[4],">,>>>,>>9").
  txtCuantos-5:SCREEN-VALUE = STRING(vtatabla.valor[5],">,>>>,>>9").
  txtCuantos-6:SCREEN-VALUE = STRING(vtatabla.valor[6],">,>>>,>>9").
  txtCuantos-7:SCREEN-VALUE = STRING(vtatabla.valor[7],">,>>>,>>9").

  txtImporte:SCREEN-VALUE = STRING(vtatabla.valor[1] * 10,">,>>>,>>9").
  txtImporte-2:SCREEN-VALUE = STRING(vtatabla.valor[2] * 20,">,>>>,>>9").
  txtImporte-3:SCREEN-VALUE = STRING(vtatabla.valor[3] * 25,">,>>>,>>9").
  txtImporte-4:SCREEN-VALUE = STRING(vtatabla.valor[4] * 30,">,>>>,>>9").
  txtImporte-5:SCREEN-VALUE = STRING(vtatabla.valor[5] * 50,">,>>>,>>9").
  txtImporte-6:SCREEN-VALUE = STRING(vtatabla.valor[6] * 100,">,>>>,>>9").
  txtImporte-7:SCREEN-VALUE = STRING(vtatabla.valor[7] * 15,">,>>>,>>9").

  txtTotal:SCREEN-VALUE = STRING((vtatabla.valor[1] * 10) + 
                                     (vtatabla.valor[2] * 20) + 
                                     (vtatabla.valor[3] * 25) + 
                                     (vtatabla.valor[4] * 30) + 
                                     (vtatabla.valor[5] * 50) + 
                                     (vtatabla.valor[6] * 100) +
                                     (vtatabla.valor[7] * 15) 
                                     ,">,>>>,>>9").

  cboProvProd:SCREEN-VALUE = vtatabla.llave_c2 + "-" + vtatabla.llave_c3.
  txt-CodPro:SCREEN-VALUE = vtatabla.llave_c2.
  txtProd:SCREEN-VALUE = vtatabla.llave_c3.
  txt-fecha:SCREEN-VALUE = string(vtatabla.rango_fecha[1],"99/99/9999").
  txt-codcli:SCREEN-VALUE = vtatabla.libre_c01.
  txt-nomcli:SCREEN-VALUE = vtatabla.libre_c02.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Grabar */
DO:
    ASSIGN 
        txt-codpro
        txt-fecha
        txt-codcli
        txt-nomcli
        cboProvProd
        txtCuantos
        txtCuantos-2
        txtCuantos-3
        txtCuantos-4
        txtCuantos-5
        txtCuantos-6
        txtCuantos-7
        txtImporte
        txtImporte-2
        txtImporte-3
        txtImporte-4
        txtImporte-5
        txtImporte-6
        txtImporte-7.


    RUN Imprimir.

        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cboProvProd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cboProvProd W-Win
ON VALUE-CHANGED OF cboProvProd IN FRAME F-Main /* Proveedor (Producto) */
DO:
  ASSIGN cboProvProd.
  txt-codpro:SCREEN-VALUE = substring(cboProvProd,1,8).
  txtProd:SCREEN-VALUE = substring(cboProvProd,10,4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txt-codcli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txt-codcli W-Win
ON LEAVE OF txt-codcli IN FRAME F-Main /* Cliente */
DO:
    ASSIGN {&SELF-NAME}.
    FIND FIRST gn-clie WHERE gn-clie.codcia = cl-codcia
        AND gn-clie.codcli = txt-codcli NO-LOCK NO-ERROR.
    IF AVAIL gn-clie THEN 
        DISPLAY gn-clie.nomcli @ txt-nomcli WITH FRAME {&FRAME-NAME}.
    ELSE 
        DISPLAY "" @ txt-nomcli WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtCuantos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtCuantos W-Win
ON LEAVE OF txtCuantos IN FRAME F-Main
DO:
  DEFINE VAR lCuantos AS INT.

  lCuantos = INTEGER(SELF:SCREEN-VALUE) * 10.
  
  txtImporte:SCREEN-VALUE = STRING(lCuantos,">,>>>,>>9").

  RUN um-suma.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtCuantos-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtCuantos-2 W-Win
ON LEAVE OF txtCuantos-2 IN FRAME F-Main
DO:
    DEFINE VAR lCuantos AS INT.

    lCuantos = INTEGER(SELF:SCREEN-VALUE) * 20.

    txtImporte-2:SCREEN-VALUE = STRING(lCuantos,">,>>>,>>9").

    RUN um-suma.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtCuantos-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtCuantos-3 W-Win
ON LEAVE OF txtCuantos-3 IN FRAME F-Main
DO:
    DEFINE VAR lCuantos AS INT.

    lCuantos = INTEGER(SELF:SCREEN-VALUE) * 25.

    txtImporte-3:SCREEN-VALUE = STRING(lCuantos,">,>>>,>>9").

    RUN um-suma.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtCuantos-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtCuantos-4 W-Win
ON LEAVE OF txtCuantos-4 IN FRAME F-Main
DO:
    DEFINE VAR lCuantos AS INT.

    lCuantos = INTEGER(SELF:SCREEN-VALUE) * 30.

    txtImporte-4:SCREEN-VALUE = STRING(lCuantos,">,>>>,>>9").

    RUN um-suma.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtCuantos-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtCuantos-5 W-Win
ON LEAVE OF txtCuantos-5 IN FRAME F-Main
DO:
    DEFINE VAR lCuantos AS INT.

    lCuantos = INTEGER(SELF:SCREEN-VALUE) * 50.

    txtImporte-5:SCREEN-VALUE = STRING(lCuantos,">,>>>,>>9").

    RUN um-suma.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtCuantos-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtCuantos-6 W-Win
ON LEAVE OF txtCuantos-6 IN FRAME F-Main
DO:
    DEFINE VAR lCuantos AS INT.

    lCuantos = INTEGER(SELF:SCREEN-VALUE) * 100.

    txtImporte-6:SCREEN-VALUE = STRING(lCuantos,">,>>>,>>9").

    RUN um-suma.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME txtCuantos-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL txtCuantos-7 W-Win
ON LEAVE OF txtCuantos-7 IN FRAME F-Main
DO:
    DEFINE VAR lCuantos AS INT.

    lCuantos = INTEGER(SELF:SCREEN-VALUE) * 15.

    txtImporte-7:SCREEN-VALUE = STRING(lCuantos,">,>>>,>>9").

    RUN um-suma.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Datos W-Win 
PROCEDURE Carga-Datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Tipo de fuente C39HrP48DhTt      
------------------------------------------------------------------------------*/
/*
  DEFINE VARIABLE iInt      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iLin      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cVar01    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cVar02    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cVar03    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cNroSec01 AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cNroSec02 AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cNroSec03 AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE cLlave    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cFecha    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cImpTot   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iDigVer   AS INTEGER     NO-UNDO.

  DEFINE VARIABLE cNroTck   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cNomCli   AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE cCodPrd   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cCodPrv   AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE iNroVls   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iCol      AS INTEGER     NO-UNDO.

  DEFINE VARIABLE lNroTck AS INT.

  REPEAT:
    s-task-no = RANDOM(1,999999).
    IF NOT CAN-FIND(FIRST w-report WHERE w-report.task-no = s-task-no NO-LOCK)
    THEN LEAVE.
  END.

  /*Busca Cliente*/
  cNomCli = txt-nomcli.

  /* Datos del Proveedor */
  cCodPrv = substring(cboProvProd,1,8).
  cCodPrd = substring(cboProvProd,10,4).
/*
   FIND FIRST VtaCTickets WHERE VtaCTickets.CodCia = s-codcia
      AND VtaCTickets.CodPro = txt-codpro NO-LOCK NO-ERROR.
*/      
  FIND FIRST VtaCTickets WHERE VtaCTickets.CodCia = s-codcia
     AND VtaCTickets.CodPro = cCodPrv AND VtaCTickets.Producto = cCodPrd NO-LOCK NO-ERROR.

  IF NOT AVAIL VtaCTickets THEN DO:
      MESSAGE "Producto y/o Proveedor no tiene registrado" SKIP
              "    Estructura de Ticket     "
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "adm-error".
  END.

  ASSIGN 
      cLlave  = VtaCTickets.Producto
      cFecha  = SUBSTRING(STRING(YEAR(txt-fecha),"9999"),3) + 
                STRING(MONTH(txt-fecha),"99") + STRING(DAY(txt-fecha),"99")
      cImptot = STRING((txt-nrototal * 100),"9999999").
      
  IF (txt-importe MODULO txt-nrototal) > 0 THEN DO:
      MESSAGE "Importe Total debe ser multipo de Valor Nominal"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "entry" TO txt-importe IN FRAME {&FRAME-NAME}.
      RETURN "adm-error".
  END.
  ELSE DO:
      ASSIGN 
          iLin = 1
          iCol = 1. 

      /*lNroTck = 114677.  /*114984 114969.*/*/

      DO iInt = 1 TO (txt-importe / txt-nrototal):

          lNroTck = lNroTck + 1.

          FIND LAST FacCorre WHERE 
              FacCorre.CodCia = S-CODCIA AND
              FacCorre.CodDoc = cCodDoc AND
              FacCorre.CodDiv = S-CODDIV EXCLUSIVE-LOCK NO-ERROR.
          IF AVAILABLE FacCorre THEN DO: 
              
              ASSIGN 
                  cNroTck = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
                  FacCorre.Correlativo = FacCorre.Correlativo + 1.
                           
          END.
          cNroTck = "000" + STRING(lNroTck,"999999").
          cVar01 = cLlave + cFecha + cNroTck + cImptot.
          
          RUN vtagn\edenred-01 (cVar01,OUTPUT iDigVer).
          
          cVar01 = cVar01 + STRING(iDigVer,"9") + "0".
          cNroSec01 = cNroTck.

          IF ((iInt - 1) MODULO 3) = 0 THEN
              ASSIGN 
                iLin = iLin + 1
                iCol = 1.
            
          FIND FIRST w-report WHERE w-report.task-no = s-task-no 
              AND w-report.llave-i = iLin NO-ERROR.
          IF NOT AVAIL w-report THEN DO:
              CREATE w-report.
              ASSIGN
                  task-no    = s-task-no
                  llave-i    = iLin
                  campo-c[5] = cNomCli
                  campo-d[1] = txt-fecha            /*Fecha Vencimiento*/
                  campo-f[1] = txt-nrototal.         /*Importe Vale*/ 
          END.
          campo-c[14 + iCol] = cVar01.         /* Cod Barra antes de la encriptacion */

          RUN lib\_strto128c(cVar01, OUTPUT cVar01).
          
          ASSIGN 
              campo-c[iCol] = cVar01
              campo-i[iCol] = INTEGER(cNroSec01)
              iCol          = iCol + 1.      
      END.
  END.

  

  /***
  DO iInt = 1 TO INT(txt-nrototal) :
      /*Primera Parte */
      FIND LAST FacCorre WHERE 
          FacCorre.CodCia = S-CODCIA AND
          FacCorre.CodDoc = cCodDoc AND
          FacCorre.CodDiv = S-CODDIV EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE FacCorre THEN DO: 
          ASSIGN 
              cNroTck = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
              FacCorre.Correlativo = FacCorre.Correlativo + 1.
      END.
      cVar01 = cLlave + cFecha + cNroTck + cImptot.
      RUN vtagn\edenred-01 (cVar01,OUTPUT iDigVer).
      cVar01 = cVar01 + STRING(iDigVer,"9") + "0".
      cNroSec01 = cNroTck.

      /*Segunda Parte */
      FIND LAST FacCorre WHERE 
          FacCorre.CodCia = S-CODCIA AND
          FacCorre.CodDoc = cCodDoc AND
          FacCorre.CodDiv = S-CODDIV EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE FacCorre THEN DO: 
          ASSIGN 
              cNroTck = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
              FacCorre.Correlativo = FacCorre.Correlativo + 1.
      END.
      iInt = iInt + 1.
      cVar02 = cLlave + cFecha + cNroTck + cImptot.
      RUN vtagn\edenred-01 (cVar02,OUTPUT iDigVer).
      cVar02 = cVar02 + STRING(iDigVer,"9") + "0".
      cNroSec02 = cNroTck.


      /*Tercera Parte */
      FIND LAST FacCorre WHERE 
          FacCorre.CodCia = S-CODCIA AND
          FacCorre.CodDoc = cCodDoc AND
          FacCorre.CodDiv = S-CODDIV EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE FacCorre THEN DO: 
          ASSIGN 
              cNroTck = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
              FacCorre.Correlativo = FacCorre.Correlativo + 1.
      END.
      iInt = iInt + 1.
      cVar03 = cLlave + cFecha + cNroTck + cImptot.
      RUN vtagn\edenred-01 (cVar03,OUTPUT iDigVer).
      cVar03 = cVar03 + STRING(iDigVer,"9") + "0".
      cNroSec03 = cNroTck.


      RUN lib\_strto128c(cVar01, OUTPUT cVar01).
      RUN lib\_strto128c(cVar02, OUTPUT cVar02).
      RUN lib\_strto128c(cVar03, OUTPUT cVar03).

      FIND FIRST w-report WHERE w-report.task-no = s-task-no 
          AND w-report.llave-i = iLin NO-LOCK NO-ERROR.
      IF NOT AVAIL w-report THEN DO:
          CREATE w-report.
          ASSIGN
              task-no    = s-task-no
              llave-i    = iLin
              campo-c[1] = cVar01
              campo-i[1] = INTEGER(cNroSec01)
              campo-c[2] = cVar02
              campo-i[2] = INTEGER(cNroSec02)
              campo-c[3] = cVar03
              campo-i[3] = INTEGER(cNroSec03)
              campo-c[5] = cNomCli
              campo-d[1] = txt-fecha            /*Fecha Vencimiento*/
              campo-f[1] = txt-importe.         /*Importe Vale*/ 
      END.
      iLin = iLin + 1.
  END.
         
         *****/   
*/         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cargar-vales W-Win 
PROCEDURE Cargar-vales :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Tipo de fuente C39HrP48DhTt      
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cVar01    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cVar02    AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cVar03    AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE cNroTck   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cNomCli   AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE cCodPrd   AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cCodPrv   AS CHARACTER   NO-UNDO.  

  REPEAT:
    s-task-no = RANDOM(1,999999).
    IF NOT CAN-FIND(FIRST w-report WHERE w-report.task-no = s-task-no NO-LOCK)
    THEN LEAVE.
  END.

  /*Busca Cliente*/
  cNomCli = txt-nomcli.

  /* Datos del Proveedor */
  cCodPrv = substring(cboProvProd,1,8).
  cCodPrd = substring(cboProvProd,10,4).

  FIND FIRST VtaCTickets WHERE VtaCTickets.CodCia = s-codcia
     AND VtaCTickets.CodPro = cCodPrv AND VtaCTickets.Producto = cCodPrd NO-LOCK NO-ERROR.

  IF NOT AVAIL VtaCTickets THEN DO:
      MESSAGE "Producto y/o Proveedor no tiene registrado" SKIP
              "    Estructura de Ticket     "
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "adm-error".
  END.

  /*  Inicio  */
    iLin = 1.
    iCol = 1.
    IF txtCuantos > 0 THEN RUN crear-etiquetas (INPUT txtCuantos, INPUT 10).    /*10*/
    IF txtCuantos-7 > 0 THEN RUN crear-etiquetas (INPUT txtCuantos-7, INPUT 15).
    IF txtCuantos-2 > 0 THEN RUN crear-etiquetas (INPUT txtCuantos-2, INPUT 20). /*20*/
    IF txtCuantos-3 > 0 THEN RUN crear-etiquetas (INPUT txtCuantos-3, INPUT 25). /*25*/
    IF txtCuantos-4 > 0 THEN RUN crear-etiquetas (INPUT txtCuantos-4, INPUT 30).
    IF txtCuantos-5 > 0 THEN RUN crear-etiquetas (INPUT txtCuantos-5, INPUT 50).
    IF txtCuantos-6 > 0 THEN RUN crear-etiquetas (INPUT txtCuantos-6, INPUT 100).
    

    RUN crear-etiquetas (INPUT 0, INPUT 0).

/**/
    DEFINE VAR sRegActual AS ROWID.

    ASSIGN sRegActual = ROWID(vtatabla).
    DEF BUFFER B-vtatabla FOR vtatabla.

    FIND B-vtatabla WHERE ROWID(B-vtatabla) = sRegActual EXCLUSIVE NO-ERROR.
    IF AVAILABLE B-vtatabla THEN DO:
        ASSIGN b-vtatabla.libre_c03  = 'IMPRESO'.
    END.
    RELEASE b-vtatabla.


        {&OPEN-QUERY-BROWSE-2}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE chr_to_asc W-Win 
PROCEDURE chr_to_asc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER tcCodBarra AS CHAR.
DEF OUTPUT PARAMETER lcRetASc AS CHAR.
    
DEFINE VAR lLen AS INT.
DEFINE VAR lValores AS CHAR.
DEFINE VAR lChar AS CHAR.
DEFINE VAR lAsc AS INT.
DEFINE VAR lStr AS CHAR.
DEFINE VAR i AS INT.
DEFINE VAR lGuion AS CHAR.

lLen = LENGTH(tcCodBarra).
lGuion = "".
lValores = "".
REPEAT i = 1 TO lLen :
    lChar = SUBSTRING(tcCodBarra,i,1).
    lAsc = ASC(lChar).
    lStr = STRING(lAsc).
    lValores = lValores + lGuion + lStr.
    lGuion = "_".
END.

lcRetASc = lValores.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crear-etiquetas W-Win 
PROCEDURE crear-etiquetas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pCuantos AS INT   NO-UNDO.
DEFINE INPUT PARAMETER pDeCuanto AS DEC  NO-UNDO.

DEFINE VARIABLE cLlave    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFecha    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cImpTot   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iDigVer   AS INTEGER     NO-UNDO.

DEFINE VARIABLE cNroTck   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVar01    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNroSec01 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNomCli   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cCodPrd   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCodPrv   AS CHARACTER   NO-UNDO.

DEFINE VARIABLE iInt   AS INTEGER     NO-UNDO.

DEFINE VARIABLE lNroTck AS INT.
DEFINE VAR lInicio AS LOGICAL.

/**/
DEFINE VAR cNumValeInicio AS CHAR.
DEFINE VAR cNumValeTermino AS CHAR.

DEFINE VAR iNumValeInicio AS INT.
DEFINE VAR iNumValeTermino AS INT.

/* */
DEF BUFFER i-vtatabla FOR vtatabla.
/*
DISABLE TRIGGERS FOR LOAD OF vtatabla.
DISABLE TRIGGERS FOR LOAD OF i-vtatabla.
*/
DEFINE VAR lTck AS CHAR.

SESSION:SET-WAIT-STATE('GENERAL').

ASSIGN 
      cLlave  = VtaCTickets.Producto
      cFecha  = SUBSTRING(STRING(YEAR(txt-fecha),"9999"),3) + 
                STRING(MONTH(txt-fecha),"99") + STRING(DAY(txt-fecha),"99")
      /*cImptot = STRING((txt-nrototal * 100),"9999999").*/
      cImptot = STRING((pDeCuanto * 100),"9999999").

  /*Busca Cliente*/
  cNomCli = txt-nomcli.

/* Por el TIcket de INICIO */
    IF ((iCol - 1) MODULO 3) = 0 THEN DO :
        ASSIGN iLin = iLin + 1
                 iCol = 1.
    END.
    FIND FIRST w-report WHERE w-report.task-no = s-task-no 
         AND w-report.llave-i = iLin NO-ERROR.
    IF NOT AVAIL w-report THEN DO:
        CREATE w-report.
        ASSIGN
            task-no    = s-task-no
            llave-i    = iLin.
    END.
    campo-c[5] = cNomCli.
    campo-d[iCol] = txt-fecha.           /*Fecha Vencimiento*/
    campo-f[iCol] = pDeCuanto.           /*Importe Vale*/
    campo-c[14 + iCol] = " " .      /* Cod Barra antes de la encriptacion */
    ASSIGN 
         campo-c[iCol] = ""
         campo-i[iCol] = 0 
         campo-c[27 + iCol] = "".

    iCol          = iCol + 1.      

/* -------------------------- */
cNumValeInicio = "".
cNumValeTermino = "".

DO iInt = 1 TO pCuantos :
          
    FIND LAST FacCorre WHERE 
              FacCorre.CodCia = S-CODCIA AND
              FacCorre.CodDoc = cCodDoc AND
              FacCorre.CodDiv = S-CODDIV EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE FacCorre THEN DO:  
        
        ASSIGN 
             cNroTck = STRING(FacCorre.NroSer,"999") + STRING(FacCorre.Correlativo,"999999")
             FacCorre.Correlativo = FacCorre.Correlativo + 1.       

             IF (cNumValeInicio = "") THEN cNumValeInicio = cNroTck.
             cNumValeTermino = cNroTck.
    END.
    
    cVar01 = cLlave + cFecha + cNroTck + cImptot.
    
    RUN vtagn\edenred-01.r (INPUT cVar01,OUTPUT iDigVer).
          
    /* MESSAGE cVar01 iDigVer. */

    cVar01 = cVar01 + STRING(iDigVer,"9") + "0".
    cNroSec01 = cNroTck.
    
    IF ((iCol - 1) MODULO 3) = 0 THEN DO :
        ASSIGN iLin = iLin + 1
                 iCol = 1.
    END.
    FIND FIRST w-report WHERE w-report.task-no = s-task-no 
         AND w-report.llave-i = iLin NO-ERROR.
    IF NOT AVAIL w-report THEN DO:
        CREATE w-report.
        ASSIGN
            task-no    = s-task-no
            llave-i    = iLin.
    END.
    ASSIGN 
            campo-c[5] = cNomCli
            campo-d[iCol] = txt-fecha            /*Fecha Vencimiento*/
            campo-f[iCol] = pDeCuanto.          /*Importe Vale*/     
            campo-c[14 + iCol] = cVar01.         /* Cod Barra antes de la encriptacion */
   
            lTck = cVar01.
   
    RUN lib\_strto128c(cVar01, OUTPUT cVar01).
          
    ASSIGN 
         campo-c[iCol] = cVar01
         campo-i[iCol] = INTEGER(cNroSec01)
         campo-c[27 + iCol] = "" /*cVar01 Codigo de Barra Encriptada  */.
         iCol          = iCol + 1.     
    
    /* Grabar los Tickets */
    FIND FIRST i-vtatabla WHERE i-vtatabla.codcia = s-codcia AND 
                                i-vtatabla.tabla = 'VUTILEXTCK' AND
                                i-vtatabla.llave_c1 = lTck NO-LOCK NO-ERROR.
    IF NOT AVAILABLE i-vtatabla THEN DO:
        CREATE i-vtatabla.
            ASSIGN i-vtatabla.codcia = s-codcia
                    i-vtatabla.tabla = 'VUTILEXTCK'
                    i-vtatabla.llave_c1 = lTck                 /* Codigo de Barra */
                    i-vtatabla.llave_c2 = vtatabla.llave_c2     /* Proveedor */
                    i-vtatabla.llave_c3 = vtatabla.llave_c3     /* Producto */
                    i-vtatabla.llave_c4 = cImpTot               /* Denominacion */
                    i-vtatabla.llave_c5 = cNroTck                 /* Nro del Vale */
                    i-vtatabla.llave_c6 = vtatabla.llave_c1.     /*Control*/

    END.

END.

/* Grabo el Rango de Tickets */
IF cNumValeInicio <> "" THEN DO:
    CREATE b-vtatabla.
    BUFFER-COPY vtatabla TO b-vtatabla.
    ASSIGN  b-vtatabla.llave_c4 = cImpTot
            b-vtatabla.llave_c5 = cNumValeInicio
            b-vtatabla.llave_c6 = cNumValeTermino
            b-vtatabla.llave_c7 = STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS")
            /*b-vtatabla.llave_c8 = STRING(pRCID,"999999999999")*/
            b-vtatabla.libre_c03 = 'DETALLE'
            b-vtatabla.rango_valor[1] = INT(cNumValeInicio)
            b-vtatabla.rango_valor[2] = INT(cNumValeTermino).
END.

SESSION:SET-WAIT-STATE('').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY txt-codpro cboProvProd txt-fecha txtProd txt-codcli txt-nomcli 
          txtImporte txtCuantos txtCuantos-7 txtImporte-7 txtCuantos-2 
          txtImporte-2 txtImporte-3 txtCuantos-3 txtImporte-4 txtCuantos-4 
          txtImporte-5 txtCuantos-5 txtImporte-6 txtCuantos-6 txtTotal 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BUTTON-1 BROWSE-2 txt-codcli 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir W-Win 
PROCEDURE Imprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*RUN Carga-Datos.*/
  RUN Cargar-vales.

  IF RETURN-VALUE = 'adm-error' THEN RETURN "adm-error".
/*

  GET-KEY-VALUE SECTION 'STARTUP' KEY 'BASE' VALUE RB-REPORT-LIBRARY.
  ASSIGN
    RB-REPORT-LIBRARY = RB-REPORT-LIBRARY + 'vta\rbvta.prl'       
    RB-REPORT-NAME = 'Vales Utilex 02 Prueba'       /* A LA MONEDA DEL DOCUMENTO */
    RB-INCLUDE-RECORDS = 'O'
    RB-FILTER = 'w-report.task-no = ' + STRING(s-task-no).

/*     RB-OTHER-PARAMETERS = 's-nomcia = ' + s-nomcia +                  */
/*                             '~ns-codcli = ' + gn-clie.codcli +        */
/*                              '~ns-nomcli = ' + gn-clie.nomcli +       */
/*                              '~np-saldo-mn = ' + STRING(x-saldo-mn) + */
/*                              '~np-saldo-me = ' + STRING(x-saldo-me).  */


  RUN lib/_imprime2 (RB-REPORT-LIBRARY,
                    RB-REPORT-NAME,
                    RB-INCLUDE-RECORDS,
                    RB-FILTER,
                    RB-OTHER-PARAMETERS).

  /*RUN to_excel (s-task-no).*/
  
*/

  FOR EACH w-report WHERE task-no = s-task-no:
      DELETE w-report.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME} :
      ASSIGN txt-fecha = TODAY.
      DISPLAY txt-fecha.

  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros W-Win 
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
        WHEN "" THEN .
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros W-Win 
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
        WHEN "" THEN ASSIGN input-var-1 = "".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "VtaTabla"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE to_excel W-Win 
PROCEDURE to_excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER tcTask AS INT.

DEFINE VAR lCodBarra AS CHAR.
DEFINE VAR lCodBarraAsc AS CHAR.

FOR EACH w-report WHERE task-no = tcTask:    
    lCodBarra = w-report.campo-c[1].
    RUN chr_to_asc(lCodBarra,OUTPUT  lCodBarraAsc).
    ASSIGN w-report.campo-c[11]=lCodBarraAsc.
    /**/
    lCodBarra = w-report.campo-c[2].
    RUN chr_to_asc(lCodBarra,OUTPUT  lCodBarraAsc).
    ASSIGN w-report.campo-c[12]=lCodBarraAsc.
    /**/
    lCodBarra = w-report.campo-c[3].
    RUN chr_to_asc(lCodBarra,OUTPUT  lCodBarraAsc).
    ASSIGN w-report.campo-c[13]=lCodBarraAsc.

END.

/* A excel */
        DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
        DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
        DEFINE VARIABLE chWorksheet             AS COM-HANDLE.

        DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.

        DEFINE VARIABLE iCount                  AS INTEGER init 1.
        DEFINE VARIABLE iIndex                  AS INTEGER.
        DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 1.
        DEFINE VARIABLE cColumn                 AS CHARACTER.
        DEFINE VARIABLE cRange                  AS CHARACTER.
        DEFINE VARIABLE x-signo                 AS DECI.

        /* create a new Excel Application object */
        CREATE "Excel.Application" chExcelApplication.

        /* launch Excel so it is visible to the user */
        chExcelApplication:Visible = TRUE.

        /* Para crear a new Workbook */
        chWorkbook = chExcelApplication:Workbooks:Add().

    /* get the active Worksheet */
        chWorkSheet = chExcelApplication:Sheets:Item(1).

iColumn = 1.
FOR EACH w-report WHERE task-no = tcTask:    
     iColumn = iColumn + 1.
     cColumn = STRING(iColumn).

    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + w-report.campo-c[15].

    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + w-report.campo-c[1].

    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + w-report.campo-c[11].

    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + w-report.campo-c[16].

    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + w-report.campo-c[2].

    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + w-report.campo-c[12].

    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + w-report.campo-c[17].

    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + w-report.campo-c[3].

    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + w-report.campo-c[13].

END.

    chWorkSheet:SaveAs("c:\ciman\tickes_veri.xls").
        chExcelApplication:DisplayAlerts = False.
        chExcelApplication:Quit().

        /* release com-handles */
        RELEASE OBJECT chExcelApplication NO-ERROR.      
        RELEASE OBJECT chWorkbook NO-ERROR.
        RELEASE OBJECT chWorksheet NO-ERROR.
        RELEASE OBJECT chWorksheetRange NO-ERROR. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE um-suma W-Win 
PROCEDURE um-suma :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR lSuma AS DEC.
DO WITH FRAME {&FRAME-NAME}:
    lSuma = lSuma + INTEGER(txtImporte:SCREEN-VALUE).
    lSuma = lSuma + INTEGER(txtImporte-2:SCREEN-VALUE).
    lSuma = lSuma + INTEGER(txtImporte-3:SCREEN-VALUE).
    lSuma = lSuma + INTEGER(txtImporte-4:SCREEN-VALUE).
    lSuma = lSuma + INTEGER(txtImporte-5:SCREEN-VALUE).
    lSuma = lSuma + INTEGER(txtImporte-6:SCREEN-VALUE).
    lSuma = lSuma + INTEGER(txtImporte-7:SCREEN-VALUE).

    txtTotal:SCREEN-VALUE = STRING(lSuma,">>>,>>>,>>9").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

