&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T-MATG NO-UNDO LIKE Almmmatg.



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
DEF SHARED VAR s-user-id AS CHAR.
DEF SHARED VAR pv-codcia AS INT.

&SCOPED-DEFINE Condicion Almmmatg.codcia = s-codcia ~
AND Almmmatg.TpoArt <> "D" ~
AND (COMBO-BOX-Linea = 'Todas' OR Almmmatg.codfam = ENTRY(1, COMBO-BOX-Linea, ' - ') ) ~
AND (COMBO-BOX-SubLinea = 'Todas' OR Almmmatg.subfam = ENTRY(1, COMBO-BOX-SubLinea, ' - ') ) ~
AND (FILL-IN-CodPro = "" OR Almmmatg.CodPr1 = FILL-IN-CodPro) ~
AND (FILL-IN-DesMat = "" OR INDEX(Almmmatg.desmat, FILL-IN-DesMat) > 0)

DEFINE VARIABLE fmot LIKE T-MATG.PreOfi.
DEFINE VARIABLE pre-ofi LIKE T-MATG.PreOfi.
DEFINE VARIABLE MrgMin LIKE T-MATG.MrgUti-A.
DEFINE VARIABLE MrgOfi LIKE T-MATG.MrgUti-A.
DEFINE VARIABLE MaxCat LIKE ClfClie.PorDsc.
DEFINE VARIABLE MaxVta LIKE Dsctos.PorDto.
DEFINE VARIABLE F-FACTOR AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreVta-A AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreVta-B AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreVta-C AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-MrgUti-A AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-MrgUti-B AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-MrgUti-C AS DECIMAL NO-UNDO.
DEFINE VARIABLE X-CTOUND AS DECIMAL NO-UNDO.
DEFINE VARIABLE X-CTOTOT AS DECIMAL FORMAT "->>>>>>>>>9.999999" NO-UNDO.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

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
&Scoped-define FIELDS-IN-QUERY-br_table T-MATG.codmat T-MATG.DesMat ~
T-MATG.UndStk T-MATG.MonVta T-MATG.TpoCmb T-MATG.DesMar T-MATG.CtoTot ~
T-MATG.CtoTotMarco T-MATG.Prevta[1] T-MATG.MrgUti-A T-MATG.Prevta[2] ~
T-MATG.UndA T-MATG.MrgUti-B T-MATG.Prevta[3] T-MATG.UndB T-MATG.MrgUti-C ~
T-MATG.Prevta[4] T-MATG.UndC T-MATG.Dec__01 T-MATG.PreOfi T-MATG.Chr__01 ~
T-MATG.MrgAlt[1] T-MATG.PreAlt[1] T-MATG.UndAlt[1] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table T-MATG.MonVta ~
T-MATG.Prevta[1] T-MATG.MrgUti-A T-MATG.Prevta[2] T-MATG.MrgUti-B ~
T-MATG.Prevta[3] T-MATG.MrgUti-C T-MATG.Prevta[4] T-MATG.MrgAlt[1] ~
T-MATG.PreAlt[1] 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table T-MATG
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table T-MATG
&Scoped-define QUERY-STRING-br_table FOR EACH T-MATG WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH Almmmatg OF T-MATG NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH T-MATG WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH Almmmatg OF T-MATG NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table T-MATG Almmmatg
&Scoped-define FIRST-TABLE-IN-QUERY-br_table T-MATG
&Scoped-define SECOND-TABLE-IN-QUERY-br_table Almmmatg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS COMBO-BOX-Linea BUTTON-8 COMBO-BOX-Sublinea ~
FILL-IN-CodMat FILL-IN-CodPro FILL-IN-DesMat br_table 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-Linea COMBO-BOX-Sublinea ~
FILL-IN-CodMat FILL-IN-CodPro FILL-IN-NomPro FILL-IN-DesMat 

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


/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-br_table 
       MENU-ITEM m_LIMA_-_Descuento_Promociona LABEL "LIMA - Descuento Promocional"
       MENU-ITEM m_LIMA_-_Descuento_por_Volume LABEL "LIMA - Descuento por Volumen"
       MENU-ITEM m_UTILEX_-_Descuento_Promocio LABEL "UTILEX - Descuento Promocional"
       MENU-ITEM m_UTILEX_-_Descuento_por_Volu LABEL "UTILEX - Descuento por Volumen"
       MENU-ITEM m_PROVINCIAS_-_Descuento_Prom LABEL "PROVINCIAS - Descuento Promocional 3ros"
       MENU-ITEM m_PROVINCIAS_-_Descuento_Prom2 LABEL "PROVINCIAS - Descuento Promocional Propios".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-8 
     LABEL "APLICAR FILTRO" 
     SIZE 18 BY 1.12
     FONT 6.

DEFINE VARIABLE COMBO-BOX-Linea AS CHARACTER FORMAT "X(256)":U INITIAL "Todas" 
     LABEL "Linea" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "TODAS" 
     DROP-DOWN-LIST
     SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-Sublinea AS CHARACTER FORMAT "X(256)":U INITIAL "TODAS" 
     LABEL "Sublinea" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "TODAS" 
     DROP-DOWN-LIST
     SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-CodMat AS CHARACTER FORMAT "X(13)":U 
     LABEL "Art�culo" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-CodPro AS CHARACTER FORMAT "X(8)":U 
     LABEL "Proveedor" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-DesMat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descripci�n" 
     VIEW-AS FILL-IN 
     SIZE 55 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-NomPro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY .81 NO-UNDO.

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
      T-MATG.codmat COLUMN-LABEL "Articulo" FORMAT "X(6)":U WIDTH 6.43
            COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 14
      T-MATG.DesMat FORMAT "X(60)":U
      T-MATG.UndStk COLUMN-LABEL "Unidad" FORMAT "X(6)":U
      T-MATG.MonVta COLUMN-LABEL "Moneda" FORMAT "9":U VIEW-AS COMBO-BOX INNER-LINES 5
                      LIST-ITEM-PAIRS "Soles",1,
                                      "Dolares",2
                      DROP-DOWN-LIST 
      T-MATG.TpoCmb COLUMN-LABEL "TC" FORMAT "Z9.9999":U
      T-MATG.DesMar COLUMN-LABEL "Marca" FORMAT "X(20)":U
      T-MATG.CtoTot COLUMN-LABEL "Costo Total!S/." FORMAT ">>>,>>9.9999":U
            COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 14
      T-MATG.CtoTotMarco COLUMN-LABEL "Costo MARCO!S/." FORMAT ">>>,>>9.9999":U
            COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 8
      T-MATG.Prevta[1] COLUMN-LABEL "Precio Lista!S/." FORMAT ">>,>>>,>>9.9999":U
            WIDTH 9.43 COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 14
      T-MATG.MrgUti-A COLUMN-LABEL "% Uti A" FORMAT "->>,>>9.99":U
            COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 11
      T-MATG.Prevta[2] COLUMN-LABEL "Precio A!S/." FORMAT ">>>,>>9.9999":U
            COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 11
      T-MATG.UndA COLUMN-LABEL "UM A" FORMAT "X(6)":U COLUMN-FGCOLOR 0 COLUMN-BGCOLOR 11
      T-MATG.MrgUti-B COLUMN-LABEL "% Uti B" FORMAT "->>,>>9.99":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 13
      T-MATG.Prevta[3] COLUMN-LABEL "Precio B!S/." FORMAT ">>>,>>9.9999":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 13
      T-MATG.UndB COLUMN-LABEL "UM B" FORMAT "X(6)":U COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 13
      T-MATG.MrgUti-C COLUMN-LABEL "% Uti C" FORMAT "->>,>>9.99":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 1
      T-MATG.Prevta[4] COLUMN-LABEL "Precio C!S/." FORMAT ">>>,>>9.9999":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 1
      T-MATG.UndC COLUMN-LABEL "UM C" FORMAT "X(6)":U COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 1
      T-MATG.Dec__01 COLUMN-LABEL "% Uti Ofi" FORMAT "->>,>>9.99":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 12
      T-MATG.PreOfi COLUMN-LABEL "Precio Oficina!S/." FORMAT ">>>,>>9.9999":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 12
      T-MATG.Chr__01 COLUMN-LABEL "UM Ofic" FORMAT "X(6)":U COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 12
      T-MATG.MrgAlt[1] COLUMN-LABEL "UTILEX!% Uti" FORMAT "->>,>>9.99":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 2
      T-MATG.PreAlt[1] COLUMN-LABEL "UTILEX!Precio S/." FORMAT ">>>,>>9.9999":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 2
      T-MATG.UndAlt[1] COLUMN-LABEL "UTILEX!UM" FORMAT "x(6)":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 2
  ENABLE
      T-MATG.MonVta
      T-MATG.Prevta[1]
      T-MATG.MrgUti-A
      T-MATG.Prevta[2]
      T-MATG.MrgUti-B
      T-MATG.Prevta[3]
      T-MATG.MrgUti-C
      T-MATG.Prevta[4]
      T-MATG.MrgAlt[1]
      T-MATG.PreAlt[1]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 142 BY 20.96
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     COMBO-BOX-Linea AT ROW 1.19 COL 12 COLON-ALIGNED WIDGET-ID 2
     BUTTON-8 AT ROW 1.19 COL 124 WIDGET-ID 10
     COMBO-BOX-Sublinea AT ROW 2.15 COL 12 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-CodMat AT ROW 2.15 COL 79 COLON-ALIGNED WIDGET-ID 18
     FILL-IN-CodPro AT ROW 3.12 COL 12 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-NomPro AT ROW 3.12 COL 24 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     FILL-IN-DesMat AT ROW 3.12 COL 79 COLON-ALIGNED WIDGET-ID 12
     br_table AT ROW 4.27 COL 1
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
      TABLE: T-MATG T "?" NO-UNDO INTEGRAL Almmmatg
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
         HEIGHT             = 24.35
         WIDTH              = 142.
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
/* BROWSE-TAB br_table FILL-IN-DesMat F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:POPUP-MENU IN FRAME F-Main             = MENU POPUP-MENU-br_table:HANDLE
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 5.

/* SETTINGS FOR FILL-IN FILL-IN-NomPro IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "Temp-Tables.T-MATG,INTEGRAL.Almmmatg OF Temp-Tables.T-MATG"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > Temp-Tables.T-MATG.codmat
"T-MATG.codmat" "Articulo" ? "character" 14 0 ? ? ? ? no ? no no "6.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.T-MATG.DesMat
"T-MATG.DesMat" ? "X(60)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.T-MATG.UndStk
"T-MATG.UndStk" "Unidad" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.T-MATG.MonVta
"T-MATG.MonVta" "Moneda" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "DROP-DOWN-LIST" "," ? "Soles,1,Dolares,2" 5 no 0 no no
     _FldNameList[5]   > Temp-Tables.T-MATG.TpoCmb
"T-MATG.TpoCmb" "TC" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.T-MATG.DesMar
"T-MATG.DesMar" "Marca" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.T-MATG.CtoTot
"T-MATG.CtoTot" "Costo Total!S/." ">>>,>>9.9999" "decimal" 14 0 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.T-MATG.CtoTotMarco
"T-MATG.CtoTotMarco" "Costo MARCO!S/." ">>>,>>9.9999" "decimal" 8 0 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.T-MATG.Prevta[1]
"T-MATG.Prevta[1]" "Precio Lista!S/." ? "decimal" 14 0 ? ? ? ? yes ? no no "9.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.T-MATG.MrgUti-A
"T-MATG.MrgUti-A" "% Uti A" ? "decimal" 11 0 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.T-MATG.Prevta[2]
"T-MATG.Prevta[2]" "Precio A!S/." ">>>,>>9.9999" "decimal" 11 0 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.T-MATG.UndA
"T-MATG.UndA" "UM A" "X(6)" "character" 11 0 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.T-MATG.MrgUti-B
"T-MATG.MrgUti-B" "% Uti B" ? "decimal" 13 15 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.T-MATG.Prevta[3]
"T-MATG.Prevta[3]" "Precio B!S/." ">>>,>>9.9999" "decimal" 13 15 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.T-MATG.UndB
"T-MATG.UndB" "UM B" "X(6)" "character" 13 15 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.T-MATG.MrgUti-C
"T-MATG.MrgUti-C" "% Uti C" ? "decimal" 1 15 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.T-MATG.Prevta[4]
"T-MATG.Prevta[4]" "Precio C!S/." ">>>,>>9.9999" "decimal" 1 15 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Temp-Tables.T-MATG.UndC
"T-MATG.UndC" "UM C" "X(6)" "character" 1 15 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > Temp-Tables.T-MATG.Dec__01
"T-MATG.Dec__01" "% Uti Ofi" "->>,>>9.99" "decimal" 12 15 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > Temp-Tables.T-MATG.PreOfi
"T-MATG.PreOfi" "Precio Oficina!S/." ">>>,>>9.9999" "decimal" 12 15 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > Temp-Tables.T-MATG.Chr__01
"T-MATG.Chr__01" "UM Ofic" "X(6)" "character" 12 15 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > Temp-Tables.T-MATG.MrgAlt[1]
"T-MATG.MrgAlt[1]" "UTILEX!% Uti" ? "decimal" 2 15 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > Temp-Tables.T-MATG.PreAlt[1]
"T-MATG.PreAlt[1]" "UTILEX!Precio S/." ">>>,>>9.9999" "decimal" 2 15 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > Temp-Tables.T-MATG.UndAlt[1]
"T-MATG.UndAlt[1]" "UTILEX!UM" "x(6)" "character" 2 15 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define SELF-NAME T-MATG.Prevta[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-MATG.Prevta[1] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-MATG.Prevta[1] IN BROWSE br_table /* Precio Lista!S/. */
DO:
    RUN Precio-de-Oficina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-MATG.MrgUti-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-MATG.MrgUti-A br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-MATG.MrgUti-A IN BROWSE br_table /* % Uti A */
DO:
    ASSIGN
        F-MrgUti-A = DECIMAL(T-MATG.MrgUti-A:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
        X-CTOUND   = DECIMAL(T-MATG.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

     F-FACTOR = 1.
     F-PreVta-A = 0.
     /****   Busca el Factor de conversion   ****/
     IF T-MATG.UndA <> "" THEN DO:
         FIND Almtconv WHERE Almtconv.CodUnid = T-MATG.UndBas
             AND  Almtconv.Codalter = T-MATG.UndA
             NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Almtconv THEN DO:
             MESSAGE "Codigo de unidad no existe" VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
         END.
         F-FACTOR = Almtconv.Equival.
         F-PreVta-A = ROUND(( X-CTOUND * (1 + F-MrgUti-A / 100) ), 6) * F-FACTOR.
     END.

    DISPLAY F-PreVta-A @ Prevta[2] WITH BROWSE {&BROWSE-NAME}.

    RUN Precio-de-Oficina.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-MATG.Prevta[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-MATG.Prevta[2] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-MATG.Prevta[2] IN BROWSE br_table /* Precio A!S/. */
DO:
    ASSIGN
        F-PreVta-A = DECIMAL(T-MATG.Prevta[2]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
        X-CTOUND = DECIMAL(T-MATG.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    F-FACTOR = 1.
    F-MrgUti-A = 0.    
    /****   Busca el Factor de conversion   ****/
    IF T-MATG.UndA <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = T-MATG.UndBas
            AND  Almtconv.Codalter = T-MATG.UndA
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
            MESSAGE "Codigo de unidad no existe" VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
        F-FACTOR = Almtconv.Equival.
        F-MrgUti-A = ROUND(((((F-PreVta-A / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
    END.
    /*******************************************/
    DISPLAY F-MrgUti-A @ T-MATG.MrgUti-A WITH BROWSE {&BROWSE-NAME}.
    RUN Precio-de-Oficina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-MATG.MrgUti-B
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-MATG.MrgUti-B br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-MATG.MrgUti-B IN BROWSE br_table /* % Uti B */
DO:
    ASSIGN
        F-MrgUti-B = DECIMAL(T-MATG.MrgUti-B:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
        X-CTOUND   = DECIMAL(T-MATG.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

     F-FACTOR = 1.
     F-Prevta-B = 0.
     /****   Busca el Factor de conversion   ****/
     IF T-MATG.UndB <> "" THEN DO:
         FIND Almtconv WHERE Almtconv.CodUnid = T-MATG.UndBas
                        AND  Almtconv.Codalter = T-MATG.UndB
                       NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Almtconv THEN DO:
            MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
         END.
         F-FACTOR = Almtconv.Equival.
         F-PreVta-B = ROUND(( X-CTOUND * (1 + F-MrgUti-B / 100) ), 6) * F-FACTOR.
     END.
    DISPLAY F-PreVta-B @ Prevta[3]
            WITH BROWSE {&BROWSE-NAME}.

    RUN Precio-de-Oficina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-MATG.Prevta[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-MATG.Prevta[3] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-MATG.Prevta[3] IN BROWSE br_table /* Precio B!S/. */
DO:
    ASSIGN
        F-PreVta-B = DECIMAL(T-MATG.Prevta[3]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
        X-CTOUND = DECIMAL(T-MATG.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).


     F-FACTOR = 1.
     F-MrgUti-B = 0.   
     /****   Busca el Factor de conversion   ****/
     IF T-MATG.UndB <> "" THEN DO:
         FIND Almtconv WHERE Almtconv.CodUnid = T-MATG.UndBas
                        AND  Almtconv.Codalter = T-MATG.UndB
                       NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Almtconv THEN DO:
            MESSAGE "Codigo de unidad no existe" VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
         END.
         F-FACTOR = Almtconv.Equival.
         F-MrgUti-B = ROUND(((((F-PreVta-B / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
     END.
     /*******************************************/


     DISPLAY F-MrgUti-B @ T-MATG.MrgUti-B
             WITH BROWSE {&BROWSE-NAME}.

     RUN Precio-de-Oficina.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-MATG.MrgUti-C
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-MATG.MrgUti-C br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-MATG.MrgUti-C IN BROWSE br_table /* % Uti C */
DO:
    ASSIGN
        F-MrgUti-C = DECIMAL(T-MATG.MrgUti-C:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
        X-CTOUND   = DECIMAL(T-MATG.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

     F-FACTOR = 1.
     F-Prevta-C = 0.
     /****   Busca el Factor de conversion   ****/
     IF T-MATG.UndC <> "" THEN DO:
         FIND Almtconv WHERE Almtconv.CodUnid = T-MATG.UndBas
                        AND  Almtconv.Codalter = T-MATG.UndC
                       NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Almtconv THEN DO:
            MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
         END.
         F-FACTOR = Almtconv.Equival.
         F-PreVta-C = ROUND(( X-CTOUND * (1 + F-MrgUti-C / 100) ), 6) * F-FACTOR.
     END.




    DISPLAY F-PreVta-C @ Prevta[4]
            WITH BROWSE {&BROWSE-NAME}.

    RUN Precio-de-Oficina.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-MATG.Prevta[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-MATG.Prevta[4] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-MATG.Prevta[4] IN BROWSE br_table /* Precio C!S/. */
DO:
    ASSIGN
        F-PreVta-C = DECIMAL(T-MATG.Prevta[4]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
        X-CTOUND = DECIMAL(T-MATG.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

     F-FACTOR = 1.
     F-MrgUti-C = 0.
     /****   Busca el Factor de conversion   ****/
     IF T-MATG.UndC <> "" THEN DO:
         FIND Almtconv WHERE Almtconv.CodUnid = T-MATG.UndBas
                        AND  Almtconv.Codalter = T-MATG.UndC
                       NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Almtconv THEN DO:
            MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
         END.
         F-FACTOR = Almtconv.Equival.
         F-MrgUti-C = ROUND(((((F-PreVta-C / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
     END.
     /*******************************************/


     DISPLAY F-MrgUti-C @ T-MATG.MrgUti-C
             WITH BROWSE {&BROWSE-NAME}.

     RUN Precio-de-Oficina.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-MATG.MrgAlt[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-MATG.MrgAlt[1] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-MATG.MrgAlt[1] IN BROWSE br_table /* UTILEX!% Uti */
DO:
    ASSIGN
        X-CTOUND   = DECIMAL(T-MATG.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
     F-FACTOR = 1.
     /****   Busca el Factor de conversion   ****/
     FIND Almtconv WHERE Almtconv.CodUnid = T-MATG.UndBas
         AND  Almtconv.Codalter = T-MATG.CHR__01
         NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Almtconv THEN RETURN.
     F-FACTOR = Almtconv.Equival.
     DISPLAY
         ROUND(( X-CTOUND * (1 + DECIMAL(SELF:SCREEN-VALUE) / 100) ), 6) * F-FACTOR @ T-MATG.PreAlt[1] 
         T-MATG.CHR__01 @ T-MATG.UndAlt[1]
         WITH BROWSE {&browse-name}.
/*      IF T-MATG.UndAlt[1]:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:                */
/*          FIND Almtconv WHERE Almtconv.CodUnid = T-MATG.UndBas                                */
/*              AND  Almtconv.Codalter = T-MATG.UndAlt[1]:SCREEN-VALUE IN BROWSE {&browse-name} */
/*              NO-LOCK NO-ERROR.                                                               */
/*          IF NOT AVAILABLE Almtconv THEN RETURN.                                              */
/*          F-FACTOR = Almtconv.Equival.                                                        */
/*          DISPLAY                                                                             */
/*              ROUND(( X-CTOUND * (1 + DECIMAL(SELF:SCREEN-VALUE) / 100) ), 6) * F-FACTOR @    */
/*               T-MATG.PreAlt[1] WITH BROWSE {&browse-name}.                                   */
/*      END.                                                                                    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-MATG.PreAlt[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-MATG.PreAlt[1] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-MATG.PreAlt[1] IN BROWSE br_table /* UTILEX!Precio S/. */
DO:
    ASSIGN
        X-CTOUND = DECIMAL(T-MATG.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
    F-FACTOR = 1.
    /****   Busca el Factor de conversion   ****/
    FIND Almtconv WHERE Almtconv.CodUnid = T-MATG.UndBas
        AND  Almtconv.Codalter = T-MATG.CHR__01
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN RETURN.
    F-FACTOR = Almtconv.Equival.
    DISPLAY
        ROUND(((((DECIMAL(SELF:SCREEN-VALUE) / F-FACTOR) / X-CTOUND) - 1) * 100), 6) @ T-MATG.MrgAlt[1] 
        T-MATG.CHR__01 @ T-MATG.UndAlt[1]
        WITH BROWSE {&browse-name}.

/*     IF T-MATG.UndAlt[1]:SCREEN-VALUE IN BROWSE {&browse-name} <> "" THEN DO:                */
/*         FIND Almtconv WHERE Almtconv.CodUnid = T-MATG.UndBas                                */
/*             AND  Almtconv.Codalter = T-MATG.UndAlt[1]:SCREEN-VALUE IN BROWSE {&browse-name} */
/*             NO-LOCK NO-ERROR.                                                               */
/*         IF NOT AVAILABLE Almtconv THEN RETURN.                                              */
/*         F-FACTOR = Almtconv.Equival.                                                        */
/*         DISPLAY                                                                             */
/*             ROUND(((((DECIMAL(SELF:SCREEN-VALUE) / F-FACTOR) / X-CTOUND) - 1) * 100), 6) @  */
/*             T-MATG.MrgAlt[1] WITH BROWSE {&BROWSE-NAME}.                                    */
/*     END.                                                                                    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-MATG.UndAlt[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-MATG.UndAlt[1] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF T-MATG.UndAlt[1] IN BROWSE br_table /* UTILEX!UM */
DO:
  SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 B-table-Win
ON CHOOSE OF BUTTON-8 IN FRAME F-Main /* APLICAR FILTRO */
DO:
   RUN Carga-Temporal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Linea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Linea B-table-Win
ON VALUE-CHANGED OF COMBO-BOX-Linea IN FRAME F-Main /* Linea */
DO:
  ASSIGN {&self-name}.
  COMBO-BOX-Sublinea:DELETE(COMBO-BOX-Sublinea:LIST-ITEMS).
  COMBO-BOX-Sublinea:ADD-LAST("TODAS").
  COMBO-BOX-Sublinea:SCREEN-VALUE = "TODAS".
  FOR EACH Almsfami NO-LOCK WHERE Almsfami.codcia = s-codcia
      AND Almsfami.codfam = ENTRY(1, COMBO-BOX-Linea, ' - ') :
      COMBO-BOX-Sublinea:ADD-LAST(AlmSFami.subfam + " - " + AlmSFami.dessub).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Sublinea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Sublinea B-table-Win
ON VALUE-CHANGED OF COMBO-BOX-Sublinea IN FRAME F-Main /* Sublinea */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-CodMat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodMat B-table-Win
ON LEAVE OF FILL-IN-CodMat IN FRAME F-Main /* Art�culo */
DO:
    ASSIGN {&self-name}.
    DEF VAR pCodMat AS CHAR.
    pCodMat = SELF:SCREEN-VALUE.
    IF pCodMat = '' THEN RETURN.
    ASSIGN
        pCodMat = STRING(INTEGER(pCodMat), '999999')
        NO-ERROR.
    FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
        AND Almmmatg.codmat = pCodMat
        AND Almmmatg.tpoart <> 'D'
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almmmatg THEN DO:
        MESSAGE 'Art�culo NO registrado o desactivado'
            VIEW-AS ALERT-BOX WARNING.
        SELF:SCREEN-VALUE = ''.
        RETURN NO-APPLY.
    END.
    SELF:SCREEN-VALUE = pCodMat.
    ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-CodPro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodPro B-table-Win
ON LEAVE OF FILL-IN-CodPro IN FRAME F-Main /* Proveedor */
DO:
  ASSIGN {&self-name}.
  FIND gn-prov WHERE gn-prov.codcia = pv-codcia
      AND gn-prov.codpro = {&self-name}
      NO-LOCK NO-ERROR.
  IF AVAILABLE gn-prov THEN FILL-IN-NomPro = gn-prov.NomPro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DesMat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DesMat B-table-Win
ON LEAVE OF FILL-IN-DesMat IN FRAME F-Main /* Descripci�n */
DO:
  ASSIGN {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_LIMA_-_Descuento_por_Volume
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_LIMA_-_Descuento_por_Volume B-table-Win
ON CHOOSE OF MENU-ITEM m_LIMA_-_Descuento_por_Volume /* LIMA - Descuento por Volumen */
DO:
    IF AVAILABLE Almmmatg THEN RUN Vta2/D-Lima-DtoVol (Almmmatg.codmat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_LIMA_-_Descuento_Promociona
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_LIMA_-_Descuento_Promociona B-table-Win
ON CHOOSE OF MENU-ITEM m_LIMA_-_Descuento_Promociona /* LIMA - Descuento Promocional */
DO:
    IF AVAILABLE Almmmatg THEN RUN Vta2/D-Lima-Dtopromv2 (Almmmatg.codmat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_PROVINCIAS_-_Descuento_Prom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_PROVINCIAS_-_Descuento_Prom B-table-Win
ON CHOOSE OF MENU-ITEM m_PROVINCIAS_-_Descuento_Prom /* PROVINCIAS - Descuento Promocional 3ros */
DO:
    IF AVAILABLE Almmmatg THEN RUN Vta2/ddctoprovinciapromo3ros (Almmmatg.codmat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_PROVINCIAS_-_Descuento_Prom2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_PROVINCIAS_-_Descuento_Prom2 B-table-Win
ON CHOOSE OF MENU-ITEM m_PROVINCIAS_-_Descuento_Prom2 /* PROVINCIAS - Descuento Promocional Propios */
DO:
    IF AVAILABLE Almmmatg THEN RUN Vta2/ddctoprovinciapromopropios (Almmmatg.codmat).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_UTILEX_-_Descuento_por_Volu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_UTILEX_-_Descuento_por_Volu B-table-Win
ON CHOOSE OF MENU-ITEM m_UTILEX_-_Descuento_por_Volu /* UTILEX - Descuento por Volumen */
DO:
    FIND VtaListaMinGn OF T-MATG NO-LOCK NO-ERROR.
    IF AVAILABLE VtaListaMinGn THEN RUN Vta2/D-Utilex-DtoVol (T-MATG.codmat).
    ELSE DO:
        MESSAGE 'Primero debe asignarle un PRECIO DE OFICINA UTILEX'
            VIEW-AS ALERT-BOX WARNING.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_UTILEX_-_Descuento_Promocio
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_UTILEX_-_Descuento_Promocio B-table-Win
ON CHOOSE OF MENU-ITEM m_UTILEX_-_Descuento_Promocio /* UTILEX - Descuento Promocional */
DO:
    FIND VtaListaMinGn OF T-MATG NO-LOCK NO-ERROR.
    IF AVAILABLE VtaListaMinGn THEN RUN Vta2/D-Utilex-DtoProm (T-MATG.codmat).
    ELSE DO:
        MESSAGE 'Primero debe asignarle un PRECIO DE OFICINA UTILEX'
            VIEW-AS ALERT-BOX WARNING.
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

ON 'RETURN':U OF T-MATG.MonVta, 
    T-MATG.MrgAlt[1], T-MATG.UndAlt[1], T-MATG.PreAlt[1], 
    T-MATG.MrgUti-A, T-MATG.MrgUti-B, T-MATG.MrgUti-C, 
    T-MATG.Prevta[1], T-MATG.Prevta[2], T-MATG.Prevta[3], T-MATG.Prevta[4],
    T-MATG.UndA,  T-MATG.UndB, T-MATG.UndC
DO:
    APPLY 'TAB':U.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza-UTILEX B-table-Win 
PROCEDURE Actualiza-UTILEX :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR x-CtoTot AS DEC NO-UNDO.
DEF VAR f-Factor AS DEC NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* Primer Caso: Registrar informaci�n en Lista Minorista General */
    IF T-MATG.MrgAlt[1] > 0 OR T-MATG.PreAlt[1] > 0 THEN DO:
        FIND VtaListaMinGn OF T-MATG EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR THEN DO:
            IF LOCKED(VtaListaMinGn) THEN DO:
                RUN dispatch IN THIS-PROCEDURE ('show-errors':U).
                UNDO, RETURN 'ADM-ERROR'.
            END.
            CREATE VtaListaMinGn.
            ASSIGN
                VtaListaMinGn.CodCia = T-MATG.codcia
                VtaListaMinGn.codmat = T-MATG.codmat
                VtaListaMinGn.FchIng = TODAY.
        END.
        ASSIGN
            VtaListaMinGn.Chr__01 = T-MATG.UndAlt[1]
            VtaListaMinGn.Dec__01 = T-MATG.MrgAlt[1]
            VtaListaMinGn.PreOfi  = T-MATG.PreAlt[1]
            VtaListaMinGn.FchAct  = TODAY
            VtaListaMinGn.usuario = s-user-id.
        /* Un solo tipo de cambio, una sola moneda */
        ASSIGN
            VtaListaMinGn.MonVta  = Almmmatg.MonVta
            VtaListaMinGn.TpoCmb  = Almmmatg.TpoCmb.
        /* REGRABAMOS EN LA MONEDA DE VENTA */
        IF Almmmatg.MonVta = 2 THEN
            ASSIGN
            VtaListaMinGn.PreOfi  = VtaListaMinGn.PreOfi / Almmmatg.TpoCmb.
        /* Calculamos el margen */
        IF Almmmatg.monvta = VtaListaMinGn.monvta THEN x-CtoTot = Almmmatg.CtoTot.
        ELSE IF VtaListaMinGn.monvta = 1 THEN x-CtoTot = Almmmatg.ctotot *  Almmmatg.tpocmb.
        ELSE x-CtoTot = Almmmatg.ctotot /  Almmmatg.tpocmb.
        FIND Almtconv WHERE Almtconv.CodUnid  = Almmmatg.UndBas 
            AND Almtconv.Codalter = VtaListaMinGn.Chr__01
            NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN DO:
            F-FACTOR = Almtconv.Equival / Almmmatg.FacEqu.
        END.  
        ASSIGN
            VtaListaMinGn.Dec__01 = ( (VtaListaMinGn.PreOfi / (x-Ctotot * f-Factor) ) - 1 ) * 100
            T-MATG.MrgAlt[1] = VtaListaMinGn.Dec__01.
        FIND CURRENT VtaListaMinGn NO-LOCK NO-ERROR.
    END.
    ELSE DO:
        FIND VtaListaMinGn OF T-MATG EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE VtaListaMinGn THEN DO:
            DELETE VtaListaMinGn.
        END.
        ASSIGN
            T-MATG.MrgAlt[1] = 0
            T-MATG.PreAlt[1] = 0
            T-MATG.UndAlt[1] = "".
    END.
END.
RETURN 'OK'.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal B-table-Win 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE T-MATG.

/* CASO DE SOLICITAR UN CODIGO ESPEC�FICO */
FIND Almmmatg WHERE Almmmatg.codcia = s-codcia
    AND Almmmatg.TpoArt <> "D"
    AND Almmmatg.codMat = FILL-IN-CodMat
    NO-LOCK NO-ERROR.
IF AVAILABLE Almmmatg AND FILL-IN-CodMat <> '' THEN DO:
    /* Limpiamos otros filtros */
    ASSIGN
        COMBO-BOX-Linea = 'TODAS'
        COMBO-BOX-Sublinea = 'TODAS'
        FILL-IN-CodPro = ''
        FILL-IN-DesMat = ''
        FILL-IN-NomPro = ''.
    DISPLAY
        COMBO-BOX-Linea COMBO-BOX-Sublinea FILL-IN-CodPro FILL-IN-DesMat FILL-IN-NomPro
        WITH FRAME {&FRAME-NAME}.
    EMPTY TEMP-TABLE T-MATG.
    CREATE T-MATG.
    BUFFER-COPY Almmmatg 
        EXCEPT Almmmatg.PreAlt Almmmatg.UndAlt Almmmatg.MrgAlt
        TO T-MATG.
END.
ELSE DO:
    FOR EACH Almmmatg NO-LOCK WHERE {&Condicion}:
        CREATE T-MATG.
        BUFFER-COPY Almmmatg 
            EXCEPT Almmmatg.PreAlt Almmmatg.UndAlt Almmmatg.MrgAlt
            TO T-MATG.
        FIND VtaListaMinGn OF Almmmatg NO-LOCK NO-ERROR.
        IF AVAILABLE VtaListaMinGn THEN
            ASSIGN
            T-MATG.UndAlt[1] = VtaListaMinGn.Chr__01 
            T-MATG.MrgAlt[1] = VtaListaMinGn.Dec__01
            T-MATG.PreAlt[1] = VtaListaMinGn.PreOfi.
    END.
END.
/* ************************************** */
/* TODOS LOS PRECIOS EN SOLES */
FOR EACH T-MATG:
    FIND VtaListaMinGn OF T-MATG NO-LOCK NO-ERROR.
    IF AVAILABLE VtaListaMinGn THEN
        ASSIGN
        T-MATG.UndAlt[1] = VtaListaMinGn.Chr__01 
        T-MATG.MrgAlt[1] = VtaListaMinGn.Dec__01
        T-MATG.PreAlt[1] = VtaListaMinGn.PreOfi.
    IF T-MATG.MonVta = 2 THEN
        ASSIGN
        T-MATG.CtoLis = T-MATG.CtoLis * T-MATG.TpoCmb
        T-MATG.CtoTot = T-MATG.CtoTot  * T-MATG.TpoCmb
        T-MATG.PreAlt[1] = T-MATG.PreAlt[1] * T-MATG.TpoCmb
        T-MATG.Prevta[1] = T-MATG.PreVta[1] * T-MATG.TpoCmb
        T-MATG.Prevta[2] = T-MATG.PreVta[2] * T-MATG.TpoCmb
        T-MATG.Prevta[3] = T-MATG.PreVta[3] * T-MATG.TpoCmb
        T-MATG.Prevta[4] = T-MATG.PreVta[4] * T-MATG.TpoCmb
        T-MATG.PreOfi    = T-MATG.PreOfi    * T-MATG.TpoCmb.
    IF T-MATG.DEC__02 = 2 THEN
        ASSIGN
        T-MATG.CtoTotMarco = T-MATG.CtoTotMarco  * T-MATG.TpoCmb.
END.
RUN dispatch IN THIS-PROCEDURE ('open-query':U).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel B-table-Win 
PROCEDURE Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE t-Column                AS INTEGER INIT 1.
DEFINE VARIABLE t-Row                   AS INTEGER INIT 1.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/* set the column names for the Worksheet */
DEF VAR x-CtoTot LIKE T-MATG.ctotot NO-UNDO.
DEF VAR x-PreVta LIKE T-MATG.prevta NO-UNDO.
DEF VAR x-PreOfi LIKE T-MATG.preofi NO-UNDO.

ASSIGN
    chWorkSheet:Range("A1"):Value = "CONTINENTAL - LISTA DE PRECIOS LIMA Y UTILEX"
    chWorkSheet:Range("A2"):Value = "ARTICULO"
    chWorkSheet:Columns("A"):NumberFormat = "@"
    chWorkSheet:Range("B2"):Value = "DESCRIPCION"
    chWorkSheet:Range("C2"):Value = "UNIDAD"
    chWorkSheet:Range("D2"):Value = "MONEDA"
    chWorkSheet:Range("E2"):Value = "TC"
    chWorkSheet:Range("F2"):Value = "MARCA"
    chWorkSheet:Range("G2"):Value = "COSTO TOTAL S/."
    chWorkSheet:Range("H2"):Value = "COSTO MARCO S/."
    chWorkSheet:Range("I2"):Value = "PRECIO LISTA S/."
    chWorkSheet:Range("J2"):Value = "%UTI A"
    chWorkSheet:Range("K2"):Value = "PRECIO A S/."
    chWorkSheet:Range("L2"):Value = "UM A"
    chWorkSheet:Range("M2"):Value = "%UTI B"
    chWorkSheet:Range("N2"):Value = "PRECIO B S/."
    chWorkSheet:Range("O2"):Value = "UM B"
    chWorkSheet:Range("P2"):Value = "%UTI C"
    chWorkSheet:Range("Q2"):Value = "PRECIO C S/."
    chWorkSheet:Range("R2"):Value = "UM C"
    chWorkSheet:Range("S2"):Value = "% UTI OFI"
    chWorkSheet:Range("T2"):Value = "PRECIO OFICINA S/."
    chWorkSheet:Range("U2"):Value = "UM OFIC"
    chWorkSheet:Range("V2"):Value = "UTILEX % UTI"
    chWorkSheet:Range("W2"):Value = "UTILEX PRECIO S/."
    chWorkSheet:Range("X2"):Value = "UTILEX UM"
    chWorkSheet:Range("Y2"):Value = "CLASIFICACION".

ASSIGN
    t-Row = 2.
GET FIRST {&browse-name}.
REPEAT WHILE AVAILABLE T-MATG:
    ASSIGN
        t-Column = 0
        t-Row    = t-Row + 1.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.codmat.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.desmat.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.undstk.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = (IF T-MATG.monvta = 1 THEN 'S/.' ELSE 'US$').
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.tpocmb.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.desmar.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.ctotot.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.CtoTotMarco.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.Prevta[1].
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.mrguti-a.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.Prevta[2].
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.unda.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.mrguti-b.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.Prevta[3].
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.undb.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.mrguti-c.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.Prevta[4].
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.undc.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.Dec__01.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.PreOfi.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.CHR__01.
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.MrgAlt[1].
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.PreAlt[1].
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.UndAlt[1].
    ASSIGN
        t-Column = t-Column + 1
        chWorkSheet:Cells(t-Row, t-Column):VALUE = T-MATG.TipRot[1].
    GET NEXT {&browse-name}.
END.
chExcelApplication:VISIBLE = TRUE.
/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

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
  FIND CURRENT Almmmatg EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAILABLE Almmmatg THEN UNDO, RETURN 'ADM-ERROR'.
    
  DEFINE VARIABLE F-FACTOR     AS DECIMAL   NO-UNDO.
  DEFINE VARIABLE F-PreAnt LIKE Almmmatg.PreBas  NO-UNDO.

  F-PreAnt = Almmmatg.PreBas.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      T-MATG.CtoTot = DECIMAL(T-MATG.CtoTot:SCREEN-VALUE IN BROWSE {&browse-name})
      T-MATG.Dec__01 = DECIMAL(T-MATG.DEC__01:SCREEN-VALUE IN BROWSE {&browse-name})
      T-MATG.PreOfi = DECIMAL(T-MATG.PreOfi:SCREEN-VALUE IN BROWSE {&browse-name}).
  IF T-MATG.MrgAlt[1] > 0 OR T-MATG.PreALt[1] > 0 
      THEN T-MATG.UndAlt[1] = T-MATG.Chr__01.    /* <<< OJO <<< */
  ELSE T-MATG.UndAlt[1] = "".

  ASSIGN
      Almmmatg.PreVta[1] = T-MATG.PreVta[1]
      Almmmatg.PreVta[2] = T-MATG.Prevta[2]
      Almmmatg.PreVta[3] = T-MATG.Prevta[3]
      Almmmatg.PreVta[4] = T-MATG.Prevta[4]
      Almmmatg.MrgUti-A  = T-MATG.MrgUti-A
      Almmmatg.MrgUti-B  = T-MATG.MrgUti-B
      Almmmatg.MrgUti-C  = T-MATG.MrgUti-C
      Almmmatg.MonVta = T-MATG.MonVta
      Almmmatg.CtoTot = T-MATG.CtoTot
      Almmmatg.CtoLis = T-MATG.CtoLis
      Almmmatg.CtoUnd = T-MATG.CtoLis
      Almmmatg.Dec__01 = T-MATG.Dec__01
      Almmmatg.PreOfi = T-MATG.PreOfi.
  
  {vta/lispre-a.i}

  /* REGRABAMOS EN LA MONEDA DE VENTA */
  IF Almmmatg.MonVta = 2 THEN DO:
      ASSIGN
          Almmmatg.CtoLis = Almmmatg.CtoLis / Almmmatg.TpoCmb
          Almmmatg.CtoTot = Almmmatg.CtoTot  / Almmmatg.TpoCmb
          Almmmatg.CtoUnd = Almmmatg.CtoUnd  / Almmmatg.TpoCmb
          Almmmatg.Prevta[1] = Almmmatg.PreVta[1] / Almmmatg.TpoCmb
          Almmmatg.Prevta[2] = Almmmatg.PreVta[2] / Almmmatg.TpoCmb
          Almmmatg.Prevta[3] = Almmmatg.PreVta[3] / Almmmatg.TpoCmb
          Almmmatg.Prevta[4] = Almmmatg.PreVta[4] / Almmmatg.TpoCmb
          Almmmatg.PreOfi    = Almmmatg.PreOfi    / Almmmatg.TpoCmb.
  END.

  IF F-PreAnt <> Almmmatg.PreBas THEN Almmmatg.FchmPre[3] = TODAY.
  ASSIGN
      Almmmatg.FchmPre[1] = TODAY
      Almmmatg.Usuario = S-USER-ID
      Almmmatg.FchAct  = TODAY.

  /* RHC Limpiamos informaci�n en exceso */
  IF Almmmatg.UndC = '' THEN ASSIGN Almmmatg.MrgUti-C = 0 Almmmatg.Prevta[4] = 0.
  IF Almmmatg.UndB = '' THEN ASSIGN Almmmatg.MrgUti-B = 0 Almmmatg.Prevta[3] = 0.
  IF Almmmatg.UndA = '' THEN ASSIGN Almmmatg.MrgUti-A = 0 Almmmatg.Prevta[2] = 0.

  /* REFLEJAMOS INFORMACION EL LA LISTA MINORISTA GENERAL UTILEX */
  RUN Actualiza-UTILEX.
  IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

  /* Aqui se cuelga 03Nov2014 ??????  */
  FIND CURRENT Almmmatg NO-LOCK NO-ERROR.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          COMBO-BOX-Linea:SENSITIVE = YES
          COMBO-BOX-Sublinea:SENSITIVE = YES
          FILL-IN-CodPro:SENSITIVE = YES
          FILL-IN-DesMat:SENSITIVE = YES
          FILL-IN-CodMat:SENSITIVE = YES
          BUTTON-8:SENSITIVE = YES.
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
      ASSIGN
          COMBO-BOX-Linea:SENSITIVE = NO
          COMBO-BOX-Sublinea:SENSITIVE = NO
          FILL-IN-CodPro:SENSITIVE = NO
          FILL-IN-DesMat:SENSITIVE = NO
          FILL-IN-CodMat:SENSITIVE = NO
          BUTTON-8:SENSITIVE = NO.
      IF Almmmatg.CHR__02 = "T" THEN T-MATG.Prevta[1]:READ-ONLY IN BROWSE {&browse-name} = YES.
      ELSE T-MATG.Prevta[1]:READ-ONLY IN BROWSE {&browse-name} = NO.
  END.

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
  DO WITH FRAME {&FRAME-NAME}:
/*       FOR EACH Almtfami NO-LOCK WHERE Almtfami.codcia = s-codcia:              */
/*           COMBO-BOX-Linea:ADD-LAST(Almtfami.codfam + " - " + Almtfami.desfam). */
/*       END.                                                                     */
      FOR EACH Vtatabla NO-LOCK WHERE Vtatabla.codcia = s-codcia
          AND Vtatabla.tabla = "LP"
          AND Vtatabla.llave_c1 = s-user-id,
          FIRST Almtfami NO-LOCK WHERE Almtfami.codcia = s-codcia
          AND Almtfami.codfam = Vtatabla.llave_c2:
          COMBO-BOX-Linea:ADD-LAST( Almtfami.codfam + ' - ' + Almtfami.desfam).
      END.
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
  RUN Precio-de-Oficina.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precio-de-oficina B-table-Win 
PROCEDURE Precio-de-oficina :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

{vta2/precio-de-oficina-tiendas.i}

/* MaxCat = 0.                                                                                                                                                     */
/* MaxVta = 0.                                                                                                                                                     */
/* fmot   = 0.                                                                                                                                                     */
/* MrgMin = 5000.                                                                                                                                                  */
/* MrgOfi = 0.                                                                                                                                                     */
/* F-FACTOR = 1.                                                                                                                                                   */
/* MaxCat = 4.                                                                                                                                                     */
/* MaxVta = 3.                                                                                                                                                     */
/*                                                                                                                                                                 */
/* ASSIGN                                                                                                                                                          */
/*     F-MrgUti-A = DECIMAL(T-MATG.MrgUti-A:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})                                                                                 */
/*     F-PreVta-A = DECIMAL(T-MATG.Prevta[2]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})                                                                                */
/*     F-MrgUti-B = DECIMAL(T-MATG.MrgUti-B:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})                                                                                 */
/*     F-PreVta-B = DECIMAL(T-MATG.Prevta[3]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})                                                                                */
/*     F-MrgUti-C = DECIMAL(T-MATG.MrgUti-C:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})                                                                                 */
/*     F-PreVta-C = DECIMAL(T-MATG.Prevta[4]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).                                                                               */
/*                                                                                                                                                                 */
/* X-CTOUND = DECIMAL(T-MATG.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).                                                                                        */
/*                                                                                                                                                                 */
/* /****   Busca el Factor de conversion   ****/                                                                                                                   */
/* IF T-MATG.Chr__01 <> "" THEN DO:                                                                                                                                */
/*     FIND Almtconv WHERE Almtconv.CodUnid = T-MATG.UndBas                                                                                                        */
/*         AND  Almtconv.Codalter = T-MATG.Chr__01                                                                                                                 */
/*         NO-LOCK NO-ERROR.                                                                                                                                       */
/*     IF NOT AVAILABLE Almtconv THEN DO:                                                                                                                          */
/*        MESSAGE "Codigo de unidad no existe" VIEW-AS ALERT-BOX ERROR.                                                                                            */
/*        RETURN.                                                                                                                                                  */
/*     END.                                                                                                                                                        */
/*     F-FACTOR = Almtconv.Equival.                                                                                                                                */
/* END.                                                                                                                                                            */
/* /*******************************************/                                                                                                                   */
/*                                                                                                                                                                 */
/* /* **********************************************************************                                                                                       */
/*     NOTA IMPORTANTE: Cualquier cambio debe hacerse tambi�n                                                                                                      */
/*                     en LOGISTICA -> LIsta de precios por proveedor                                                                                              */
/* ************************************************************************* */                                                                                    */
/*                                                                                                                                                                 */
/* CASE T-MATG.Chr__02 :                                                                                                                                           */
/*     WHEN "T" THEN DO:                                                                                                                                           */
/*         /*  TERCEROS  */                                                                                                                                        */
/*         IF F-MrgUti-A < MrgMin AND F-MrgUti-A <> 0 THEN MrgMin = F-MrgUti-A.                                                                                    */
/*         IF F-MrgUti-B < MrgMin AND F-MrgUti-B <> 0 THEN MrgMin = F-MrgUti-B.                                                                                    */
/*         IF F-MrgUti-C < MrgMin AND F-MrgUti-C <> 0 THEN MrgMin = F-MrgUti-C.                                                                                    */
/*                                                                                                                                                                 */
/*         fmot = (1 + MrgMin / 100) / ((1 - MaxCat / 100) * (1 - MaxVta / 100)).                                                                                  */
/*                                                                                                                                                                 */
/*         pre-ofi = X-CTOUND * fmot * F-FACTOR .                                                                                                                  */
/*                                                                                                                                                                 */
/*         MrgOfi = ROUND((fmot - 1) * 100, 6).                                                                                                                    */
/*                                                                                                                                                                 */
/*     END.                                                                                                                                                        */
/*     WHEN "P" THEN DO:                                                                                                                                           */
/*         /* PROPIOS */                                                                                                                                           */
/*        pre-ofi = DECIMAL(T-MATG.Prevta[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) * F-FACTOR.                                                                    */
/*        MrgOfi = ((DECIMAL(T-MATG.Prevta[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) / DECIMAL(T-MATG.Ctotot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} )) - 1 ) * 100. */
/*                                                                                                                                                                 */
/*     END.                                                                                                                                                        */
/* END.                                                                                                                                                            */
/*                                                                                                                                                                 */
/*                                                                                                                                                                 */
/* DO WITH FRAME {&FRAME-NAME}:                                                                                                                                    */
/*    DISPLAY                                                                                                                                                      */
/*        MrgOfi @ T-MATG.Dec__01                                                                                                                                  */
/*        pre-ofi @ T-MATG.PreOfi                                                                                                                                  */
/*        WITH BROWSE {&BROWSE-NAME}.                                                                                                                              */
/* END.                                                                                                                                                            */

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

                      
    /* VALIDACION LISTA MAYORISTA LIMA */ 
    F-Factor = 1.                    
    IF T-MATG.UndA <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = T-MATG.UndBas
                       AND  Almtconv.Codalter = T-MATG.UndA
                      NO-LOCK NO-ERROR.
        IF AVAILABLE Almtconv THEN F-FACTOR = Almtconv.Equival.
    END.

   CASE T-MATG.Chr__02 :
        WHEN "T" THEN DO:        
            
        END.
        WHEN "P" THEN DO:
           IF (DECIMAL(T-MATG.Prevta[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) * F-FACTOR ) < 
              (DECIMAL(T-MATG.Prevta[2]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})) THEN DO:
              MESSAGE "Precio de lista Menor al Precio Venta A......" VIEW-AS ALERT-BOX ERROR.
              APPLY "ENTRY" TO T-MATG.Prevta[1].
              RETURN "ADM-ERROR".      
           END.                         
        END. 
    END.    

   IF DECIMAL(T-MATG.MrgUti-C:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) < 0 THEN DO:
      MESSAGE "Margen Utilidad C Negativo......" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO T-MATG.MrgUti-C.
      RETURN "ADM-ERROR".      
   END.

   IF DECIMAL(T-MATG.MrgUti-B:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) < 0 THEN DO:
      MESSAGE "Margen Utilidad B Negativo......" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO T-MATG.MrgUti-B.
      RETURN "ADM-ERROR".      
   END.

   IF DECIMAL(T-MATG.MrgUti-A:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) < 0 THEN DO:
      MESSAGE "Margen Utilidad A Negativo......" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO T-MATG.MrgUti-A.
      RETURN "ADM-ERROR".      
   END.

   IF DECIMAL(T-MATG.Monvta:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO:
      MESSAGE "Codigo de Moneda Incorrecto......" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO T-MATG.MonVta.
      RETURN "ADM-ERROR".      
   END.

   IF DECIMAL(T-MATG.MrgUti-C:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > DECIMAL(T-MATG.MrgUti-B:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN DO:
      MESSAGE "Margen de util. C as mayor que el margen de util. B" SKIP
            "Margen Utilidad Incorrecto......" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO T-MATG.MrgUti-C.
      RETURN "ADM-ERROR".      
   END.

   IF DECIMAL(T-MATG.MrgUti-B:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > DECIMAL(T-MATG.MrgUti-A:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN DO:
      MESSAGE "Margen de util. B as mayor que el margen de util. A" SKIP
            "Margen Utilidad Incorrecto......" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO T-MATG.MrgUti-B.
      RETURN "ADM-ERROR".      
   END.

   /* VALIDACION LISTA MINORISTA UTILEX */
   IF DECIMAL(T-MATG.MrgAlt[1]:SCREEN-VALUE IN BROWSE {&browse-name}) > 0
       OR DECIMAL(T-MATG.PreAlt[1]:SCREEN-VALUE IN BROWSE {&browse-name}) > 0
       THEN DO:
       FIND Almtconv WHERE Almtconv.CodUnid  = T-MATG.UndBas 
           AND Almtconv.Codalter = T-MATG.UndAlt[1]:SCREEN-VALUE IN BROWSE {&browse-name}
           NO-LOCK NO-ERROR.
       IF NOT AVAILABLE Almtconv THEN DO:
           MESSAGE "Equivalencia UTILEX NO definida" SKIP
               "Unidad base :" T-MATG.undbas SKIP
               "Unidad venta:" T-MATG.UndAlt[1]:SCREEN-VALUE IN BROWSE {&browse-name}
               VIEW-AS ALERT-BOX ERROR.
           APPLY 'entry' TO T-MATG.UndAlt[1] IN BROWSE {&browse-name}.
           RETURN "ADM-ERROR".
       END.
       IF DECIMAL(T-MATG.PreAlt[1]:SCREEN-VALUE IN BROWSE {&Browse-name}) = 0 THEN DO:
           MESSAGE 'Debe ingresar el precio de venta UTILEX'
               VIEW-AS ALERT-BOX ERROR.
           APPLY 'entry' TO T-MATG.PreAlt[1] IN BROWSE {&browse-name}.
           RETURN "ADM-ERROR".
       END.
   END.

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

