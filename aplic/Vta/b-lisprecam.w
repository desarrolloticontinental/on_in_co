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
DEFINE SHARED VARIABLE S-CODCIA AS INTEGER.
DEFINE SHARED VARIABLE S-NOMCIA AS CHAR.
DEFINE SHARED VARIABLE S-CODALM   AS CHARACTER.
DEFINE SHARED VARIABLE S-USER-ID  AS CHARACTER.

DEFINE SHARED VARIABLE lh_Handle  AS HANDLE.

DEFINE VARIABLE F-FACTOR     AS DECIMAL   NO-UNDO.

DEFINE BUFFER MATG FOR VtaCatCam.
DEFINE VARIABLE F-CODMAR AS CHAR INIT "" NO-UNDO.
DEFINE VARIABLE X-Mon AS CHAR NO-UNDO.
DEFINE VARIABLE X-CTOUND AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PRENET AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-CTOPRM AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreVta LIKE VtaCatCam.PreBas NO-UNDO.
DEFINE VARIABLE F-MrgUti AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-MrgCom AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PorImp AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreSol AS DECIMAL NO-UNDO.
DEFINE VARIABLE X-CTOTOT AS DECIMAL FORMAT "->>>>>>>>>9.999999" NO-UNDO.
DEFINE VARIABLE F-PorMax AS DECIMAL NO-UNDO.
DEFINE VARIABLE x-clase  AS CHAR.
DEFINE VARIABLE F-PreVta-A AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreVta-B AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-PreVta-C AS DECIMAL NO-UNDO.

DEFINE VARIABLE F-MrgUti-A AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-MrgUti-B AS DECIMAL NO-UNDO.
DEFINE VARIABLE F-MrgUti-C AS DECIMAL NO-UNDO.

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.


/* Definimos Variables de impresoras */
DEFINE VARIABLE s-printer-list AS CHAR NO-UNDO.
DEFINE VARIABLE s-port-list AS CHAR NO-UNDO.
DEFINE VARIABLE s-port-name AS CHAR format "x(20)" NO-UNDO.
DEFINE VARIABLE s-printer-count AS INTEGER NO-UNDO.

DEFINE NEW SHARED VARIABLE s-pagina-final     AS INTEGER.
DEFINE NEW SHARED VARIABLE s-pagina-inicial   AS INTEGER.
DEFINE NEW SHARED VARIABLE s-salida-impresion AS INTEGER.
DEFINE NEW SHARED VARIABLE s-printer-name     AS CHARACTER.
DEFINE NEW SHARED VARIABLE s-print-file       AS CHARACTER.
DEFINE NEW SHARED VARIABLE s-nro-copias       AS INTEGER.
DEFINE NEW SHARED VARIABLE s-orientacion      AS INTEGER.
DEFINE STREAM REPORT.

DEFINE VARIABLE x-dsc1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE X-PRE1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE x-dsc2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE X-PRE2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE x-dsc3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE X-PRE3 AS DECIMAL NO-UNDO.

DEFINE VARIABLE x-password AS LOGICAL NO-UNDO INITIAL FALSE.

/* Preprocesadores para cada campo filtro */ 
&SCOPED-DEFINE CONDICION VtaCatCam.CodCia = S-CODCIA ~
AND VtaCatCam.subfam <> '888'

/*&SCOPED-DEFINE FILTRO1 ( (VtaCatCam.FchmPre[2] >= F-FchDes) AND (VtaCatCam.FchmPre[2] <= F-FchHas) )*/
&SCOPED-DEFINE FILTRO1 (VtaCatCam.DesMat = F-Filtro) /*AND (VtaCatCam.DesMar BEGINS F-marca) )*/
 
DEFINE IMAGE IMAGE-1 FILENAME "IMG\TIME" SIZE 5 BY 1.5.
DEF VAR FI-MENSAJE AS CHAR FORMAT "X(40)" .

FIND FacCfgGn WHERE FacCfgGn.CodCia = S-CODCIA NO-LOCK NO-ERROR.

DEFINE FRAME F-Proceso
     IMAGE-1 AT ROW 1.5 COL 5
     "Espere un momento" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 1.5 COL 16 FONT 6
     "por favor ...." VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 2.5 COL 19 FONT 6 SKIP
     Fi-Mensaje NO-LABEL FONT 6  SKIP     
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         BGCOLOR 15 FGCOLOR 0 
         TITLE "Procesando ..." FONT 7.

DEFINE VARIABLE MaxCat LIKE ClfClie.PorDsc.
DEFINE VARIABLE MaxVta LIKE Dsctos.PorDto.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES VtaCatCam

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table VtaCatCam.codmat VtaCatCam.DesMat ~
VtaCatCam.DesMar VtaCatCam.clase VtaCatCam.MonVta X-Mon @ X-Mon ~
VtaCatCam.TpoCmb VtaCatCam.PorMax VtaCatCam.UndBas VtaCatCam.CtoLis ~
VtaCatCam.CtoTot VtaCatCam.Prevta[1] VtaCatCam.MrgUti-A VtaCatCam.Prevta[2] ~
VtaCatCam.UndA VtaCatCam.MrgUti-B VtaCatCam.Prevta[3] VtaCatCam.UndB ~
VtaCatCam.MrgUti-C VtaCatCam.Prevta[4] VtaCatCam.UndC VtaCatCam.Dec__01 ~
VtaCatCam.PreOfi VtaCatCam.Chr__01 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table VtaCatCam.clase ~
VtaCatCam.MonVta VtaCatCam.PorMax VtaCatCam.CtoLis VtaCatCam.Prevta[1] ~
VtaCatCam.MrgUti-A VtaCatCam.Prevta[2] VtaCatCam.MrgUti-B ~
VtaCatCam.Prevta[3] VtaCatCam.MrgUti-C VtaCatCam.Prevta[4] 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table VtaCatCam
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table VtaCatCam
&Scoped-define QUERY-STRING-br_table FOR EACH VtaCatCam WHERE ~{&KEY-PHRASE} ~
      AND {&CONDICION}  ~
 AND VtaCatCam.CodCia = S-CODCIA ~
 AND VtaCatCam.DesMat BEGINS F-Filtro ~
 AND VtaCatCam.CodPr1 BEGINS F-Provee ~
 AND VtaCatCam.FchCes = ? NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH VtaCatCam WHERE ~{&KEY-PHRASE} ~
      AND {&CONDICION}  ~
 AND VtaCatCam.CodCia = S-CODCIA ~
 AND VtaCatCam.DesMat BEGINS F-Filtro ~
 AND VtaCatCam.CodPr1 BEGINS F-Provee ~
 AND VtaCatCam.FchCes = ? NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_table VtaCatCam
&Scoped-define FIRST-TABLE-IN-QUERY-br_table VtaCatCam


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-FchMod F-FchDes F-FchHas F-CodMat ~
F-Filtro F-Provee br_table 
&Scoped-Define DISPLAYED-OBJECTS CB-FchMod F-FchDes F-FchHas F-CodMat ~
F-Filtro F-Provee 

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
Fechas|y||integral.VtaCatCam.DesMat
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "Fechas",
     Keys-Supplied = ':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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
Codigo|||integral.VtaCatCam.CodCia|yes,integral.VtaCatCam.codmat|yes
Descripcion|||integral.VtaCatCam.CodCia|yes,integral.VtaCatCam.DesMat|yes
Marca|||integral.VtaCatCam.CodCia|yes,integral.VtaCatCam.DesMar|yes
Familia|y||integral.VtaCatCam.CodCia|yes,integral.VtaCatCam.codfam|yes,integral.VtaCatCam.CtoLis|yes
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = "Codigo,Descripcion,Marca,Familia",
     SortBy-Case = Familia':U).

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
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-Descuentos 
       MENU-ITEM m_Descuento_Promocional LABEL "Descuento Promocional"
       MENU-ITEM m_Descuento_Por_Volumen LABEL "Descuento Por Volumen".


/* Definitions of the field level widgets                               */
DEFINE VARIABLE CB-FchMod AS CHARACTER FORMAT "X(256)":U INITIAL "Todas" 
     LABEL "Filtrar" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "Todas","Ultimas Modificaciones" 
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE F-CodMat AS CHARACTER FORMAT "X(256)":U 
     LABEL "Codigo" 
     VIEW-AS FILL-IN 
     SIZE 8.57 BY .69 NO-UNDO.

DEFINE VARIABLE F-FchDes AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69 NO-UNDO.

DEFINE VARIABLE F-FchHas AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .69 NO-UNDO.

DEFINE VARIABLE F-Filtro AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descripcion" 
     VIEW-AS FILL-IN 
     SIZE 28.72 BY .69 NO-UNDO.

DEFINE VARIABLE F-Provee AS CHARACTER FORMAT "X(256)":U 
     LABEL "Proveedor" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .69 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      VtaCatCam SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      VtaCatCam.codmat COLUMN-LABEL "Articulo" FORMAT "X(8)":U
      VtaCatCam.DesMat FORMAT "X(65)":U
      VtaCatCam.DesMar FORMAT "X(14)":U
      VtaCatCam.clase COLUMN-LABEL "Com.!Dif." FORMAT "!":U
      VtaCatCam.MonVta COLUMN-LABEL "M" FORMAT "9":U
      X-Mon @ X-Mon COLUMN-LABEL "Mon" FORMAT "X(4)":U
      VtaCatCam.TpoCmb FORMAT "Z9.9999":U
      VtaCatCam.PorMax COLUMN-LABEL "%Max!Dscto" FORMAT ">9.99":U
      VtaCatCam.UndBas COLUMN-LABEL "Und!Base" FORMAT "X(6)":U
      VtaCatCam.CtoLis COLUMN-LABEL "<Costo sin IGV>" FORMAT ">>>>,>>9.9999":U
      VtaCatCam.CtoTot COLUMN-LABEL "Costo de Lista Total" FORMAT ">>>>,>>9.9999":U
      VtaCatCam.Prevta[1] COLUMN-LABEL "Precio Lista" FORMAT ">>,>>>,>>9.9999":U
      VtaCatCam.MrgUti-A COLUMN-LABEL "%Marg!Utilid A" FORMAT "->>9.99":U
            COLUMN-BGCOLOR 11
      VtaCatCam.Prevta[2] COLUMN-LABEL "Precio Vta!A" FORMAT ">>>>,>>9.9999":U
            COLUMN-BGCOLOR 11
      VtaCatCam.UndA COLUMN-LABEL "UM.!A" FORMAT "X(6)":U COLUMN-BGCOLOR 11
      VtaCatCam.MrgUti-B COLUMN-LABEL "%Marg!Utilid B" FORMAT "->>9.99":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 13
      VtaCatCam.Prevta[3] COLUMN-LABEL "Precio Vta!B" FORMAT ">>>>,>>9.9999":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 13
      VtaCatCam.UndB COLUMN-LABEL "UM.!B" FORMAT "X(6)":U COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 13
      VtaCatCam.MrgUti-C COLUMN-LABEL "%Marg!Utilid C" FORMAT "->>9.99":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 9
      VtaCatCam.Prevta[4] COLUMN-LABEL "Precio Vta!C" FORMAT ">>>>,>>9.9999":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 9
      VtaCatCam.UndC COLUMN-LABEL "UM.!C" FORMAT "X(6)":U COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 9
      VtaCatCam.Dec__01 COLUMN-LABEL "%Marg Utilid!Pre.Ofi." FORMAT "->>9.99":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 12
      VtaCatCam.PreOfi COLUMN-LABEL "Precio de Oficina" FORMAT ">>>>,>>9.9999":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 12
      VtaCatCam.Chr__01 COLUMN-LABEL "UM.!Pre.Ofi." FORMAT "X(6)":U
            COLUMN-FGCOLOR 15 COLUMN-BGCOLOR 12
  ENABLE
      VtaCatCam.clase
      VtaCatCam.MonVta
      VtaCatCam.PorMax
      VtaCatCam.CtoLis
      VtaCatCam.Prevta[1]
      VtaCatCam.MrgUti-A
      VtaCatCam.Prevta[2]
      VtaCatCam.MrgUti-B
      VtaCatCam.Prevta[3]
      VtaCatCam.MrgUti-C
      VtaCatCam.Prevta[4]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 107.86 BY 10.58
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     CB-FchMod AT ROW 1.15 COL 4.86 COLON-ALIGNED
     F-FchDes AT ROW 1.27 COL 29.43 COLON-ALIGNED
     F-FchHas AT ROW 1.27 COL 45.86 COLON-ALIGNED
     F-CodMat AT ROW 2.15 COL 8 COLON-ALIGNED
     F-Filtro AT ROW 2.15 COL 27.14 COLON-ALIGNED
     F-Provee AT ROW 2.96 COL 8 COLON-ALIGNED
     br_table AT ROW 3.92 COL 1.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 4.


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
         HEIGHT             = 13.92
         WIDTH              = 109.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm-vm/method/vmviewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table F-Provee F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:POPUP-MENU IN FRAME F-Main             = MENU POPUP-MENU-Descuentos:HANDLE
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "INTEGRAL.VtaCatCam"
     _Options          = "NO-LOCK INDEXED-REPOSITION KEY-PHRASE SORTBY-PHRASE"
     _OrdList          = "INTEGRAL.VtaCatCam.CodCia|yes,INTEGRAL.VtaCatCam.codmat|yes"
     _Where[1]         = "{&CONDICION} 
 AND integral.VtaCatCam.CodCia = S-CODCIA
 AND integral.VtaCatCam.DesMat BEGINS F-Filtro
 AND integral.VtaCatCam.CodPr1 BEGINS F-Provee
 AND integral.VtaCatCam.FchCes = ?"
     _FldNameList[1]   > integral.VtaCatCam.codmat
"VtaCatCam.codmat" "Articulo" "X(8)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > integral.VtaCatCam.DesMat
"VtaCatCam.DesMat" ? "X(65)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > integral.VtaCatCam.DesMar
"VtaCatCam.DesMar" ? "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > integral.VtaCatCam.clase
"VtaCatCam.clase" "Com.!Dif." "!" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > integral.VtaCatCam.MonVta
"VtaCatCam.MonVta" "M" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"X-Mon @ X-Mon" "Mon" "X(4)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = integral.VtaCatCam.TpoCmb
     _FldNameList[8]   > integral.VtaCatCam.PorMax
"VtaCatCam.PorMax" "%Max!Dscto" ">9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > integral.VtaCatCam.UndBas
"VtaCatCam.UndBas" "Und!Base" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > integral.VtaCatCam.CtoLis
"VtaCatCam.CtoLis" "<Costo sin IGV>" ">>>>,>>9.9999" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > integral.VtaCatCam.CtoTot
"VtaCatCam.CtoTot" "Costo de Lista Total" ">>>>,>>9.9999" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > integral.VtaCatCam.Prevta[1]
"VtaCatCam.Prevta[1]" "Precio Lista" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > integral.VtaCatCam.MrgUti-A
"VtaCatCam.MrgUti-A" "%Marg!Utilid A" "->>9.99" "decimal" 11 ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > integral.VtaCatCam.Prevta[2]
"VtaCatCam.Prevta[2]" "Precio Vta!A" ">>>>,>>9.9999" "decimal" 11 ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > integral.VtaCatCam.UndA
"VtaCatCam.UndA" "UM.!A" "X(6)" "character" 11 ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > integral.VtaCatCam.MrgUti-B
"VtaCatCam.MrgUti-B" "%Marg!Utilid B" "->>9.99" "decimal" 13 15 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > integral.VtaCatCam.Prevta[3]
"VtaCatCam.Prevta[3]" "Precio Vta!B" ">>>>,>>9.9999" "decimal" 13 15 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > integral.VtaCatCam.UndB
"VtaCatCam.UndB" "UM.!B" "X(6)" "character" 13 15 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > integral.VtaCatCam.MrgUti-C
"VtaCatCam.MrgUti-C" "%Marg!Utilid C" "->>9.99" "decimal" 9 15 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > integral.VtaCatCam.Prevta[4]
"VtaCatCam.Prevta[4]" "Precio Vta!C" ">>>>,>>9.9999" "decimal" 9 15 ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > integral.VtaCatCam.UndC
"VtaCatCam.UndC" "UM.!C" "X(6)" "character" 9 15 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > integral.VtaCatCam.Dec__01
"VtaCatCam.Dec__01" "%Marg Utilid!Pre.Ofi." "->>9.99" "decimal" 12 15 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > integral.VtaCatCam.PreOfi
"VtaCatCam.PreOfi" "Precio de Oficina" ">>>>,>>9.9999" "decimal" 12 15 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > integral.VtaCatCam.Chr__01
"VtaCatCam.Chr__01" "UM.!Pre.Ofi." "X(6)" "character" 12 15 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ANY-PRINTABLE OF br_table IN FRAME F-Main
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
DO:
/*  IF AVAILABLE {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN
 *      RUN lgc/d-detart.r({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.codmat).
 *   RUN dispatch IN THIS-PROCEDURE ('display-fields':U).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
/*
  IF VtaCatCam.PorVta[5] = 0 THEN
     VtaCatCam.PorVta[5]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = STRING(ROUND(VtaCatCam.MrgUti * F-DctoPro / 100,2)).
*/
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
   0  objects when the browser's current row changes. */
   {src/adm/template/brschnge.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCatCam.CtoLis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCatCam.CtoLis br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF VtaCatCam.CtoLis IN BROWSE br_table /* <Costo sin IGV> */
DO:

   ASSIGN
    X-CTOTOT = DECIMAL(VtaCatCam.CtoLis:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
   
   VtaCatCam.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = IF VtaCatCam.AftIgv THEN 
                                                              STRING(X-CTOTOT * (1 + FacCfgGn.PorIgv / 100))
                                                           ELSE STRING(X-CTOTOT).
                                                           
   X-CTOUND = DECIMAL(VtaCatCam.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    
    /*******************************************/
   ASSIGN
       F-MrgUti-A = DECIMAL(VtaCatCam.MrgUti-A:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
       F-PreVta-A = DECIMAL(VtaCatCam.Prevta[2]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
       F-MrgUti-B = DECIMAL(VtaCatCam.MrgUti-B:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
       F-PreVta-B = DECIMAL(VtaCatCam.Prevta[3]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
       F-MrgUti-C = DECIMAL(VtaCatCam.MrgUti-C:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
       F-PreVta-C = DECIMAL(VtaCatCam.Prevta[4]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
    
    IF DECIMAL(VtaCatCam.Prevta[2]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > 0 THEN DO:
        F-FACTOR = 1.
        /****   Busca el Factor de conversion   ****/
        IF VtaCatCam.UndA <> "" THEN DO:
            FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                           AND  Almtconv.Codalter = VtaCatCam.UndA
                          NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Almtconv THEN DO:
               MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
               RETURN NO-APPLY.
            END.
            F-FACTOR = Almtconv.Equival.
            F-MrgUti-A = ROUND((((((F-PreVta-A / F-FACTOR) ) / X-CTOUND) - 1) * 100), 6).
        END.
        /*******************************************/
    END.
    IF DECIMAL(VtaCatCam.Prevta[3]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > 0 THEN DO:
        F-FACTOR = 1.
        /****   Busca el Factor de conversion   ****/
        IF VtaCatCam.UndB <> "" THEN DO:
            FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                           AND  Almtconv.Codalter = VtaCatCam.UndB
                          NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Almtconv THEN DO:
               MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
               RETURN NO-APPLY.
            END.
            F-FACTOR = Almtconv.Equival.
            F-MrgUti-B = ROUND((((((F-PreVta-B / F-FACTOR) ) / X-CTOUND) - 1) * 100), 6).
        END.
        /*******************************************/
    END.
    IF DECIMAL(VtaCatCam.Prevta[4]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > 0 THEN DO:
        F-FACTOR = 1.
        /****   Busca el Factor de conversion   ****/
        IF VtaCatCam.UndC <> "" THEN DO:
            FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                           AND  Almtconv.Codalter = VtaCatCam.UndC
                          NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Almtconv THEN DO:
               MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
               RETURN NO-APPLY.
            END.
            F-FACTOR = Almtconv.Equival.
            F-MrgUti-C = ROUND((((((F-PreVta-C / F-FACTOR) ) / X-CTOUND) - 1) * 100), 6).
        END.
        /*******************************************/
    END.

    DISPLAY F-MrgUti-A @ VtaCatCam.MrgUti-A
            F-MrgUti-B @ VtaCatCam.MrgUti-B
            F-MrgUti-C @ VtaCatCam.MrgUti-C
            WITH BROWSE {&BROWSE-NAME}.

    RUN Precio-de-Oficina.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCatCam.Prevta[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCatCam.Prevta[1] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF VtaCatCam.Prevta[1] IN BROWSE br_table /* Precio Lista */
DO:

    RUN Precio-de-Oficina.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCatCam.MrgUti-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCatCam.MrgUti-A br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF VtaCatCam.MrgUti-A IN BROWSE br_table /* %Marg!Utilid A */
DO:
   ASSIGN
       F-MrgUti-A = DECIMAL(VtaCatCam.MrgUti-A:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
       X-CTOUND   = DECIMAL(VtaCatCam.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    F-FACTOR = 1.
    F-PreVta-A = 0.
    /****   Busca el Factor de conversion   ****/
    IF VtaCatCam.UndA <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                       AND  Almtconv.Codalter = VtaCatCam.UndA
                      NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
           MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
        END.
        F-FACTOR = Almtconv.Equival.
        F-PreVta-A = ROUND(( X-CTOUND * (1 + F-MrgUti-A / 100) ), 6) * F-FACTOR.
    END.

   DISPLAY F-PreVta-A @ Prevta[2]
           WITH BROWSE {&BROWSE-NAME}.

   RUN Precio-de-Oficina.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCatCam.Prevta[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCatCam.Prevta[2] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF VtaCatCam.Prevta[2] IN BROWSE br_table /* Precio Vta!A */
DO:
   ASSIGN
       F-PreVta-A = DECIMAL(VtaCatCam.Prevta[2]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
       X-CTOUND = DECIMAL(VtaCatCam.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    F-FACTOR = 1.
    F-MrgUti-A = 0.    
    /****   Busca el Factor de conversion   ****/
    IF VtaCatCam.UndA <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                       AND  Almtconv.Codalter = VtaCatCam.UndA
                      NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
           MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
        END.
        F-FACTOR = Almtconv.Equival.
        F-MrgUti-A = ROUND(((((F-PreVta-A / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
    END.
    /*******************************************/


    DISPLAY F-MrgUti-A @ VtaCatCam.MrgUti-A
            WITH BROWSE {&BROWSE-NAME}.
    RUN Precio-de-Oficina.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCatCam.MrgUti-B
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCatCam.MrgUti-B br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF VtaCatCam.MrgUti-B IN BROWSE br_table /* %Marg!Utilid B */
DO:
   ASSIGN
       F-MrgUti-B = DECIMAL(VtaCatCam.MrgUti-B:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
       X-CTOUND   = DECIMAL(VtaCatCam.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    F-FACTOR = 1.
    F-Prevta-B = 0.
    /****   Busca el Factor de conversion   ****/
    IF VtaCatCam.UndB <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                       AND  Almtconv.Codalter = VtaCatCam.UndB
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


&Scoped-define SELF-NAME VtaCatCam.Prevta[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCatCam.Prevta[3] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF VtaCatCam.Prevta[3] IN BROWSE br_table /* Precio Vta!B */
DO:
   ASSIGN
       F-PreVta-B = DECIMAL(VtaCatCam.Prevta[3]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
       X-CTOUND = DECIMAL(VtaCatCam.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).


    F-FACTOR = 1.
    F-MrgUti-B = 0.   
    /****   Busca el Factor de conversion   ****/
    IF VtaCatCam.UndB <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                       AND  Almtconv.Codalter = VtaCatCam.UndB
                      NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
           MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
        END.
        F-FACTOR = Almtconv.Equival.
        F-MrgUti-B = ROUND(((((F-PreVta-B / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
    END.
    /*******************************************/


    DISPLAY F-MrgUti-B @ VtaCatCam.MrgUti-B
            WITH BROWSE {&BROWSE-NAME}.

    RUN Precio-de-Oficina.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VtaCatCam.MrgUti-C
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCatCam.MrgUti-C br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF VtaCatCam.MrgUti-C IN BROWSE br_table /* %Marg!Utilid C */
DO:
   ASSIGN
       F-MrgUti-C = DECIMAL(VtaCatCam.MrgUti-C:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
       X-CTOUND   = DECIMAL(VtaCatCam.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    F-FACTOR = 1.
    F-Prevta-C = 0.
    /****   Busca el Factor de conversion   ****/
    IF VtaCatCam.UndC <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                       AND  Almtconv.Codalter = VtaCatCam.UndC
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


&Scoped-define SELF-NAME VtaCatCam.Prevta[4]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VtaCatCam.Prevta[4] br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF VtaCatCam.Prevta[4] IN BROWSE br_table /* Precio Vta!C */
DO:
   ASSIGN
       F-PreVta-C = DECIMAL(VtaCatCam.Prevta[4]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).
       X-CTOUND = DECIMAL(VtaCatCam.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

    F-FACTOR = 1.
    F-MrgUti-C = 0.
    /****   Busca el Factor de conversion   ****/
    IF VtaCatCam.UndC <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                       AND  Almtconv.Codalter = VtaCatCam.UndC
                      NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Almtconv THEN DO:
           MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
           RETURN NO-APPLY.
        END.
        F-FACTOR = Almtconv.Equival.
        F-MrgUti-C = ROUND(((((F-PreVta-C / F-FACTOR) / X-CTOUND) - 1) * 100), 6).
    END.
    /*******************************************/


    DISPLAY F-MrgUti-C @ VtaCatCam.MrgUti-C
            WITH BROWSE {&BROWSE-NAME}.
           
    RUN Precio-de-Oficina.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-FchMod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-FchMod B-table-Win
ON VALUE-CHANGED OF CB-FchMod IN FRAME F-Main /* Filtrar */
DO:
  ASSIGN CB-FchMod F-FchDes F-FchHas .
  IF CB-FchMod = "Todas" THEN 
    RUN set-attribute-list('Key-Name=?').
  ELSE 
    RUN set-attribute-list('Key-Name=Fechas').
    
  RUN dispatch IN THIS-PROCEDURE ('open-query-cases':U).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-CodMat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CodMat B-table-Win
ON LEAVE OF F-CodMat IN FRAME F-Main /* Codigo */
DO:
  IF INTEGER(SELF:SCREEN-VALUE) = 0 THEN RETURN.
  SELF:SCREEN-VALUE = STRING(INTEGER(SELF:SCREEN-VALUE),"999999").
  ASSIGN F-CodMat.
  RUN dispatch IN THIS-PROCEDURE ('busca':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-FchDes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-FchDes B-table-Win
ON LEAVE OF F-FchDes IN FRAME F-Main /* Desde */
DO:
  /* VtaCatCam.FchmPre[2] */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Filtro B-table-Win
ON LEAVE OF F-Filtro IN FRAME F-Main /* Descripcion */
OR "RETURN":U OF F-Filtro
DO:
  IF F-Filtro = F-Filtro:SCREEN-VALUE THEN RETURN.
  ASSIGN F-Filtro.
  RUN dispatch IN THIS-PROCEDURE ('open-query':U). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-Provee
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Provee B-table-Win
ON LEAVE OF F-Provee IN FRAME F-Main /* Proveedor */
OR "RETURN":U OF F-Provee
DO:
  IF F-Provee = F-Provee:SCREEN-VALUE THEN RETURN.
  ASSIGN F-Provee.
  RUN dispatch IN THIS-PROCEDURE ('open-query':U). 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Descuento_Por_Volumen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Descuento_Por_Volumen B-table-Win
ON CHOOSE OF MENU-ITEM m_Descuento_Por_Volumen /* Descuento Por Volumen */
DO:
  IF AVAILABLE {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN
     RUN Vta/D-Dtovol.r({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.codmat).
     RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Descuento_Promocional
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Descuento_Promocional B-table-Win
ON CHOOSE OF MENU-ITEM m_Descuento_Promocional /* Descuento Promocional */
DO:
  IF AVAILABLE {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}} THEN
     RUN Vta/D-Dtoprom.r({&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.codmat).
     RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


ON "RETURN":U OF VtaCatCam.Monvta,VtaCatCam.PorMax,VtaCatCam.CtoLis,VtaCatCam.Prevta[1],VtaCatCam.MrgUti-A,VtaCatCam.Prevta[2],VtaCatCam.MrgUti-B,VtaCatCam.Prevta[3],VtaCatCam.MrgUti-C,VtaCatCam.Prevta[4]
DO:
   APPLY "TAB":U.
   RETURN NO-APPLY.
END.

ON FIND OF VtaCatCam 
DO:
   X-CTOTOT = VtaCatCam.CtoTot.
   IF VtaCatCam.AftIgv THEN X-CTOTOT = X-CTOTOT / (1 + FacCfgGn.PorIgv / 100).
   X-CTOUND = VtaCatCam.CtoTot.

   F-PreNet = ROUND(X-CTOUND * ( 1 + VtaCatCam.MrgUti / 100), 2).
   
   IF VtaCatCam.MonVta = 1 THEN DO:
      ASSIGN X-Mon = "S/."
             F-CTOPRM = VtaCatCam.CtoPrm[1]
             F-PreSol = VtaCatCam.PreVta[1].
   END.
   ELSE
      ASSIGN X-Mon = "US$"
             F-CTOPRM = VtaCatCam.CtoPrm[2]
             F-PreSol = ROUND(VtaCatCam.PreVta[1] * FacCfgGn.Tpocmb[1],2).
             

END.
/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
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
    WHEN 'Fechas':U THEN DO:
       &Scope KEY-PHRASE ( {&Filtro1} )
       RUN get-attribute ('SortBy-Case':U).
       CASE RETURN-VALUE:
         WHEN 'Codigo':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaCatCam.CodCia BY VtaCatCam.codmat
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Descripcion':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaCatCam.CodCia BY VtaCatCam.DesMat
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Marca':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaCatCam.CodCia BY VtaCatCam.DesMar
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Familia':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaCatCam.CodCia BY VtaCatCam.codfam BY VtaCatCam.CtoLis
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         OTHERWISE DO:
           &Undefine SORTBY-PHRASE
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END. /* OTHERWISE...*/
       END CASE.
    END. /* Fechas */
    OTHERWISE DO:
       &Scope KEY-PHRASE TRUE
       RUN get-attribute ('SortBy-Case':U).
       CASE RETURN-VALUE:
         WHEN 'Codigo':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaCatCam.CodCia BY VtaCatCam.codmat
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Descripcion':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaCatCam.CodCia BY VtaCatCam.DesMat
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Marca':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaCatCam.CodCia BY VtaCatCam.DesMar
           {&OPEN-QUERY-{&BROWSE-NAME}}
         END.
         WHEN 'Familia':U THEN DO:
           &Scope SORTBY-PHRASE BY VtaCatCam.CodCia BY VtaCatCam.codfam BY VtaCatCam.CtoLis
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Agrega-Articulo B-table-Win 
PROCEDURE Agrega-Articulo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE x-mats AS CHARACTER   NO-UNDO.
    RUN VTA\d-artic.w (OUTPUT x-mats).    
    FOR EACH Almmmatg WHERE Almmmatg.CodCia = s-CodCia
        AND LOOKUP(Almmmatg.CodMat,x-mats) > 0 NO-LOCK:
        FIND FIRST MATG WHERE MATG.CodCia = Almmmatg.CodCia
            AND MATG.CodMat = Almmmatg.CodMat NO-LOCK NO-ERROR.
        IF AVAILABLE MATG THEN NEXT.
        CREATE MATG.
        BUFFER-COPY Almmmatg TO MATG.
    END.
    RUN adm-open-query.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprime B-table-Win 
PROCEDURE Imprime :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE c-Copias AS INTEGER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN F-CodMat F-Filtro F-Provee CB-FchMod F-FchDes F-FchHas.
    END.

    RUN lib/Imprimir2.
    IF s-salida-impresion = 0 THEN RETURN.

    IF s-salida-impresion = 1 THEN 
        s-print-file = SESSION:TEMP-DIRECTORY +
        STRING(NEXT-VALUE(sec-arc,integral)) + ".prn".

    DO c-Copias = 1 TO s-nro-copias ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
        CASE s-salida-impresion:
            WHEN 1 OR WHEN 3 THEN
                OUTPUT STREAM REPORT TO VALUE(s-print-file) PAGED PAGE-SIZE 62.
            WHEN 2 THEN
                OUTPUT STREAM REPORT TO PRINTER PAGED PAGE-SIZE 62. /* Impresora */
        END CASE.
        PUT STREAM REPORT CONTROL CHR(27) "@" CHR(27) "C" CHR(66) CHR(15) CHR(27) "P".
        IF CB-FchMod = "Todas" THEN RUN Imprimir-Todos.
        ELSE RUN Imprimir-por-Fechas.
        PAGE STREAM REPORT.
        OUTPUT STREAM report CLOSE.
    END.
    OUTPUT STREAM report CLOSE.

    CASE s-salida-impresion:
        WHEN 1 OR WHEN 3 THEN DO:
            FRAME {&FRAME-NAME}:SENSITIVE = FALSE.
            RUN LIB/W-README.R(s-print-file).
            FRAME {&FRAME-NAME}:SENSITIVE = TRUE.
            IF s-salida-impresion = 1 THEN
                OS-DELETE VALUE(s-print-file).
        END.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir-por-Fechas B-table-Win 
PROCEDURE Imprimir-por-Fechas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR F-PRECOS AS DECIMAL INIT 0.
 DEFINE VAR F-PREVTA AS DECIMAL INIT 0.
 DEFINE VAR x-ctotot1 AS DECIMAL INIT 0.
 DEFINE VAR x-ctotot2 AS DECIMAL INIT 0.
 DEFINE VAR x-ctorep1 AS DECIMAL INIT 0.
 DEFINE VAR x-ctorep2 AS DECIMAL INIT 0.
 DEFINE VAR x-preuni1 AS DECIMAL INIT 0.
 DEFINE VAR x-preuni2 AS DECIMAL INIT 0.
 DEFINE VAR x-prenet1 AS DECIMAL INIT 0.
 DEFINE VAR x-prenet2 AS DECIMAL INIT 0.

 DEFINE FRAME f-cab
        MATG.codmat FORMAT "X(6)"        AT 02
        MATG.DesMat FORMAT "X(40)"       AT 09
        MATG.UndStk FORMAT "X(4)"        AT 50
        MATG.Pesmat FORMAT ">>,>>9.9999" 
        x-Ctotot1   FORMAT "->>>,>>9.99"
        x-Ctotot2   FORMAT "->>>,>>9.99"
        x-Ctorep2   FORMAT "->>>,>>9.99"
        x-Ctorep1   FORMAT "->>>,>>9.99"
        x-Preuni2   FORMAT "->>>,>>9.99"
        x-Preuni1   FORMAT "->>>,>>9.99"
        MATG.PorMax FORMAT "->>9.99"      
        x-prenet2   FORMAT "->>>,>>9.99"
        x-prenet1   FORMAT "->>>,>>9.99"
        MATG.MrgUti FORMAT "->>9.99"
        HEADER
        CHR(18) + CHR(27) + CHR(77) + CHR(14) + CHR(27) + CHR(71) + S-NOMCIA + CHR(20) + CHR(27) + CHR(80) + CHR(15) + CHR(27) + CHR(72) FORMAT "X(20)"
        CHR(27) + CHR(77) + CHR(15) + CHR(27) + CHR(72) + "LISTA DE PRECIOS" TO 72 FORMAT "X(21)" 
        "PAGINA : " TO 135 PAGE-NUMBER(REPORT) FORMAT "ZZ9" SKIP
        "FECHA : " TO 152 TODAY FORMAT "99/99/9999" SKIP(1)
        "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
        " CODIGO                                          UND         PESO       COSTO REPOSICION      COSTO.REP.UNITARIO         PRECIO UNITARIO      %            PRECIO NETO      %MRG." SKIP
        "ARTICULO       DESCRIPCION                       MED          KG.         US$      S/.           US$      S/.               US$     S/.      DCTO.        US$       S/.     UTIL." SKIP
        "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP


/*        999999 1234567890123456789012345678901234567890 12345678901234567 123 -99,999.9999 -99,999.9999  X  -99,999.9999 -999.99 -999.99 99/99/9999
                 9                                        50                68  71          83           96 99          110    117    124       */     
          WITH WIDTH 200 NO-BOX NO-LABEL NO-UNDERLINE STREAM-IO DOWN.

  FOR EACH MATG WHERE MATG.CodCia = S-CODCIA
                 AND  MATG.DesMat BEGINS F-Filtro
                 /*AND  MATG.TpoArt BEGINS R-Tipo*/
                 AND  MATG.CodPr1 BEGINS F-Provee  
                 AND  (MATG.FchmPre[2] >= F-FchDes) 
                 AND  (MATG.FchmPre[2] <= F-FchHas)
                BY MATG.CodFam BY MATG.OrdLis:
      IF MATG.MonVta = 1 THEN 
         ASSIGN X-Mon = "S/."
                F-CTOPRM = MATG.CtoPrm[1].
      ELSE
         ASSIGN X-Mon = "US$"
                F-CTOPRM = MATG.CtoPrm[2].
      F-MrgCom = ROUND((1 - (F-CTOPRM / MATG.PreBas)) * 100,2).
      
      IF VtaCatCam.AftIgv THEN DO:
         F-PRECOS = ROUND(MATG.CtoLis + (MATG.CtoLis * (FacCfgGn.PorIgv / 100)),2).
         F-PREVTA = ROUND(MATG.PreBas + (MATG.PreBas * (FacCfgGn.PorIgv / 100)),2).     
      END.
      ELSE DO:
         F-PRECOS = ROUND(MATG.CtoLis,2).
         F-PREVTA = ROUND(MATG.PreBas,2).     
      END.   
            
      DISPLAY  STREAM REPORT 
          MATG.codmat 
          MATG.DesMat 
          MATG.DesMar 
          X-Mon
          F-CTOPRM
/*        MATG.CtoLis */
          F-PRECOS
          MATG.TpoMrg 
/*        MATG.PreBas */
          F-PREVTA
          MATG.MrgUti
          MATG.PorMax 
          MATG.FchAct WITH FRAME F-Cab.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir-Todos B-table-Win 
PROCEDURE Imprimir-Todos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE VAR F-PRECOS AS DECIMAL INIT 0.
 DEFINE VAR F-PREVTA AS DECIMAL INIT 0.
 DEFINE VAR x-ctotot1 AS DECIMAL INIT 0.
 DEFINE VAR x-ctotot2 AS DECIMAL INIT 0.
 DEFINE VAR x-ctorep1 AS DECIMAL INIT 0.
 DEFINE VAR x-ctorep2 AS DECIMAL INIT 0.
 DEFINE VAR x-preuni1 AS DECIMAL INIT 0.
 DEFINE VAR x-preuni2 AS DECIMAL INIT 0.
 DEFINE VAR x-prenet1 AS DECIMAL INIT 0.
 DEFINE VAR x-prenet2 AS DECIMAL INIT 0.
 
 DEFINE FRAME f-cab
        MATG.codmat FORMAT "X(6)"        AT 02
        MATG.DesMat FORMAT "X(40)"       AT 09
        MATG.UndStk FORMAT "X(4)"        AT 50
        MATG.Pesmat FORMAT ">>,>>9.9999" 
        x-Ctotot2   FORMAT "->>>,>>9.99"
        x-Ctotot1   FORMAT "->>>,>>9.99"
        x-Ctorep2   FORMAT "->>>,>>9.99"
        x-Ctorep1   FORMAT "->>>,>>9.99"
        x-Preuni2   FORMAT "->>>,>>9.99"
        x-Preuni1   FORMAT "->>>,>>9.99"
        MATG.PorMax FORMAT "->>9.99"      
        x-prenet2   FORMAT "->>>,>>9.99"
        x-prenet1   FORMAT "->>>,>>9.99"
        MATG.MrgUti FORMAT "->>9.99"
        HEADER
        CHR(18) + CHR(27) + CHR(77) + CHR(14) + CHR(27) + CHR(71) + S-NOMCIA + CHR(20) + CHR(27) + CHR(80) + CHR(15) + CHR(27) + CHR(72) FORMAT "X(20)"
        CHR(27) + CHR(77) + CHR(15) + CHR(27) + CHR(72) + "LISTA DE PRECIOS" TO 72 FORMAT "X(21)" 
        "PAGINA : " TO 135 PAGE-NUMBER(REPORT) FORMAT "ZZ9" SKIP
        "FECHA : " TO 152 TODAY FORMAT "99/99/9999" SKIP(1)
        "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
        " CODIGO                                          UND         PESO       COSTO REPOSICION      COSTO.REP.UNITARIO         PRECIO UNITARIO      %            PRECIO NETO      %MRG." SKIP
        "ARTICULO       DESCRIPCION                       MED          KG.         US$      S/.           US$      S/.               US$     S/.      DCTO.        US$       S/.     UTIL." SKIP
        "---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------" SKIP
/*        999999 1234567890123456789012345678901234567890 12345678901234567 123 99,999.9999 99,999.9999  X  99,999.9999 999.99 999.99 99/99/9999
                 9                                        50                68  71          83           96 99          110    117    124       */     
        WITH WIDTH 200 NO-BOX NO-LABEL NO-UNDERLINE STREAM-IO DOWN.
 
  FOR EACH MATG WHERE MATG.CodCia = S-CODCIA
                 AND  MATG.DesMat BEGINS F-Filtro
                 /*AND  MATG.TpoArt BEGINS R-Tipo*/
                 AND  MATG.CodPr1 BEGINS F-Provee
                BREAK BY MATG.CodFam
                      BY MATG.OrdLis:
      
      IF FIRST-OF(MATG.CodFam) THEN DO:
         DOWN 1 STREAM REPORT WITH FRAME F-Cab.
         FIND Almtfami WHERE Almtfami.CodCia = s-codcia 
                        AND  Almtfami.codfam = MATG.CodFam 
                       NO-LOCK NO-ERROR.
         IF AVAILABLE Almtfami THEN 
            PUT STREAM REPORT CHR(27) + CHR(71) + MATG.CodFam Almtfami.desfam AT 10 CHR(27) + CHR(72).
         DOWN 1 STREAM REPORT WITH FRAME F-Cab.
      END.     
      IF MATG.MonVta = 1 THEN DO:
         x-ctotot1 = MATG.CtoTot.
         x-ctotot2 = ROUND(MATG.CtoTot / FacCfgGn.Tpocmb[1], 2).
         IF MATG.AftIgv THEN DO:
            x-ctotot1 = x-ctotot1 / ( 1 + FacCfgGn.PorIgv / 100 ).
            x-ctotot2 = x-ctotot2 / ( 1 + FacCfgGn.PorIgv / 100 ).
         END.
         x-Ctorep1 = x-ctotot1.
         x-Ctorep2 = x-ctotot2.
         IF MATG.Pesmat > 0 THEN DO:
            x-Ctorep1 = ROUND(x-ctotot1 / 1000 * MATG.Pesmat, 2).
            x-Ctorep2 = ROUND(x-ctotot2 / 1000 * MATG.Pesmat, 2).
         END.
         x-Preuni1 = MATG.PreVta[1].
         x-Preuni2 = ROUND(MATG.PreVta[1] / FacCfgGn.Tpocmb[1], 2).
         x-prenet1 = ROUND(x-Ctorep1 * (1 + MATG.MrgUti / 100), 2).
         x-prenet2 = ROUND(x-Ctorep2 * (1 + MATG.MrgUti / 100), 2).
         END.
      ELSE DO:
         x-ctotot1 = ROUND(MATG.CtoTot * FacCfgGn.Tpocmb[1], 2).
         x-ctotot2 = MATG.CtoTot.
         IF MATG.AftIgv THEN DO:
            x-ctotot1 = x-ctotot1 / ( 1 + FacCfgGn.PorIgv / 100 ).
            x-ctotot2 = x-ctotot2 / ( 1 + FacCfgGn.PorIgv / 100 ).
         END.
         x-Ctorep1 = x-ctotot1.
         x-Ctorep2 = x-ctotot2.
         IF MATG.Pesmat > 0 THEN DO:
            x-Ctorep1 = ROUND(x-ctotot1 / 1000 * MATG.Pesmat, 2).
            x-Ctorep2 = ROUND(x-ctotot2 / 1000 * MATG.Pesmat, 2).
         END.
         x-Preuni1 = ROUND(MATG.PreVta[1] * FacCfgGn.Tpocmb[1], 2).
         x-Preuni2 = MATG.PreVta[1].
         x-prenet1 = ROUND(x-Ctorep1 * (1 + MATG.MrgUti / 100), 2).
         x-prenet2 = ROUND(x-Ctorep2 * (1 + MATG.MrgUti / 100), 2).
      END.

      IF VtaCatCam.AftIgv THEN DO:
         F-PRECOS = ROUND(MATG.CtoLis + (MATG.CtoLis * (FacCfgGn.PorIgv / 100)),2).
         F-PREVTA = ROUND(MATG.PreBas + (MATG.PreBas * (FacCfgGn.PorIgv / 100)),2).     
      END.
      ELSE DO:
         F-PRECOS = ROUND(MATG.CtoLis,2).
         F-PREVTA = ROUND(MATG.PreBas,2).     
      END.   
      
      DISPLAY  STREAM REPORT 
          MATG.codmat 
          MATG.DesMat 
          MATG.UndStk
          MATG.Pesmat
          x-Ctotot1   
          x-Ctotot2   
          x-Ctorep1   
          x-Ctorep2   
          x-Preuni1   
          x-Preuni2   
          MATG.PorMax 
          x-prenet1   
          x-prenet2   
          MATG.MrgUti WITH FRAME F-Cab.
   END.
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
  DEFINE VARIABLE F-PreAnt LIKE VtaCatCam.PreBas  NO-UNDO.
  IF AVAILABLE VtaCatCam THEN F-PreAnt = VtaCatCam.PreBas.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-statement':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      VtaCatCam.MrgUti-A  = DECIMAL(VtaCatCam.MrgUti-A:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      VtaCatCam.Prevta[2] = DECIMAL(VtaCatCam.Prevta[2]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      VtaCatCam.MrgUti-B  = DECIMAL(VtaCatCam.MrgUti-B:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      VtaCatCam.PreVta[3] = DECIMAL(VtaCatCam.Prevta[3]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      VtaCatCam.MrgUti-C  = DECIMAL(VtaCatCam.MrgUti-C:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      VtaCatCam.PreVta[4] = DECIMAL(VtaCatCam.Prevta[4]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      VtaCatCam.CtoTot = DEC(VtaCatCam.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      VtaCatCam.CtoLis = DEC(VtaCatCam.CtoLis:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      VtaCatCam.CtoUnd = DEC(VtaCatCam.CtoLis:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      VtaCatCam.Dec__01 = DEC(VtaCatCam.Dec__01:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      VtaCatCam.PreOfi = DEC(VtaCatCam.PreOfi:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
      VtaCatCam.PreVta[1] = DEC(VtaCatCam.PreVta[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) .

  {vta/lispre-a2.i}

/* BLOQUEADO *****************************************************************************
  IF VtaCatCam.UndA <> "" THEN DO:
    FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
        AND  Almtconv.Codalter = VtaCatCam.UndA
        NO-LOCK NO-ERROR.
    F-FACTOR = Almtconv.Equival.
  END.

  VtaCatCam.PreVta[1] = IF VtaCatCam.Chr__02 = "T"  THEN VtaCatCam.PreVta[2] / F-FACTOR ELSE VtaCatCam.PreVta[1].

  IF VtaCatCam.AftIgv THEN 
    VtaCatCam.PreBas = ROUND((DEC(VtaCatCam.PreVta[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})) / ( 1 + FacCfgGn.PorIgv / 100), 6).

  VtaCatCam.MrgUti = ((VtaCatCam.Prevta[1] / VtaCatCam.Ctotot) - 1 ) * 100. 

  F-FACTOR = 1.
    /****   Busca el Factor de conversion   ****/
  IF VtaCatCam.UndA <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                       AND  Almtconv.Codalter = VtaCatCam.UndA
                      NO-LOCK NO-ERROR.
        F-FACTOR = Almtconv.Equival.
        VtaCatCam.Dsctos[1] =  (((VtaCatCam.Prevta[2] / F-FACTOR)/ VtaCatCam.Prevta[1]) - 1 ) * 100. 
  END.


  F-FACTOR = 1.
    /****   Busca el Factor de conversion   ****/
  IF VtaCatCam.UndB <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                       AND  Almtconv.Codalter = VtaCatCam.UndB
                      NO-LOCK NO-ERROR.
        F-FACTOR = Almtconv.Equival.
        VtaCatCam.Dsctos[2] =  (((VtaCatCam.Prevta[3] / F-FACTOR)/ VtaCatCam.Prevta[1]) - 1 ) * 100. 
  END.

  F-FACTOR = 1.
    /****   Busca el Factor de conversion   ****/
  IF VtaCatCam.UndC <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                       AND  Almtconv.Codalter = VtaCatCam.UndC
                      NO-LOCK NO-ERROR.
        F-FACTOR = Almtconv.Equival.
        VtaCatCam.Dsctos[3] =  (((VtaCatCam.Prevta[4] / F-FACTOR)/ VtaCatCam.Prevta[1]) - 1 ) * 100. 
  END.

* FIN DE BLOQUEO ***************************************************************** */


  IF F-PreAnt <> VtaCatCam.PreBas THEN VtaCatCam.FchmPre[3] = TODAY.
  VtaCatCam.FchmPre[1] = TODAY.
  VtaCatCam.Usuario = S-USER-ID.
  VtaCatCam.FchAct  = TODAY.
  
/*   RUN lib/logtabla ('VtaCatCam', VtaCatCam.codmat, 'WRITE').   /* Log de cambios */ */

  FIND Almtfami WHERE Almtfami.CodCia = S-CODCIA AND 
                      Almtfami.codfam = VtaCatCam.Codfam
                      No-LOCK NO-ERROR.
  IF AVAILABLE Almtfami THEN
  VtaCatCam.TpoCmb = Almtfami.Tpocmb.

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

  DO ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
     FIND MATG WHERE MATG.CodCia = S-CODCIA 
                AND  MATG.CodMat = F-CodMat 
               NO-LOCK NO-ERROR.
     IF AVAILABLE MATG THEN OUTPUT-VAR-1 = ROWID(MATG).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record B-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN dispatch IN THIS-PROCEDURE ('display-fields':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-imprime B-table-Win 
PROCEDURE local-imprime :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'imprime':U ) .
  /* Code placed here will execute AFTER standard behavior.    */

  RUN Imprime.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
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
  
  RUN Precio-de-Oficina.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Precio-de-Oficina B-table-Win 
PROCEDURE Precio-de-Oficina :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE fmot LIKE VtaCatCam.PreOfi.
DEFINE VARIABLE pre-ofi LIKE VtaCatCam.PreOfi.
DEFINE VARIABLE MrgMin LIKE VtaCatCam.MrgUti-A.
DEFINE VARIABLE MrgOfi LIKE VtaCatCam.MrgUti-A.

MaxCat = 0.
MaxVta = 0.
fmot   = 0.
MrgMin = 100.
MrgOfi = 0.
F-FACTOR = 1.
MaxCat = 4.
MaxVta = 3.


ASSIGN
    F-MrgUti-A = DECIMAL(VtaCatCam.MrgUti-A:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
    F-PreVta-A = DECIMAL(VtaCatCam.Prevta[2]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
    F-MrgUti-B = DECIMAL(VtaCatCam.MrgUti-B:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
    F-PreVta-B = DECIMAL(VtaCatCam.Prevta[3]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
    F-MrgUti-C = DECIMAL(VtaCatCam.MrgUti-C:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})
    F-PreVta-C = DECIMAL(VtaCatCam.Prevta[4]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

X-CTOUND = DECIMAL(VtaCatCam.CtoTot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}).

/****   Busca el Factor de conversion   ****/
IF VtaCatCam.Chr__01 <> "" THEN DO:
    FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                   AND  Almtconv.Codalter = VtaCatCam.Chr__01
                  NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Almtconv THEN DO:
       MESSAGE "Codigo de unidad no exixte" VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
    F-FACTOR = Almtconv.Equival.
END.
/*******************************************/

/* **********************************************************************
    NOTA IMPORTANTE: Cualquier cambio debe hacerse tambi�n
                    en LOGISTICA -> LIsta de precios por proveedor
************************************************************************* */                    

CASE VtaCatCam.Chr__02 :
    WHEN "T" THEN DO:        
        
        IF F-MrgUti-A < MrgMin AND F-MrgUti-A <> 0 THEN MrgMin = F-MrgUti-A.
        IF F-MrgUti-B < MrgMin AND F-MrgUti-B <> 0 THEN MrgMin = F-MrgUti-B.
        IF F-MrgUti-C < MrgMin AND F-MrgUti-C <> 0 THEN MrgMin = F-MrgUti-C.
        
        fmot = (1 + MrgMin / 100) / ((1 - MaxCat / 100) * (1 - MaxVta / 100)).
        
        pre-ofi = X-CTOUND * fmot * F-FACTOR .        
       
        MrgOfi = ROUND((fmot - 1) * 100, 6).

    END.
    WHEN "P" THEN DO:
       pre-ofi = DECIMAL(VtaCatCam.Prevta[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) * F-FACTOR.
       MrgOfi = ((DECIMAL(VtaCatCam.Prevta[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) / DECIMAL(VtaCatCam.Ctotot:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} )) - 1 ) * 100. 
        
    END. 
END.    


DO WITH FRAME {&FRAME-NAME}:
   DISPLAY MrgOfi @ VtaCatCam.Dec__01
           pre-ofi @ VtaCatCam.PreOfi 
           WITH BROWSE {&BROWSE-NAME}.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
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
  {src/adm/template/snd-list.i "VtaCatCam"}

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
    IF VtaCatCam.UndA <> "" THEN DO:
        FIND Almtconv WHERE Almtconv.CodUnid = VtaCatCam.UndBas
                       AND  Almtconv.Codalter = VtaCatCam.UndA
                      NO-LOCK NO-ERROR.
        F-FACTOR = Almtconv.Equival.
    END.

   CASE VtaCatCam.Chr__02 :
        WHEN "T" THEN DO:        
            
        END.
        WHEN "P" THEN DO:
           IF (DECIMAL(VtaCatCam.Prevta[1]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) * F-FACTOR ) < 
              (DECIMAL(VtaCatCam.Prevta[2]:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})) THEN DO:
              MESSAGE "Precio de lista Menor al Precio Venta A......" VIEW-AS ALERT-BOX ERROR.
              APPLY "ENTRY" TO VtaCatCam.Prevta[1].
              RETURN "ADM-ERROR".      
           END.                         
        END. 
    END.    

   IF DECIMAL(VtaCatCam.MrgUti-C:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) < 0 THEN DO:
      MESSAGE "Margen Utilidad C Negativo......" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO VtaCatCam.MrgUti-C.
      RETURN "ADM-ERROR".      
   END.

   IF DECIMAL(VtaCatCam.MrgUti-B:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) < 0 THEN DO:
      MESSAGE "Margen Utilidad B Negativo......" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO VtaCatCam.MrgUti-B.
      RETURN "ADM-ERROR".      
   END.

   IF DECIMAL(VtaCatCam.MrgUti-A:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) < 0 THEN DO:
      MESSAGE "Margen Utilidad A Negativo......" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO VtaCatCam.MrgUti-A.
      RETURN "ADM-ERROR".      
   END.

   IF DECIMAL(VtaCatCam.Monvta:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) = 0 THEN DO:
      MESSAGE "Codigo de Moneda Incorrecto......" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO VtaCatCam.MonVta.
      RETURN "ADM-ERROR".      
   END.

   IF DECIMAL(VtaCatCam.MrgUti-C:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > DECIMAL(VtaCatCam.MrgUti-B:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN DO:
      MESSAGE "Margen de util. C as mayor que el margen de util. B" SKIP
            "Margen Utilidad Incorrecto......" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO VtaCatCam.MrgUti-C.
      RETURN "ADM-ERROR".      
   END.

   IF DECIMAL(VtaCatCam.MrgUti-B:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) > DECIMAL(VtaCatCam.MrgUti-A:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}) THEN DO:
      MESSAGE "Margen de util. B as mayor que el margen de util. A" SKIP
            "Margen Utilidad Incorrecto......" VIEW-AS ALERT-BOX ERROR.
      APPLY "ENTRY" TO VtaCatCam.MrgUti-B.
      RETURN "ADM-ERROR".      
   END.
   
   IF VtaCatCam.clase:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} <> ''
   THEN DO:
        IF INDEX('ABCDEFGHIJKLMNOPQRSTUVWX', TRIM(VtaCatCam.clase:SCREEN-VALUE IN BROWSE {&BROWSE-NAME})) = 0
        THEN DO:
            MESSAGE "La Comisi�n Diferenciada debe ser una letra entre la A y la X"
                VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY":U TO VtaCatCam.clase.
            RETURN "ADM-ERROR":U.
        END.
   END.

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
IF NOT AVAILABLE VtaCatCam THEN RETURN "ADM-ERROR".
RETURN "OK".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
