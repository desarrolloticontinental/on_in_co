&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE COMPROBANTES NO-UNDO LIKE CcbCDocu.



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
DEFINE NEW SHARED VARIABLE input-var-1 AS CHARACTER.
DEFINE NEW SHARED VARIABLE input-var-2 AS CHARACTER.
DEFINE NEW SHARED VARIABLE input-var-3 AS CHARACTER.
DEFINE NEW SHARED VARIABLE output-var-1 AS ROWID.
DEFINE NEW SHARED VARIABLE output-var-2 AS CHARACTER.
DEFINE NEW SHARED VARIABLE output-var-3 AS CHARACTER.

/* Definici�n de variables locales */
DEFINE VARIABLE whpadre AS WIDGET-HANDLE.
DEFINE VARIABLE wh      AS WIDGET-HANDLE.
DEFINE VARIABLE curr-record AS RECID.
DEFINE VARIABLE f-cmpbte AS CHAR INIT "".
DEFINE VARIABLE X-MON    AS CHAR INIT "".
DEFINE VARIABLE X-EST    AS CHAR INIT "".
DEFINE SHARED VARIABLE S-CODCIA AS INTEGER.
DEFINE SHARED VARIABLE S-NOMCIA AS CHAR.
DEFINE SHARED VARIABLE S-CODDIV AS CHAR.

DEFINE VAR ss-CodDiv LIKE s-CodDiv.     /* ARTIFICIO */
DEFINE VAR F-ESTADO AS CHAR INIT 'P,C,A'.
DEFINE VAR F-CONDI  AS CHAR.
DEFINE VAR x-codven AS CHAR.
DEFINE VAR x-ImpTot AS DEC INIT 0.
DEFINE VAR cCodDiv  AS CHAR.

DEFINE VAR x-Usuario AS CHAR FORMAT 'x(10)'.
DEFINE VAR x-Fecha   AS DATE FORMAT '99/99/99'.
DEFINE VAR x-Hora    AS CHAR FORMAT 'x(5)'.
DEFINE VAR x-Nombre  AS CHAR FORMAT 'x(40)'.
/*DEFINE VAR lTodos    AS LOGICAL NO-UNDO INIT YES.*/

/* Solo para letras */
DEF VAR x-Banco AS CHAR NO-UNDO.
DEF VAR x-Ubicacion AS CHAR NO-UNDO.
DEF VAR x-Situacion AS CHAR NO-UNDO.

/* Preprocesadores para condiciones */
DEF VAR x-Comprobantes AS CHAR NO-UNDO.
x-Comprobantes = 'BOL,FAC,N/C,N/D,CHQ,CHC,LET,TCK,BD,A/R,A/C,DCO'.
&SCOPED-DEFINE CONDICION CcbcDocu.CodCia = S-CODCIA ~
        AND Ccbcdocu.divori = gn-divi.coddiv ~
        AND LOOKUP(TRIM(CcbcDocu.CodDoc), x-Comprobantes) > 0 ~
        AND (c-CmpBte = 'Todos' OR CcbcDocu.CodDoc = C-CMPBTE) ~
        AND (c-Serie = '' OR CcbcDocu.NroDoc = c-Serie) ~
        AND (CcbcDocu.FchDoc >= f-desde AND CcbCdocu.FchDoc <= f-hasta ) ~
        AND (x-FchVto-1 = ? OR CcbCDocu.FchVto >= x-FchVto-1) ~
        AND (x-FchVTo-2 = ? OR CcbCDocu.FchVto <= x-FchVto-2) ~
        AND LOOKUP(CcbcDocu.FlgEst, F-ESTADO) > 0 ~
        AND (FILL-IN-CodCli = '' OR CcbCDocu.CodCli = FILL-IN-CodCli) ~
        AND (F-CONDI = '' OR CcbcDocu.FmaPgo = F-CONDI) ~
        AND (x-CodVen = '' OR CcbCdocu.CodVen = x-codven) ~
        AND (x-CndCre = 'T' OR CcbCDocu.CndCre = x-CndCre) ~
        AND (xcodmod = 3 OR ccbcdocu.codmon = xcodmod)

&SCOPED-DEFINE CODIGO CcbcDocu.NroDoc
/* Preprocesadores para cada campo filtro */

&SCOPED-DEFINE FILTRO1 ( COMPROBANTES.Nomcli BEGINS FILL-IN-filtro )
&SCOPED-DEFINE FILTRO2 ( INDEX ( COMPROBANTES.Nomcli , FILL-IN-filtro ) <> 0 )


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

DEFINE VARIABLE X_TOT_IS AS DECI INIT 0.
DEFINE VARIABLE X_TOT_SS AS DECI INIT 0.
DEFINE VARIABLE X_TOT_ID AS DECI INIT 0.
DEFINE VARIABLE X_TOT_SD AS DECI INIT 0.
DEFINE BUFFER B-CDOCU FOR CcbCdocu.

/* RHC 15-03-04 SOLO PARA LA IMPRESION */
DEF TEMP-TABLE t-ccbcdocu LIKE ccbcdocu.

DEFINE IMAGE IMAGE-1 FILENAME "IMG\print" SIZE 5 BY 1.5.
DEF VAR FI-MENSAJE AS CHAR FORMAT "X(40)" NO-UNDO.

DEFINE FRAME F-Proceso
     IMAGE-1 AT ROW 1.5 COL 5
     "Espere un momento" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 1.5 COL 16 FONT 6
     "por favor ...." VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 2.5 COL 19 FONT 6
          SKIP
     Fi-Mensaje NO-LABEL FONT 6
     SKIP     
    WITH OVERLAY CENTERED KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         BGCOLOR 15 FGCOLOR 0 
         TITLE "Procesando ..." FONT 7.

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
&Scoped-define INTERNAL-TABLES COMPROBANTES CcbCDocu

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table COMPROBANTES.DivOri ~
COMPROBANTES.CodDoc COMPROBANTES.NroDoc COMPROBANTES.CodRef ~
COMPROBANTES.NroRef COMPROBANTES.NomCli COMPROBANTES.FmaPgo ~
COMPROBANTES.CodVen COMPROBANTES.FchDoc COMPROBANTES.FchCbd ~
COMPROBANTES.FchVto COMPROBANTES.FchCan X-MON @ X-MON CcbCDocu.ImpTot ~
CcbCDocu.SdoAct CcbCDocu.ImpDto fEstado() @ X-EST ~
CcbCDocu.ImpTot + fAdelanto(CcbCDocu.CodCia,CcbCDocu.CodDoc,CcbCDocu.NroDoc) @ x-ImpTot ~
fUsuario() @ x-Usuario fFecha() @ x-Fecha fHora() @ x-Hora ~
fNombre() @ x-Nombre fUbicacion() @ x-Ubicacion fBanco() @ x-Banco ~
fSituacion() @ x-Situacion CcbCDocu.NroSal CcbCDocu.CodRef CcbCDocu.NroRef 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH COMPROBANTES WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST CcbCDocu OF COMPROBANTES NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH COMPROBANTES WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      FIRST CcbCDocu OF COMPROBANTES NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table COMPROBANTES CcbCDocu
&Scoped-define FIRST-TABLE-IN-QUERY-br_table COMPROBANTES
&Scoped-define SECOND-TABLE-IN-QUERY-br_table CcbCDocu


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-58 RECT-59 FILL-IN-codigo ~
FILL-IN-CodCli CMB-filtro FILL-IN-filtro BUTTON-4 B-Imprimir ~
BUTTON-Division xcodmod C-CMPBTE c-Serie f-desde f-hasta x-FchVto-1 ~
x-FchVto-2 BUTTON-19 COMBO-BOX-5 f-codven F-FMAPGO br_table BUTTON-1 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-codigo FILL-IN-CodCli CMB-filtro ~
FILL-IN-filtro x-CodDiv xcodmod C-CMPBTE c-Serie f-desde f-hasta x-CndCre ~
x-FchVto-1 x-FchVto-2 COMBO-BOX-5 f-codven F-nomven F-FMAPGO F-CndVta ~
f-totsol f-totdol f-sdosol f-sdodol FILL-IN-Mensaje 

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
Nombres que Inicien con|y||COMPROBANTES.NomCli
Nombres que Contengan|y||COMPROBANTES.NomCli
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "Nombres que Inicien con,Nombres que Contengan",
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
<SORTBY-OPTIONS></SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ,
     Sort-Case = ':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).

/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES></FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAdelanto L-table-Win 
FUNCTION fAdelanto RETURNS DECIMAL
  ( INPUT pCodCia AS INTEGER, INPUT pCodDoc AS CHAR, INPUT pNroDoc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fBanco L-table-Win 
FUNCTION fBanco RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEstado L-table-Win 
FUNCTION fEstado RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEstado-1 L-table-Win 
FUNCTION fEstado-1 RETURNS CHARACTER
  ( INPUT pFlgEst AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFecha L-table-Win 
FUNCTION fFecha RETURNS DATE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fHora L-table-Win 
FUNCTION fHora RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fNombre L-table-Win 
FUNCTION fNombre RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSituacion L-table-Win 
FUNCTION fSituacion RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fUbicacion L-table-Win 
FUNCTION fUbicacion RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fUsuario L-table-Win 
FUNCTION fUsuario RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Imprimir 
     IMAGE-UP FILE "img\print":U
     LABEL "Button 2" 
     SIZE 5 BY 1.19.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "img\auditor":U
     LABEL "Button 1" 
     SIZE 5.29 BY 1.46.

DEFINE BUTTON BUTTON-19 
     LABEL "APLICAR FILTRO" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "img\excel":U
     LABEL "Button 4" 
     SIZE 5 BY 1.35 TOOLTIP "Migrar a Excel".

DEFINE BUTTON BUTTON-Division 
     LABEL "..." 
     SIZE 4 BY .77 TOOLTIP "Selecciona Divisiones".

DEFINE VARIABLE CMB-filtro AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Todos" 
     DROP-DOWN-LIST
     SIZE 21.29 BY 1
     BGCOLOR 15 FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE COMBO-BOX-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     LABEL "Estado" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "Todos","Pendiente","Cancelado","Cobranza Dudosa","Anulado","Total Neto" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE c-Serie AS CHARACTER FORMAT "X(256)":U 
     LABEL "Serie" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-CndVta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37.72 BY .69 NO-UNDO.

DEFINE VARIABLE f-codven AS CHARACTER FORMAT "X(3)":U 
     LABEL "Vend" 
     VIEW-AS FILL-IN 
     SIZE 5.14 BY .69
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE f-desde AS DATE FORMAT "99/99/9999":U 
     LABEL "Emitidos desde" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-FMAPGO AS CHARACTER FORMAT "XXX":U 
     LABEL "Cnd.Vta" 
     VIEW-AS FILL-IN 
     SIZE 5.86 BY .69
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE f-hasta AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-nomven AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .69 NO-UNDO.

DEFINE VARIABLE f-sdodol AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY .69
     BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE f-sdosol AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY .69
     BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE f-totdol AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY .69
     BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE f-totsol AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 15 BY .69
     BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE FILL-IN-CodCli AS CHARACTER FORMAT "X(11)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-codigo AS CHARACTER FORMAT "x(12)":U 
     LABEL "C�digo" 
     VIEW-AS FILL-IN 
     SIZE 10.29 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-filtro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-Mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 53 BY .81 NO-UNDO.

DEFINE VARIABLE x-CodDiv AS CHARACTER FORMAT "X(256)":U 
     LABEL "Divisi�n" 
     VIEW-AS FILL-IN 
     SIZE 63 BY .81 NO-UNDO.

DEFINE VARIABLE x-FchVto-1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Vencidos desde" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE x-FchVto-2 AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE x-CndCre AS CHARACTER INITIAL "T" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todos", "T",
"Por Devolucion", "D",
"Otros", "N"
     SIZE 14 BY 2.12 NO-UNDO.

DEFINE VARIABLE xcodmod AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Soles", 1,
"Dolares", 2,
"Todos", 3
     SIZE 9 BY 2.12 NO-UNDO.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 128 BY 1.15.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 128 BY 5.96.

DEFINE VARIABLE C-CMPBTE AS CHARACTER INITIAL "Todos" 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Todos","Todos",
                     "FAC","FAC",
                     "BOL","BOL",
                     "TCK","TCK",
                     "LET","LET",
                     "N/D","N/D",
                     "N/C","N/C",
                     "CHQ","CHQ",
                     "CHC","CHC",
                     "BD","BD",
                     "A/R","A/R",
                     "A/C","A/C",
                     "DCO","DCO" 
     SIZE 10 BY 3.65 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      COMPROBANTES, 
      CcbCDocu SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table L-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      COMPROBANTES.DivOri FORMAT "x(5)":U
      COMPROBANTES.CodDoc COLUMN-LABEL "Tipo" FORMAT "x(3)":U
      COMPROBANTES.NroDoc FORMAT "XXX-XXXXXXXX":U
      COMPROBANTES.CodRef COLUMN-LABEL "Refer." FORMAT "x(3)":U
      COMPROBANTES.NroRef FORMAT "X(12)":U
      COMPROBANTES.NomCli FORMAT "x(40)":U
      COMPROBANTES.FmaPgo COLUMN-LABEL "Cnd.!Venta" FORMAT "X(3)":U
      COMPROBANTES.CodVen COLUMN-LABEL "Ven." FORMAT "x(5)":U
      COMPROBANTES.FchDoc COLUMN-LABEL "     Fecha    !    Emision" FORMAT "99/99/9999":U
      COMPROBANTES.FchCbd COLUMN-LABEL "Fecha!Recepci�n" FORMAT "99/99/9999":U
      COMPROBANTES.FchVto FORMAT "99/99/9999":U
      COMPROBANTES.FchCan COLUMN-LABEL "Fecha de!Cancelacion" FORMAT "99/99/9999":U
      X-MON @ X-MON COLUMN-LABEL "Mon." FORMAT "XXXX":U
      CcbCDocu.ImpTot FORMAT "->>>,>>9.99":U
      CcbCDocu.SdoAct COLUMN-LABEL "Saldo" FORMAT "->>>,>>9.99":U
      CcbCDocu.ImpDto COLUMN-LABEL "Imp. Descuento" FORMAT "->>>,>>9.99":U
            WIDTH 11
      fEstado() @ X-EST COLUMN-LABEL "Estado" FORMAT "XXX":U
      CcbCDocu.ImpTot + fAdelanto(CcbCDocu.CodCia,CcbCDocu.CodDoc,CcbCDocu.NroDoc) @ x-ImpTot COLUMN-LABEL "Importe SIN!Adelanto" FORMAT "->>>,>>9.99":U
      fUsuario() @ x-Usuario COLUMN-LABEL "Anulado por" FORMAT "x(10)":U
      fFecha() @ x-Fecha COLUMN-LABEL "Fecha" FORMAT "99/99/99":U
            WIDTH 6.57
      fHora() @ x-Hora COLUMN-LABEL "Hora" FORMAT "x(5)":U
      fNombre() @ x-Nombre COLUMN-LABEL "Motivo de anulaci�n" FORMAT "x(40)":U
      fUbicacion() @ x-Ubicacion COLUMN-LABEL "Ubicacion" FORMAT "x(10)":U
      fBanco() @ x-Banco COLUMN-LABEL "Banco" FORMAT "x(15)":U
      fSituacion() @ x-Situacion COLUMN-LABEL "Situaci�n" FORMAT "x(15)":U
      CcbCDocu.NroSal COLUMN-LABEL "Numero Unico" FORMAT "X(15)":U
      CcbCDocu.CodRef COLUMN-LABEL "Refer." FORMAT "x(4)":U
      CcbCDocu.NroRef COLUMN-LABEL "Numero Refer." FORMAT "X(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 139 BY 9.12
         BGCOLOR 15 FGCOLOR 0 FONT 4 ROW-HEIGHT-CHARS .5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN-codigo AT ROW 1.19 COL 22 COLON-ALIGNED
     FILL-IN-CodCli AT ROW 1.19 COL 38 COLON-ALIGNED
     CMB-filtro AT ROW 1.19 COL 51 NO-LABEL
     FILL-IN-filtro AT ROW 1.19 COL 73 NO-LABEL
     BUTTON-4 AT ROW 1.19 COL 131
     B-Imprimir AT ROW 1.19 COL 136
     x-CodDiv AT ROW 2.35 COL 22 COLON-ALIGNED WIDGET-ID 74
     BUTTON-Division AT ROW 2.35 COL 87 WIDGET-ID 76
     xcodmod AT ROW 2.35 COL 120 NO-LABEL
     C-CMPBTE AT ROW 3.31 COL 24 NO-LABEL WIDGET-ID 6
     c-Serie AT ROW 3.31 COL 39 COLON-ALIGNED
     f-desde AT ROW 3.31 COL 81 COLON-ALIGNED
     f-hasta AT ROW 3.31 COL 97 COLON-ALIGNED
     x-CndCre AT ROW 4.27 COL 41 NO-LABEL
     x-FchVto-1 AT ROW 4.27 COL 81 COLON-ALIGNED
     x-FchVto-2 AT ROW 4.27 COL 97 COLON-ALIGNED
     BUTTON-19 AT ROW 4.65 COL 114 WIDGET-ID 86
     COMBO-BOX-5 AT ROW 5.23 COL 81 COLON-ALIGNED
     f-codven AT ROW 6.19 COL 81 COLON-ALIGNED
     F-nomven AT ROW 6.19 COL 87 COLON-ALIGNED NO-LABEL
     F-FMAPGO AT ROW 7.15 COL 22 COLON-ALIGNED
     F-CndVta AT ROW 7.15 COL 29.14 COLON-ALIGNED NO-LABEL
     br_table AT ROW 8.12 COL 2
     BUTTON-1 AT ROW 17.77 COL 99.72
     f-totsol AT ROW 17.85 COL 81.72 RIGHT-ALIGNED NO-LABEL
     f-totdol AT ROW 17.85 COL 83.72 NO-LABEL
     f-sdosol AT ROW 18.62 COL 67.72 NO-LABEL
     f-sdodol AT ROW 18.62 COL 83.72 NO-LABEL
     FILL-IN-Mensaje AT ROW 18.88 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     "Moneda:" VIEW-AS TEXT
          SIZE 6 BY .5 AT ROW 2.54 COL 113 WIDGET-ID 78
     "Saldo Total" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 18.62 COL 57.72
     "N/C:" VIEW-AS TEXT
          SIZE 4 BY .5 AT ROW 4.27 COL 37 WIDGET-ID 80
     "Comprobante:" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 3.5 COL 14 WIDGET-ID 8
     "Documentos en US$/" VIEW-AS TEXT
          SIZE 15 BY .5 AT ROW 17.27 COL 83.72
     "Filtrar x" VIEW-AS TEXT
          SIZE 7 BY .65 AT ROW 2.35 COL 5
          FONT 6
     "Buscar x" VIEW-AS TEXT
          SIZE 7.86 BY .62 AT ROW 1.38 COL 5
          FONT 6
     "Importe Total" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 17.85 COL 57.72
     "(*)  Click Derecho - Visualiza Pagos" VIEW-AS TEXT
          SIZE 29 BY .73 AT ROW 17.35 COL 2
          FONT 6
     "Documentos en S/." VIEW-AS TEXT
          SIZE 14 BY .5 AT ROW 17.27 COL 67.72
     "(*)  Doble Click - Visualiza Detalle" VIEW-AS TEXT
          SIZE 29 BY .73 AT ROW 18 COL 2.14
          FONT 6
     RECT-58 AT ROW 1 COL 2 WIDGET-ID 82
     RECT-59 AT ROW 2.15 COL 2 WIDGET-ID 84
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
   Temp-Tables and Buffers:
      TABLE: COMPROBANTES T "?" NO-UNDO INTEGRAL CcbCDocu
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
  CREATE WINDOW L-table-Win ASSIGN
         HEIGHT             = 18.73
         WIDTH              = 142.29.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB L-table-Win 
/* ************************* Included-Libraries *********************** */

{src/bin/_prns.i}
{src/adm/method/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW L-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table F-CndVta F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:PRIVATE-DATA     = 
                "sdfsdfsdfsdfsdf".

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2.

/* SETTINGS FOR COMBO-BOX CMB-filtro IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN F-CndVta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-nomven IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-sdodol IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN f-sdosol IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN f-totdol IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN f-totsol IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-filtro IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-Mensaje IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET x-CndCre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN x-CodDiv IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "Temp-Tables.COMPROBANTES,INTEGRAL.CcbCDocu OF Temp-Tables.COMPROBANTES"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
     _FldNameList[1]   = Temp-Tables.COMPROBANTES.DivOri
     _FldNameList[2]   > Temp-Tables.COMPROBANTES.CodDoc
"COMPROBANTES.CodDoc" "Tipo" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.COMPROBANTES.NroDoc
"COMPROBANTES.NroDoc" ? "XXX-XXXXXXXX" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.COMPROBANTES.CodRef
"COMPROBANTES.CodRef" "Refer." ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.COMPROBANTES.NroRef
"COMPROBANTES.NroRef" ? "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.COMPROBANTES.NomCli
"COMPROBANTES.NomCli" ? "x(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.COMPROBANTES.FmaPgo
"COMPROBANTES.FmaPgo" "Cnd.!Venta" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.COMPROBANTES.CodVen
"COMPROBANTES.CodVen" "Ven." "x(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.COMPROBANTES.FchDoc
"COMPROBANTES.FchDoc" "     Fecha    !    Emision" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.COMPROBANTES.FchCbd
"COMPROBANTES.FchCbd" "Fecha!Recepci�n" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   = Temp-Tables.COMPROBANTES.FchVto
     _FldNameList[12]   > Temp-Tables.COMPROBANTES.FchCan
"COMPROBANTES.FchCan" "Fecha de!Cancelacion" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"X-MON @ X-MON" "Mon." "XXXX" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > integral.CcbCDocu.ImpTot
"CcbCDocu.ImpTot" ? "->>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > integral.CcbCDocu.SdoAct
"CcbCDocu.SdoAct" "Saldo" "->>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > integral.CcbCDocu.ImpDto
"CcbCDocu.ImpDto" "Imp. Descuento" "->>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"fEstado() @ X-EST" "Estado" "XXX" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"CcbCDocu.ImpTot + fAdelanto(CcbCDocu.CodCia,CcbCDocu.CodDoc,CcbCDocu.NroDoc) @ x-ImpTot" "Importe SIN!Adelanto" "->>>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"fUsuario() @ x-Usuario" "Anulado por" "x(10)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"fFecha() @ x-Fecha" "Fecha" "99/99/99" ? ? ? ? ? ? ? no ? no no "6.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"fHora() @ x-Hora" "Hora" "x(5)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"fNombre() @ x-Nombre" "Motivo de anulaci�n" "x(40)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"fUbicacion() @ x-Ubicacion" "Ubicacion" "x(10)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"fBanco() @ x-Banco" "Banco" "x(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"fSituacion() @ x-Situacion" "Situaci�n" "x(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > INTEGRAL.CcbCDocu.NroSal
"CcbCDocu.NroSal" "Numero Unico" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > INTEGRAL.CcbCDocu.CodRef
"CcbCDocu.CodRef" "Refer." "x(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > INTEGRAL.CcbCDocu.NroRef
"CcbCDocu.NroRef" "Numero Refer." "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

&Scoped-define SELF-NAME B-Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Imprimir L-table-Win
ON CHOOSE OF B-Imprimir IN FRAME F-Main /* Button 2 */
DO:
   ASSIGN xcodmod.
   /*RUN bin/_prnctr.p.*/
   RUN lib/Imprimir2.
   IF s-salida-impresion = 0 THEN RETURN.
   
   IF s-salida-impresion = 1 THEN 
      s-print-file = SESSION:TEMP-DIRECTORY + STRING(NEXT-VALUE(sec-arc,integral)) + ".scr".
   
  RUN Imprimir.
  
  CASE s-salida-impresion:
       WHEN 1 OR WHEN 3 THEN RUN LIB/D-README.R(s-print-file). 
  END CASE.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
DO:
    ASSIGN
        ss-CodDiv = s-CodDiv
        s-CodDiv = x-CodDiv.
    /*RUN VTA\D-cmpbte (Ccbcdocu.NroDoc,CcbcDocu.CodDoc).*/
    RUN Vta2/dcomprobantes (Ccbcdocu.NroDoc,CcbcDocu.CodDoc).
    s-CodDiv = ss-CodDiv.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table L-table-Win
ON RIGHT-MOUSE-CLICK OF br_table IN FRAME F-Main
DO:

    ASSIGN x-CodDiv.
    
    IF CAN-FIND(FIRST CCBDMOV WHERE
        CCBDMOV.codcia = s-codcia AND
        CCBDMOV.coddiv = x-CodDiv AND
        CCBDMOV.coddoc = Ccbcdocu.CodDoc AND
        CCBDMOV.nrodoc = Ccbcdocu.NroDoc) THEN
        RUN VTA\D-cmpbt2.r(
            Ccbcdocu.CodDoc,
            CcbcDocu.NroDoc,
            CcbcDocu.NroPed,
            CcbcDocu.SdoAct,
            Ccbcdocu.Imptot,
            CcbcDocu.NroRef,
            x-CodDiv
            ).
    ELSE
        RUN VTA\D-cmpbt1.r(
            Ccbcdocu.CodDoc,
            CcbcDocu.NroDoc,
            CcbcDocu.NroPed,
            CcbcDocu.SdoAct,
            Ccbcdocu.Imptot,
            CcbcDocu.NroRef
            ).

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


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 L-table-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
 RUN Calcula-Importe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 L-table-Win
ON CHOOSE OF BUTTON-19 IN FRAME F-Main /* APLICAR FILTRO */
DO:
  ASSIGN
      x-CodDiv xcodmod 
      C-CMPBTE c-Serie x-CndCre 
      f-desde f-hasta x-FchVto-1 x-FchVto-2
      COMBO-BOX-5  
      f-codven F-FMAPGO FILL-IN-CodCli FILL-IN-codigo.
  RUN Carga-Temporal.
  RUN dispatch IN THIS-PROCEDURE ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 L-table-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* Button 4 */
DO:  
/*   ASSIGN             */
/*       xcodmod        */
/*       lTodos = YES.  */
/*   ASSIGN             */
/*       FILL-IN-filtro */
/*       CMB-filtro     */
/*       C-CMPBTE       */
/*       x-CndCre       */
/*       xCodMod.       */
/*   lTodos = NO.       */
  RUN Excel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Division
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Division L-table-Win
ON CHOOSE OF BUTTON-Division IN FRAME F-Main /* ... */
DO:
    ASSIGN x-CodDiv.
    RUN gn/d-filtro-divisiones (INPUT-OUTPUT x-CodDiv, "SELECCIONE LAS DIVISIONES").
    DISPLAY x-CodDiv WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB-filtro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB-filtro L-table-Win
ON VALUE-CHANGED OF CMB-filtro IN FRAME F-Main
DO:
    IF CMB-filtro = CMB-filtro:SCREEN-VALUE AND FILL-IN-filtro = FILL-IN-filtro:SCREEN-VALUE        
        THEN RETURN.
    ASSIGN FILL-IN-filtro CMB-filtro.
    IF CMB-filtro = "Todos" THEN DO:
       IF C-CMPBTE <> "" THEN RUN set-attribute-list('Key-Name=Nombres que inicien con').
       ELSE RUN set-attribute-list('Key-Name=?').
    END.
    ELSE RUN set-attribute-list('Key-Name=' + CMB-filtro).
    RUN dispatch IN THIS-PROCEDURE ('open-query':U).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-5 L-table-Win
ON VALUE-CHANGED OF COMBO-BOX-5 IN FRAME F-Main /* Estado */
DO:
  ASSIGN COMBO-BOX-5.
  CASE COMBO-BOX-5:
       WHEN "Pendiente" THEN 
          ASSIGN F-ESTADO = "P".
       WHEN "Cancelado" THEN 
          ASSIGN F-ESTADO = "C".
      WHEN "Cobranza Dudosa" THEN
          ASSIGN F-ESTADO = "J".
       WHEN "Anulado" THEN 
          ASSIGN F-ESTADO = "A".
       WHEN "Total Neto" THEN 
          ASSIGN F-ESTADO = "P,C".
       OTHERWISE
          ASSIGN F-ESTADO = "P,C,A,J".        
  END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-codven
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-codven L-table-Win
ON LEAVE OF f-codven IN FRAME F-Main /* Vend */
DO:
   x-codven = SELF:SCREEN-VALUE.
   
   IF SELF:SCREEN-VALUE <> "" THEN DO:
      F-nomven:SCREEN-VALUE = "".
      x-codven = SELF:SCREEN-VALUE.
      FIND gn-ven WHERE 
           gn-ven.Codven = SELF:SCREEN-VALUE 
           NO-LOCK NO-ERROR.
      IF AVAILABLE gn-ven THEN DO:
         F-nomven:SCREEN-VALUE = gn-ven.nomven.
      END.
   END.
   ELSE F-nomven:SCREEN-VALUE = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-codven L-table-Win
ON MOUSE-SELECT-DBLCLICK OF f-codven IN FRAME F-Main /* Vend */
DO:
  RUN lkup\c-vende.r("Maestro de Vendedores").
  IF output-var-2 = ? THEN output-var-2 = "".
  F-codven:SCREEN-VALUE = output-var-2.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-FMAPGO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-FMAPGO L-table-Win
ON LEAVE OF F-FMAPGO IN FRAME F-Main /* Cnd.Vta */
DO:
   F-CONDI = F-FMAPGO:SCREEN-VALUE.
   
   IF F-FMAPGO:SCREEN-VALUE <> "" THEN DO:
      F-CndVta:SCREEN-VALUE = "".
      F-CONDI = F-FMAPGO:SCREEN-VALUE.
      FIND gn-convt WHERE 
           gn-convt.Codig = F-FMAPGO:SCREEN-VALUE 
           NO-LOCK NO-ERROR.
      IF AVAILABLE gn-convt THEN DO:
         F-CNDVTA:SCREEN-VALUE = gn-convt.Nombr.
      END.
   END.
   ELSE F-CndVta:SCREEN-VALUE = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-FMAPGO L-table-Win
ON MOUSE-SELECT-DBLCLICK OF F-FMAPGO IN FRAME F-Main /* Cnd.Vta */
DO:
  RUN lkup\c-condvt.r("CONDICIONES DE VENTA").
  IF output-var-2 = ? THEN output-var-2 = "".
  F-FMAPGO:SCREEN-VALUE = output-var-2.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-CodCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodCli L-table-Win
ON LEAVE OF FILL-IN-CodCli IN FRAME F-Main /* Cliente */
DO:
  ASSIGN {&SELF-NAME}.

  DEFINE VAR lFiltro AS LOG.
  lFiltro = YES.
  IF FILL-IN-CodCli <> "" THEN DO:
      FIND FIRST gn-clie WHERE gn-clie.codcia = 1 AND 
                                gn-clie.codcli = FILL-IN-CodCli NO-LOCK NO-ERROR.
      IF AVAILABLE gn-clie THEN DO:
          IF gn-clie.libre_l02 = YES THEN DO:
              lfiltro = NO.
          END.
      END.
  END.
  IF lFiltro = YES THEN DO:
      APPLY "VALUE-CHANGED" TO CMB-filtro.
  END.
  ELSE DO:
      MESSAGE "Use la aplicacion OPENORANGE para consultar informacion de este Cliente".
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CodCli L-table-Win
ON LEFT-MOUSE-DBLCLICK OF FILL-IN-CodCli IN FRAME F-Main /* Cliente */
DO:
  ASSIGN
    input-var-1 = ''
    input-var-2 = ''
    input-var-3 = ''
    output-var-1 = ?
    output-var-2 = ''
    output-var-3 = ''.
  RUN lkup/c-client ('Maestro de Clientes').
  IF output-var-1 <> ?
  THEN DO:
    ASSIGN SELF:SCREEN-VALUE = output-var-2.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-codigo L-table-Win
ON LEAVE OF FILL-IN-codigo IN FRAME F-Main /* C�digo */
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
OR "RETURN":U OF FILL-IN-filtro
DO:
    APPLY "VALUE-CHANGED" TO CMB-filtro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK L-table-Win 


/* ***************************  Main Block  *************************** */
ASSIGN f-desde = TODAY
       f-hasta = TODAY.
       xcodmod.
       
ON FIND OF Ccbcdocu
DO:

    IF CcbcDocu.CodMon = 1 THEN
        ASSIGN
            X-MON = "S/." .
    ELSE
        ASSIGN
            X-MON = "US$" .
            
    IF CcbCDocu.FlgEst = "P" THEN
        ASSIGN
            X-EST = "PEN" .
    ELSE
       IF CcbCDocu.FlgEst = "C" THEN
          ASSIGN
              X-EST = "CAN" .    
       ELSE       
          IF CcbcDocu.FlgEst = "A" THEN
             ASSIGN
                 X-EST = "ANU" .            
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
    WHEN 'Nombres que Inicien con':U THEN DO:
       &Scope KEY-PHRASE ( {&Filtro1} )
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* Nombres que Inicien con */
    WHEN 'Nombres que Contengan':U THEN DO:
       &Scope KEY-PHRASE ( {&Filtro2} )
       {&OPEN-QUERY-{&BROWSE-NAME}}
    END. /* Nombres que Contengan */
    OTHERWISE DO:
       &Scope KEY-PHRASE TRUE
       {&OPEN-QUERY-{&BROWSE-NAME}}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calcula-Importe L-table-Win 
PROCEDURE Calcula-Importe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR x-Factor AS INT INIT 1 NO-UNDO.
    X_TOT_IS = 0.
    X_TOT_SS = 0.
    X_TOT_ID = 0.
    X_TOT_SD = 0.

 GET FIRST {&BROWSE-NAME}.
 DO WHILE AVAILABLE(Ccbcdocu):
    x-Factor = 1.
    FIND FacDocum OF CcbCDocu NO-LOCK NO-ERROR.
    IF AVAILABLE FacDocum
    THEN x-Factor = IF FacDocum.TpoDoc = NO THEN -1 ELSE 1.
    IF ccbcdocu.flgest = 'A' THEN x-Factor = 0.     /* ANULADOS */
    IF Ccbcdocu.CodMon = 1 THEN
        ASSIGN
            X_TOT_IS = X_TOT_IS + ccbcdocu.ImpTot * x-Factor
            X_TOT_SS = X_TOT_SS + ccbcdocu.Sdoact * x-Factor.
    ELSE
        ASSIGN
            X_TOT_ID = X_TOT_ID + ccbcdocu.ImpTot * x-Factor
            X_TOT_SD = X_TOT_SD + ccbcdocu.Sdoact * x-Factor.

    GET NEXT {&BROWSE-NAME}.
 END.
 
 DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
         f-totsol = X_tot_is
         f-totdol = X_tot_id
         f-sdosol = X_tot_ss
         f-sdodol = X_tot_sd.
       DISPLAY f-totsol
               f-totdol
               f-sdosol
               f-sdodol.
    
 END.
 RUN dispatch IN THIS-PROCEDURE ('open-query':U).

END PROCEDURE.

/*
  DEF VAR x-Factor AS INT INIT 1 NO-UNDO.
    X_TOT_IS = 0.
    X_TOT_SS = 0.
    X_TOT_ID = 0.
    X_TOT_SD = 0.

 GET FIRST {&BROWSE-NAME}.
 DO WHILE AVAILABLE(Ccbcdocu):
    x-Factor = 1.
    FIND FacDocum OF CcbCDocu NO-LOCK NO-ERROR.
    IF AVAILABLE FacDocum
    THEN x-Factor = IF FacDocum.TpoDoc = NO THEN -1 ELSE 1.
    IF ccbcdocu.flgest = 'A' THEN x-Factor = 0.     /* ANULADOS */
    IF Ccbcdocu.CodMon = 1 THEN
        ASSIGN
            X_TOT_IS = X_TOT_IS + ccbcdocu.ImpTot * x-Factor
            X_TOT_SS = X_TOT_SS + ccbcdocu.Sdoact * x-Factor.
    ELSE
        ASSIGN
            X_TOT_ID = X_TOT_ID + ccbcdocu.ImpTot * x-Factor
            X_TOT_SD = X_TOT_SD + ccbcdocu.Sdoact * x-Factor.

    GET NEXT {&BROWSE-NAME}.
 END.
 
 DO WITH FRAME {&FRAME-NAME}:
       DISPLAY X_TOT_IS @ f-totsol
               X_TOT_ID @ f-totdol
               X_TOT_SS @ f-sdosol
               X_TOT_SD @ f-sdodol.
    
 END.
*/

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
        output-var-2 = STRING( {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.NroDoc, "999-999999" )
        output-var-3 = {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}.nomcli.

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Excel L-table-Win 
PROCEDURE Carga-Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    FOR EACH CcbcDocu USE-INDEX Llave10
        WHERE CcbcDocu.CodCia = S-CODCIA 
        AND CcbCDocu.DivOri BEGINS ''
        AND (CcbcDocu.FchDoc >= f-desde AND CcbCdocu.FchDoc <= f-hasta ) 
        AND (x-FchVto-1 = ? OR CcbCDocu.FchVto >= x-FchVto-1)
        AND (x-FchVTo-2 = ? OR CcbCDocu.FchVto <= x-FchVto-2)
        AND LOOKUP(TRIM(CcbcDocu.CodDoc), 'BOL,FAC,N/C,N/D,CHQ,CHC,LET,TCK,BD,A/R,A/C') > 0 
        AND CcbcDocu.CodDoc BEGINS C-CMPBTE 
        AND LOOKUP(TRIM(CcbcDocu.FlgEst), F-ESTADO) > 0 
        AND CcbcDocu.NroDoc BEGINS c-Serie 
        AND CcbCDocu.CodCli BEGINS FILL-IN-CodCli 
        AND CcbcDocu.FmaPgo BEGINS F-CONDI AND CcbCdocu.CodVen BEGINS x-codven
        AND (x-CndCre = 'T' OR CcbCDocu.CndCre = x-CndCre) 
        AND (xcodmod = 3 OR ccbcdocu.codmon = xcodmod) NO-LOCK:
        CREATE t-ccbcdocu.
        BUFFER-COPY ccbcdocu TO t-ccbcdocu.
        PAUSE 0.
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal L-table-Win 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR k AS INT INIT 10 NO-UNDO.
SESSION:SET-WAIT-STATE('GENERAL').

EMPTY TEMP-TABLE COMPROBANTES. 
FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia
    AND (x-CodDiv = '' OR LOOKUP(gn-divi.coddiv, x-CODDIV) > 0):
    FOR EACH Ccbcdocu USE-INDEX Llave16 NO-LOCK WHERE {&Condicion}:
        FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
            "PROCESANDO " + STRING(ccbcdocu.fchdoc) + ' ' + STRING(Ccbcdocu.coddiv) + ' ' +
            Ccbcdocu.coddoc + ' ' + Ccbcdocu.nrodoc.
        CREATE COMPROBANTES.
        BUFFER-COPY Ccbcdocu TO COMPROBANTES.
    END.
END.
/* FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia,                               */
/*     EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.coddiv = gn-divi.coddiv AND {&Condicion}:      */
/*     k = k + 1.                                                                          */
/*     IF NOT (x-FchVto-1 = ? OR CcbCDocu.FchVto >= x-FchVto-1)                            */
/*         AND (x-FchVTo-2 = ? OR CcbCDocu.FchVto <= x-FchVto-2)                           */
/*         THEN NEXT.                                                                      */
/*     IF NOT (F-CONDI = '' OR CcbcDocu.FmaPgo = F-CONDI) THEN NEXT.                       */
/*     IF NOT (x-CndCre = 'T' OR CcbCDocu.CndCre = x-CndCre) THEN NEXT.                    */
/*     IF NOT (xcodmod = 3 OR ccbcdocu.codmon = xcodmod) THEN NEXT.                        */
/*     IF NOT (x-CodVen = '' OR CcbCdocu.CodVen = x-codven) THEN NEXT.                     */
/*     IF k MODULO 10 = 0  THEN FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} =      */
/*         "PROCESANDO " + STRING(ccbcdocu.fchdoc) + ' ' + STRING(Ccbcdocu.coddiv) + ' ' + */
/*         Ccbcdocu.coddoc + ' ' + Ccbcdocu.nrodoc.                                        */
/*     CREATE COMPROBANTES.                                                                */
/*     BUFFER-COPY Ccbcdocu TO COMPROBANTES.                                               */
/* END.                                                                                    */
SESSION:SET-WAIT-STATE('').
FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.

END PROCEDURE.

/*
&SCOPED-DEFINE CONDICION CcbcDocu.CodCia = S-CODCIA ~
        AND (x-CodDiv = '' OR LOOKUP(CcbCDocu.DivOri, x-CODDIV) > 0) ~
        AND (CcbcDocu.FchDoc >= f-desde AND CcbCdocu.FchDoc <= f-hasta ) ~
        AND LOOKUP(TRIM(CcbcDocu.CodDoc), 'BOL,FAC,N/C,N/D,CHQ,CHC,LET,TCK,BD,A/R,A/C') > 0 ~
        AND LOOKUP(CcbcDocu.FlgEst, F-ESTADO) > 0 ~
        AND (FILL-IN-CodCli = '' OR CcbCDocu.CodCli = FILL-IN-CodCli)
        */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel L-table-Win 
PROCEDURE Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pto AS LOGICAL NO-UNDO.

  EMPTY TEMP-TABLE t-ccbcdocu.
  GET FIRST {&browse-name}.
  REPEAT WHILE AVAILABLE(COMPROBANTES):
      CREATE t-ccbcdocu.
      BUFFER-COPY Ccbcdocu TO t-ccbcdocu.
      GET NEXT {&browse-name}.
  END.
  

/*   IF NOT lTodos THEN DO:                    */
/*       GET FIRST {&BROWSE-NAME}.             */
/*       REPEAT WHILE AVAILABLE ccbcdocu:      */
/*         CREATE t-ccbcdocu.                  */
/*         BUFFER-COPY ccbcdocu TO t-ccbcdocu. */
/*         GET NEXT {&BROWSE-NAME}.            */
/*       END.                                  */
/*   END.                                      */

  FIND FIRST t-ccbcdocu NO-LOCK NO-ERROR.
  IF NOT AVAILABLE t-ccbcdocu THEN RETURN ERROR.

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER init 1.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE t-Column                AS INTEGER INIT 2.

DEFINE VAR x-ImpTot AS DEC NO-UNDO.
DEFINE VAR x-SdoAct AS DEC NO-UNDO.

/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.

/* /* launch Excel so it is visible to the user */ */
/* chExcelApplication:Visible = TRUE.              */

/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).

/* set the column names for the Worksheet */
chWorkSheet:Columns("A"):ColumnWidth = 3.
chWorkSheet:Columns("B"):ColumnWidth = 9.
chWorkSheet:Columns("C"):ColumnWidth = 3.
chWorkSheet:Columns("D"):ColumnWidth = 9.
chWorkSheet:Columns("E"):ColumnWidth = 40.
chWorkSheet:Columns("F"):ColumnWidth = 10.
chWorkSheet:Columns("G"):ColumnWidth = 10.
chWorkSheet:Columns("H"):ColumnWidth = 10.
chWorkSheet:Columns("I"):ColumnWidth = 10.
chWorkSheet:Columns("J"):ColumnWidth = 20.
chWorkSheet:Columns("K"):ColumnWidth = 20.
chWorkSheet:Columns("L"):ColumnWidth = 20.
chWorkSheet:Columns("M"):ColumnWidth = 20.

chWorkSheet:Range("A2"):Value = "Doc".
chWorkSheet:Range("B2"):Value = "Numero".
chWorkSheet:Range("C2"):Value = "Ref.".
chWorkSheet:Range("D2"):Value = "Numero".
chWorkSheet:Range("E2"):Value = "Cliente".
chWorkSheet:Range("F2"):Value = "Cond. Vta.".
chWorkSheet:Range("G2"):Value = "Vendedor".
chWorkSheet:Range("H2"):Value = "Emision".
chWorkSheet:Range("I2"):Value = "Vencimiento".
chWorkSheet:Range("J2"):Value = "Recepci�n".
chWorkSheet:Range("K2"):Value = "Cancelaci�n".
CASE xcodmod:
    WHEN 1 THEN DO:
        chWorkSheet:Range("L2"):Value = "Importe S/.".
        chWorkSheet:Range("M2"):Value = "Importe S/. SIN Adelanto".
        chWorkSheet:Range("N2"):Value = "Saldo S/.".
        chWorkSheet:Range("O2"):Value = "Estado".
        chWorkSheet:Range("O2"):Value = "Usuario anulaci�n".
        chWorkSheet:Range("Q2"):Value = "Fecha".
        chWorkSheet:Range("R2"):Value = "Hora".
        chWorkSheet:Range("S2"):Value = "Motivo anulaci�n".
        chWorkSheet:Range("T2"):Value = "Divisi�n".
        chWorkSheet:Range("U2"):Value = "Fecha Reporte".
    END.
    WHEN 2 THEN DO:
        chWorkSheet:Range("L2"):Value = "Importe US$".
        chWorkSheet:Range("M2"):Value = "Importe US$ SIN Adelanto".
        chWorkSheet:Range("N2"):Value = "Saldo US$".
        chWorkSheet:Range("O2"):Value = "Estado".
        chWorkSheet:Range("P2"):Value = "Usuario anulaci�n".
        chWorkSheet:Range("Q2"):Value = "Fecha".
        chWorkSheet:Range("R2"):Value = "Hora".
        chWorkSheet:Range("S2"):Value = "Motivo anulaci�n".
        chWorkSheet:Range("T2"):Value = "Divisi�n".
        chWorkSheet:Range("U2"):Value = "Fecha Reporte".
    END.
    WHEN 3 THEN DO:
        chWorkSheet:Range("L2"):Value = "Importe S/.".
        chWorkSheet:Range("M2"):Value = "Importe S/. SIN Adelanto".
        chWorkSheet:Range("N2"):Value = "Saldo S/.".
        chWorkSheet:Range("O2"):Value = "Importe US$".
        chWorkSheet:Range("P2"):Value = "Importe US$ SIN Adelanto".
        chWorkSheet:Range("Q2"):Value = "Saldo US$".
        chWorkSheet:Range("R2"):Value = "Estado".
        chWorkSheet:Range("S2"):Value = "Usuario anulaci�n".
        chWorkSheet:Range("T2"):Value = "Fecha".
        chWorkSheet:Range("U2"):Value = "Hora".
        chWorkSheet:Range("V2"):Value = "Motivo anulaci�n".
        chWorkSheet:Range("W2"):Value = "Divisi�n".
        chWorkSheet:Range("X2"):Value = "Fecha Reporte".
    END.
END.

FOR EACH t-ccbcdocu:
    t-column = t-column + 1.
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.coddoc.
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + t-ccbcdocu.nrodoc.
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.codref.
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + t-ccbcdocu.nroref.
    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.nomcli.
    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + t-ccbcdocu.fmapgo.
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = "'" + t-ccbcdocu.codven.
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.fchdoc.
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.fchvto.  /*fchcbd*/
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.fchcbd.
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.fchcan.
    x-ImpTot = t-ccbcdocu.imptot + fAdelanto(t-ccbcdocu.codcia, t-ccbcdocu.coddoc, t-ccbcdocu.nrodoc).
    x-SdoAct = t-ccbcdocu.sdoact.
    FIND FacDocum WHERE Facdocum.codcia = t-ccbcdocu.codcia
        AND Facdocum.coddoc = t-ccbcdocu.coddoc
        NO-LOCK.
    IF Facdocum.tpodoc = NO     /* Documentos de Abono (N/C, A/R, etc) */
    THEN ASSIGN
            x-ImpTot = -1 * x-ImpTot
            x-SdoAct = -1 * x-SdoAct.
    FIND Ccbaudit OF t-ccbcdocu NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbaudit THEN FIND ccbtabla WHERE ccbtabla.codcia = t-ccbcdocu.codcia
        AND ccbtabla.tabla = 'MA'
        AND ccbtabla.codigo = ccbaudit.codref NO-LOCK NO-ERROR.
    CASE xcodmod:
        WHEN 1 OR WHEN 2 THEN DO:
            cRange = "L" + cColumn.
            chWorkSheet:Range(cRange):Value = t-ccbcdocu.imptot.
            cRange = "M" + cColumn.
            chWorkSheet:Range(cRange):Value = x-ImpTot.
            cRange = "N" + cColumn.
            chWorkSheet:Range(cRange):Value = x-SdoAct.
            cRange = "O" + cColumn.
            chWorkSheet:Range(cRange):Value = fEstado-1(t-ccbcdocu.flgest).
            IF AVAILABLE ccbtabla AND AVAILABLE ccbaudit THEN DO:
                cRange = "P" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbaudit.usuario.
                cRange = "Q" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbaudit.fecha.
                cRange = "R" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbaudit.hora.
                cRange = "S" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbtabla.nombre.
            END.
            cRange = "T" + cColumn.
            chWorkSheet:Range(cRange):Value = "'" + t-ccbcdocu.DivOri.
            cRange = "U" + cColumn.
            chWorkSheet:Range(cRange):Value = TODAY.
        END.
        WHEN 3 THEN DO:
            IF t-ccbcdocu.codmon = 1 THEN DO:
                cRange = "L" + cColumn.
                chWorkSheet:Range(cRange):Value = t-ccbcdocu.imptot.
                cRange = "M" + cColumn.
                chWorkSheet:Range(cRange):Value = x-ImpTot.
                cRange = "N" + cColumn.
                chWorkSheet:Range(cRange):Value = x-sdoact.
            END.
            IF t-ccbcdocu.codmon = 2 THEN DO:
                cRange = "O" + cColumn.
                chWorkSheet:Range(cRange):Value = t-ccbcdocu.imptot.
                cRange = "P" + cColumn.
                chWorkSheet:Range(cRange):Value = x-ImpTot.
                cRange = "Q" + cColumn.
                chWorkSheet:Range(cRange):Value = x-sdoact.
            END.
            cRange = "R" + cColumn.
            chWorkSheet:Range(cRange):Value = fEstado-1(t-ccbcdocu.flgest).
            IF AVAILABLE ccbtabla AND AVAILABLE ccbaudit THEN DO:
                cRange = "S" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbaudit.usuario.
                cRange = "T" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbaudit.fecha.
                cRange = "U" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbaudit.hora.
                cRange = "V" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbtabla.nombre.

            END.
            cRange = "W" + cColumn.
            chWorkSheet:Range(cRange):Value = "'" + t-ccbcdocu.DivOri.
            cRange = "X" + cColumn.
            chWorkSheet:Range(cRange):Value = TODAY.

        END.
    END CASE.
    
    DISPLAY Fi-Mensaje WITH FRAME F-Proceso.
    
END.
HIDE FRAME F-Proceso.

/*pto = SESSION:SET-WAIT-STATE("").*/

MESSAGE 'Proceso Terminado'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/* launch Excel so it is visible to the user */
chExcelApplication:Visible = TRUE.

/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.

RUN dispatch IN THIS-PROCEDURE ('open-query':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Formato L-table-Win 
PROCEDURE Formato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR x-Factor AS INT NO-UNDO.
   
DEFINE FRAME F-HdrConsulta
    HEADER
    {&PRN2} + {&PRN7A} + {&PRN6A} + S-NOMCIA + {&PRN7B} + {&PRN3} + {&PRN6B} FORMAT "X(45)" 
    "Fecha  : " AT 90 TODAY SKIP(1)
    {&PRN6A} + "CONSULTA DE COMPROBANTES" + {&PRN6B} + {&PRND} AT 45 FORMAT "X(45)" SKIP(1)
    "Desde         : " STRING(F-Desde,"99/99/9999") FORMAT "x(40)" SKIP
    "Hasta         : " STRING(F-Hasta,"99/99/9999") FORMAT "x(40)" SKIP
    "-------------------------------------------------------------------------------------------------------------------------------" SKIP
    "                                                     COND. COD. FECHA DE  FECHA DE                                             " SKIP
    "DOC.NUMERO   NOMBRE O RAZON SOCIAL                   VENTA VEN. EMISION   VENCMTO.   MONEDA      IMPORTE         SALDO  ESTADO " SKIP
    "-------------------------------------------------------------------------------------------------------------------------------" SKIP
/*   1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
     123 123456789 1234567890123456789012345678901234567890 123 123 99/99/9999 99/99/9999 12345 >>>,>>9.99 >>>,>>9.99
*/     
    WITH PAGE-TOP NO-LABELS NO-BOX STREAM-IO WIDTH 200. 
  
DEFINE FRAME F-FdrConsulta
    HEADER    
    SPACE(3) '!  TOTALES           !    SOLES     !   DOLARES  ! ' SKIP
    SPACE(3) FILL('-',50) FORMAT 'X(50)' SKIP
    SPACE(3) '!  IMPORTES        !' 
        X_tot_is FORMAT '->>>>,>>9.99' '!'
        X_TOT_ID FORMAT '->>>>,>>9.99' '!' SKIP
    SPACE(3) '!  SALDOS            !' 
        X_TOT_SS FORMAT '->>>>,>>9.99' '!'
        x_TOT_SD FORMAT '->>>>,>>9.99' '!'SKIP        
    SPACE(3) FILL('-',50) FORMAT 'X(50)' SKIP
    WITH PAGE-BOTTOM NO-LABELS NO-BOX STREAM-IO WIDTH 200. 

/* CASE s-salida-impresion:
 *        WHEN 1 THEN OUTPUT TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Pantalla */
 *        WHEN 2 THEN OUTPUT TO VALUE(s-port-name)  PAGED PAGE-SIZE 62. /* Impresora */
 *        WHEN 3 THEN OUTPUT TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Archivo */
 *  END CASE.
 *  PUT CONTROL {&PRN0} {&PRN5A} CHR(66) {&PRN3}.*/

/*    X_TOT_IS = 0.
 *     X_TOT_SS = 0.
 *     X_TOT_ID = 0.
 *     X_TOT_SD = 0.*/

/* NUEVA RUTINA */
  DEFINE FRAME F-DetaCon
    t-Ccbcdocu.Coddoc 
    t-Ccbcdocu.nrodoc 
    t-Ccbcdocu.nomcli FORMAT "X(40)" 
    t-Ccbcdocu.fmapgo FORMAT "X(3)"
    t-Ccbcdocu.CodVen FORMAT "X(3)"
    t-Ccbcdocu.FchDoc FORMAT '99/99/9999'
    t-Ccbcdocu.FchVto FORMAT '99/99/9999'
    X-MON           FORMAT "X(5)"
    t-Ccbcdocu.Imptot
    t-Ccbcdocu.Sdoact
    X-EST
    WITH NO-LABELS NO-BOX NO-UNDERLINE WIDTH 200 STREAM-IO DOWN. 
  FOR EACH t-ccbcdocu:
    DELETE T-ccbcdocu.
  END.
       
  GET FIRST {&BROWSE-NAME}.
  
  REPEAT WHILE AVAILABLE ccbcdocu:
    CREATE t-ccbcdocu.
    BUFFER-COPY ccbcdocu TO t-ccbcdocu.
/*    IF t-ccbcdocu.coddoc = 'N/C' 
 *     THEN ASSIGN
 *             t-ccbcdocu.imptot = t-ccbcdocu.imptot * -1
 *             t-ccbcdocu.sdoact = t-ccbcdocu.sdoact * -1.*/
    GET NEXT {&BROWSE-NAME}.
  END.
  
   CASE s-salida-impresion:
       WHEN 1 THEN OUTPUT TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Pantalla */
/*       WHEN 2 THEN OUTPUT TO VALUE(s-port-name)  PAGED PAGE-SIZE 62. /* Impresora */*/
       WHEN 2 THEN OUTPUT TO PRINTER             PAGED PAGE-SIZE 62. /* Impresora */
       WHEN 3 THEN OUTPUT TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Archivo */
 END CASE.
 PUT CONTROL {&PRN0} {&PRN5A} CHR(66) {&PRN3}.
  
    X_TOT_IS = 0.
    X_TOT_SS = 0.
    X_TOT_ID = 0.
    X_TOT_SD = 0.
  
  FOR EACH t-ccbcdocu BREAK BY t-ccbcdocu.codcia BY t-ccbcdocu.fchdoc:
    
    x-Factor = 1.
    FIND FacDocum OF t-CcbCDocu NO-LOCK NO-ERROR.
    IF AVAILABLE FacDocum
    THEN x-Factor = IF FacDocum.TpoDoc = NO THEN -1 ELSE 1.

    IF t-ccbcdocu.flgest <> 'A'     /* NO ANULADOS */
    THEN DO:
        IF t-ccbcdocu.CodMon = 1 THEN
            ASSIGN
                X_TOT_IS = X_TOT_IS + t-ccbcdocu.ImpTot * x-Factor
                X_TOT_SS = X_TOT_SS + t-ccbcdocu.Sdoact * x-Factor.
        ELSE
            ASSIGN
                X_TOT_ID = X_TOT_ID + t-ccbcdocu.ImpTot * x-Factor
                X_TOT_SD = X_TOT_SD + t-ccbcdocu.Sdoact * x-Factor.
    END.
             
          
    IF t-CcbcDocu.CodMon = 1 THEN
        ASSIGN
            X-MON = "S/.".
    ELSE
        ASSIGN
            X-MON = "US$".
                    
    IF t-CcbCDocu.FlgEst = "P" THEN
        ASSIGN
            X-EST = "PEN" .
    ELSE
       IF t-CcbCDocu.FlgEst = "C" THEN
          ASSIGN
              X-EST = "CAN" .    
       ELSE       
          IF t-CcbcDocu.FlgEst = "A" THEN
             ASSIGN
                 X-EST = "ANU" .            

    VIEW FRAME F-HdrConsulta.
   DISPLAY 
    t-Ccbcdocu.Coddoc 
    t-Ccbcdocu.nrodoc 
    t-Ccbcdocu.nomcli 
    t-Ccbcdocu.fmapgo
    t-Ccbcdocu.CodVen
    t-Ccbcdocu.FchDoc
    t-Ccbcdocu.FchVto
    X-MON
    t-Ccbcdocu.Imptot
    t-Ccbcdocu.Sdoact
    X-EST
    WITH FRAME F-Detacon.
    IF LAST-OF(t-Ccbcdocu.codcia) THEN DO:
        UNDERLINE    
        t-Ccbcdocu.ImpTot
        t-CcbCdocu.SdoAct
        WITH FRAME F-Detacon.
        DISPLAY 
            "   Importe Soles "     @ t-Ccbcdocu.FchVto
            x_tot_is @ t-Ccbcdocu.Imptot
            WITH FRAME F-Detacon.
        DOWN WITH FRAME F-Detacon.
        DISPLAY 
            "   Importe Dolares "     @ t-Ccbcdocu.FchVto
            x_tot_id @ t-Ccbcdocu.Imptot
            WITH FRAME F-Detacon.
        DOWN WITH FRAME F-Detacon.
        DISPLAY 
            "   Saldo Soles "     @ t-Ccbcdocu.FchVto
            x_tot_ss @ t-Ccbcdocu.Imptot
            WITH FRAME F-Detacon.
        DOWN WITH FRAME F-Detacon.
        DISPLAY 
            "   Saldo Dolares "     @ t-Ccbcdocu.FchVto
            x_tot_sd @ t-Ccbcdocu.Imptot
            WITH FRAME F-Detacon.
        DOWN WITH FRAME F-Detacon.
    END.
  END.
  

OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Formato1 L-table-Win 
PROCEDURE Formato1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR x-Factor AS INT NO-UNDO.
   
DEFINE FRAME F-HdrConsulta
    HEADER
    {&PRN2} + {&PRN7A} + {&PRN6A} + S-NOMCIA + {&PRN7B} + {&PRN3} + {&PRN6B} FORMAT "X(45)" 
    "Fecha  : " AT 90 TODAY SKIP(1)
    {&PRN6A} + "CONSULTA DE COMPROBANTES" + {&PRN6B} + {&PRND} AT 45 FORMAT "X(45)" SKIP(1)
    "Desde         : " STRING(F-Desde,"99/99/9999") FORMAT "x(40)" SKIP
    "Hasta         : " STRING(F-Hasta,"99/99/9999") FORMAT "x(40)" SKIP
    "-------------------------------------------------------------------------------------------------------------------------------" SKIP
    "                                                     COND. COD. FECHA DE  FECHA DE                                             " SKIP
    "DOC.NUMERO   NOMBRE O RAZON SOCIAL                   VENTA VEN. EMISION   VENCMTO.   MONEDA      IMPORTE         SALDO  ESTADO " SKIP
    "-------------------------------------------------------------------------------------------------------------------------------" SKIP
/*   1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
     123 123456789 1234567890123456789012345678901234567890 123 123 99/99/9999 99/99/9999 12345 >>>,>>9.99 >>>,>>9.99
*/     
    WITH PAGE-TOP NO-LABELS NO-BOX STREAM-IO WIDTH 200. 
  
DEFINE FRAME F-FdrConsulta
    HEADER    
    SPACE(3) '!  TOTALES           !    SOLES     !   DOLARES  ! ' SKIP
    SPACE(3) FILL('-',50) FORMAT 'X(50)' SKIP
    SPACE(3) '!  IMPORTES        !' 
        X_tot_is FORMAT '->>>>,>>9.99' '!'
        X_TOT_ID FORMAT '->>>>,>>9.99' '!' SKIP
    SPACE(3) '!  SALDOS            !' 
        X_TOT_SS FORMAT '->>>>,>>9.99' '!'
        x_TOT_SD FORMAT '->>>>,>>9.99' '!'SKIP        
    SPACE(3) FILL('-',50) FORMAT 'X(50)' SKIP
    WITH PAGE-BOTTOM NO-LABELS NO-BOX STREAM-IO WIDTH 200. 

 CASE s-salida-impresion:
       WHEN 1 THEN OUTPUT TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Pantalla */
/*       WHEN 2 THEN OUTPUT TO VALUE(s-port-name)  PAGED PAGE-SIZE 62. /* Impresora */*/
       WHEN 2 THEN OUTPUT TO PRINTER             PAGED PAGE-SIZE 62. /* Impresora */
       WHEN 3 THEN OUTPUT TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Archivo */
 END CASE.
 PUT CONTROL {&PRN0} {&PRN5A} CHR(66) {&PRN3}.

    X_TOT_IS = 0.
    X_TOT_SS = 0.
    X_TOT_ID = 0.
    X_TOT_SD = 0.

/* NUEVA RUTINA */
  DEFINE FRAME F-DetaCon
    t-Ccbcdocu.Coddoc 
    t-Ccbcdocu.nrodoc 
    t-Ccbcdocu.nomcli FORMAT "X(40)" 
    t-Ccbcdocu.fmapgo FORMAT "X(3)"
    t-Ccbcdocu.CodVen FORMAT "X(3)"
    t-Ccbcdocu.FchDoc FORMAT '99/99/9999'
    t-Ccbcdocu.FchVto FORMAT '99/99/9999'
    X-MON           FORMAT "X(5)"
    t-Ccbcdocu.Imptot
    t-Ccbcdocu.Sdoact
    X-EST
    WITH NO-LABELS NO-BOX NO-UNDERLINE WIDTH 200 STREAM-IO DOWN. 
   
  
  FOR EACH t-ccbcdocu:
    DELETE T-ccbcdocu.
  END.
       
  GET FIRST {&BROWSE-NAME}.
  
  REPEAT WHILE AVAILABLE ccbcdocu:
    CREATE t-ccbcdocu.
    BUFFER-COPY ccbcdocu TO t-ccbcdocu.
/*    IF t-ccbcdocu.coddoc = 'N/C' 
 *     THEN ASSIGN
 *             t-ccbcdocu.imptot = t-ccbcdocu.imptot * -1
 *             t-ccbcdocu.sdoact = t-ccbcdocu.sdoact * -1.*/
    GET NEXT {&BROWSE-NAME}.
  END.
  FOR EACH t-ccbcdocu where t-ccbcdocu.codmon = 1 BREAK BY t-ccbcdocu.codcia BY t-ccbcdocu.fchdoc:
    
    x-Factor = 1.
    FIND FacDocum OF t-CcbCDocu NO-LOCK NO-ERROR.
    IF AVAILABLE FacDocum
    THEN x-Factor = IF FacDocum.TpoDoc = NO THEN -1 ELSE 1.

    IF t-ccbcdocu.flgest <> 'A'     /* NO ANULADOS */
    THEN DO:
        IF t-ccbcdocu.CodMon = 1 THEN
            ASSIGN
                X_TOT_IS = X_TOT_IS + t-ccbcdocu.ImpTot * x-Factor
                X_TOT_SS = X_TOT_SS + t-ccbcdocu.Sdoact * x-Factor.
        /*ELSE
            ASSIGN
                X_TOT_ID = X_TOT_ID + t-ccbcdocu.ImpTot * x-Factor
                X_TOT_SD = X_TOT_SD + t-ccbcdocu.Sdoact * x-Factor.*/
    END.
         
     
    IF t-CcbcDocu.CodMon = 1 THEN
        ASSIGN
            X-MON = "S/.".
    /*ELSE
        ASSIGN
            X-MON = "US$".*/
                    
    IF t-CcbCDocu.FlgEst = "P" THEN
        ASSIGN
            X-EST = "PEN" .
    ELSE
       IF t-CcbCDocu.FlgEst = "C" THEN
          ASSIGN
              X-EST = "CAN" .    
       ELSE       
          IF t-CcbcDocu.FlgEst = "A" THEN
             ASSIGN
                 X-EST = "ANU" .            

    VIEW FRAME F-HdrConsulta.
   DISPLAY 
    t-Ccbcdocu.Coddoc 
    t-Ccbcdocu.nrodoc 
    t-Ccbcdocu.nomcli 
    t-Ccbcdocu.fmapgo
    t-Ccbcdocu.CodVen
    t-Ccbcdocu.FchDoc
    t-Ccbcdocu.FchVto
    X-MON
    t-Ccbcdocu.Imptot
    t-Ccbcdocu.Sdoact
    X-EST
    WITH FRAME F-Detacon.
    
    IF LAST-OF(t-Ccbcdocu.codcia) THEN DO:
        UNDERLINE    
        t-Ccbcdocu.ImpTot
        t-CcbCdocu.SdoAct
        WITH FRAME F-Detacon.
        DISPLAY 
            "   Importe Soles "     @ t-Ccbcdocu.FchVto
            x_tot_is @ t-Ccbcdocu.Imptot
            WITH FRAME F-Detacon.
        DOWN WITH FRAME F-Detacon.
        DISPLAY 
            "   Importe Dolares "     @ t-Ccbcdocu.FchVto
            x_tot_id @ t-Ccbcdocu.Imptot
            WITH FRAME F-Detacon.
        DOWN WITH FRAME F-Detacon.
        DISPLAY 
            "   Saldo Soles "     @ t-Ccbcdocu.FchVto
            x_tot_ss @ t-Ccbcdocu.Imptot
            WITH FRAME F-Detacon.
        DOWN WITH FRAME F-Detacon.
        DISPLAY 
            "   Saldo Dolares "     @ t-Ccbcdocu.FchVto
            x_tot_sd @ t-Ccbcdocu.Imptot
            WITH FRAME F-Detacon.
        DOWN WITH FRAME F-Detacon.
    END.
  END.
  
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Formato2 L-table-Win 
PROCEDURE Formato2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR x-Factor AS INT NO-UNDO.
   
DEFINE FRAME F-HdrConsulta
    HEADER
    {&PRN2} + {&PRN7A} + {&PRN6A} + S-NOMCIA + {&PRN7B} + {&PRN3} + {&PRN6B} FORMAT "X(45)" 
    "Fecha  : " AT 90 TODAY SKIP(1)
    {&PRN6A} + "CONSULTA DE COMPROBANTES" + {&PRN6B} + {&PRND} AT 45 FORMAT "X(45)" SKIP(1)
    "Desde         : " STRING(F-Desde,"99/99/9999") FORMAT "x(40)" SKIP
    "Hasta         : " STRING(F-Hasta,"99/99/9999") FORMAT "x(40)" SKIP
    "-------------------------------------------------------------------------------------------------------------------------------" SKIP
    "                                                     COND. COD. FECHA DE  FECHA DE                                             " SKIP
    "DOC.NUMERO   NOMBRE O RAZON SOCIAL                   VENTA VEN. EMISION   VENCMTO.   MONEDA      IMPORTE         SALDO  ESTADO " SKIP
    "-------------------------------------------------------------------------------------------------------------------------------" SKIP
/*   1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
     123 123456789 1234567890123456789012345678901234567890 123 123 99/99/9999 99/99/9999 12345 >>>,>>9.99 >>>,>>9.99
*/     
    WITH PAGE-TOP NO-LABELS NO-BOX STREAM-IO WIDTH 200. 
  
DEFINE FRAME F-FdrConsulta
    HEADER    
    SPACE(3) '!  TOTALES           !    SOLES     !   DOLARES  ! ' SKIP
    SPACE(3) FILL('-',50) FORMAT 'X(50)' SKIP
    SPACE(3) '!  IMPORTES        !' 
        X_tot_is FORMAT '->>>>,>>9.99' '!'
        X_TOT_ID FORMAT '->>>>,>>9.99' '!' SKIP
    SPACE(3) '!  SALDOS            !' 
        X_TOT_SS FORMAT '->>>>,>>9.99' '!'
        x_TOT_SD FORMAT '->>>>,>>9.99' '!'SKIP        
    SPACE(3) FILL('-',50) FORMAT 'X(50)' SKIP
    WITH PAGE-BOTTOM NO-LABELS NO-BOX STREAM-IO WIDTH 200. 

 CASE s-salida-impresion:
       WHEN 1 THEN OUTPUT TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Pantalla */
/*       WHEN 2 THEN OUTPUT TO PRINTER VALUE(s-port-name)  PAGED PAGE-SIZE 62. /* Impresora */*/
       WHEN 2 THEN OUTPUT TO PRINTER             PAGED PAGE-SIZE 62. /* Impresora */
       WHEN 3 THEN OUTPUT TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Archivo */
 END CASE.
 PUT CONTROL {&PRN0} {&PRN5A} CHR(66) {&PRN3}.

    X_TOT_IS = 0.
    X_TOT_SS = 0.
    X_TOT_ID = 0.
    X_TOT_SD = 0.

/* NUEVA RUTINA */
  DEFINE FRAME F-DetaCon
    t-Ccbcdocu.Coddoc 
    t-Ccbcdocu.nrodoc 
    t-Ccbcdocu.nomcli FORMAT "X(40)" 
    t-Ccbcdocu.fmapgo FORMAT "X(3)"
    t-Ccbcdocu.CodVen FORMAT "X(3)"
    t-Ccbcdocu.FchDoc FORMAT '99/99/9999'
    t-Ccbcdocu.FchVto FORMAT '99/99/9999'
    X-MON           FORMAT "X(5)"
    t-Ccbcdocu.Imptot
    t-Ccbcdocu.Sdoact
    X-EST
    WITH NO-LABELS NO-BOX NO-UNDERLINE WIDTH 200 STREAM-IO DOWN. 
   
  
  FOR EACH t-ccbcdocu:
    DELETE T-ccbcdocu.
  END.
       
  GET FIRST {&BROWSE-NAME}.
  
  REPEAT WHILE AVAILABLE ccbcdocu:
    CREATE t-ccbcdocu.
    BUFFER-COPY ccbcdocu TO t-ccbcdocu.
/*    IF t-ccbcdocu.coddoc = 'N/C' 
 *     THEN ASSIGN
 *             t-ccbcdocu.imptot = t-ccbcdocu.imptot * -1
 *             t-ccbcdocu.sdoact = t-ccbcdocu.sdoact * -1.*/
    GET NEXT {&BROWSE-NAME}.
  END.
  FOR EACH t-ccbcdocu where t-ccbcdocu.codmon = 2 BREAK BY t-ccbcdocu.codcia BY t-ccbcdocu.fchdoc:
    
    x-Factor = 1.
    FIND FacDocum OF t-CcbCDocu NO-LOCK NO-ERROR.
    IF AVAILABLE FacDocum
    THEN x-Factor = IF FacDocum.TpoDoc = NO THEN -1 ELSE 1.

    IF t-ccbcdocu.flgest <> 'A'     /* NO ANULADOS */
    THEN DO:
        IF t-ccbcdocu.CodMon = 2 THEN
            /*ASSIGN
                X_TOT_IS = X_TOT_IS + t-ccbcdocu.ImpTot * x-Factor
                X_TOT_SS = X_TOT_SS + t-ccbcdocu.Sdoact * x-Factor.
        ELSE*/
            ASSIGN
                X_TOT_ID = X_TOT_ID + t-ccbcdocu.ImpTot * x-Factor
                X_TOT_SD = X_TOT_SD + t-ccbcdocu.Sdoact * x-Factor.
    END.
         
     
    IF t-CcbcDocu.CodMon = 2 THEN
        /*ASSIGN
            X-MON = "S/.".
    ELSE*/
        ASSIGN
            X-MON = "US$".
                    
    IF t-CcbCDocu.FlgEst = "P" THEN
        ASSIGN
            X-EST = "PEN" .
    ELSE
       IF t-CcbCDocu.FlgEst = "C" THEN
          ASSIGN
              X-EST = "CAN" .    
       ELSE       
          IF t-CcbcDocu.FlgEst = "A" THEN
             ASSIGN
                 X-EST = "ANU" .            

    VIEW FRAME F-HdrConsulta.
   DISPLAY 
    t-Ccbcdocu.Coddoc 
    t-Ccbcdocu.nrodoc 
    t-Ccbcdocu.nomcli 
    t-Ccbcdocu.fmapgo
    t-Ccbcdocu.CodVen
    t-Ccbcdocu.FchDoc
    t-Ccbcdocu.FchVto
    X-MON
    t-Ccbcdocu.Imptot
    t-Ccbcdocu.Sdoact
    X-EST
    WITH FRAME F-Detacon.
    
    IF LAST-OF(t-Ccbcdocu.codcia) THEN DO:
        UNDERLINE    
        t-Ccbcdocu.ImpTot
        t-CcbCdocu.SdoAct
        WITH FRAME F-Detacon.
        DISPLAY 
            "   Importe Soles "     @ t-Ccbcdocu.FchVto
            x_tot_is @ t-Ccbcdocu.Imptot
            WITH FRAME F-Detacon.
        DOWN WITH FRAME F-Detacon.
        DISPLAY 
            "   Importe Dolares "     @ t-Ccbcdocu.FchVto
            x_tot_id @ t-Ccbcdocu.Imptot
            WITH FRAME F-Detacon.
        DOWN WITH FRAME F-Detacon.
        DISPLAY 
            "   Saldo Soles "     @ t-Ccbcdocu.FchVto
            x_tot_ss @ t-Ccbcdocu.Imptot
            WITH FRAME F-Detacon.
        DOWN WITH FRAME F-Detacon.
        DISPLAY 
            "   Saldo Dolares "     @ t-Ccbcdocu.FchVto
            x_tot_sd @ t-Ccbcdocu.Imptot
            WITH FRAME F-Detacon.
        DOWN WITH FRAME F-Detacon.
    END.
  END.
  
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir L-table-Win 
PROCEDURE Imprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF xcodmod = 1 THEN RUN Formato1.
IF xcodmod = 2 THEN RUN Formato2.
IF xcodmod = 3 THEN RUN Formato.
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
            CMB-filtro:LIST-ITEMS IN FRAME {&FRAME-NAME} =
            CMB-filtro:LIST-ITEMS + "," + RETURN-VALUE.
    ASSIGN
        f-Desde = TODAY
        f-Hasta = TODAY.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query-cases L-table-Win 
PROCEDURE local-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query-cases':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
    
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
  {src/adm/template/snd-list.i "COMPROBANTES"}
  {src/adm/template/snd-list.i "CcbCDocu"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAdelanto L-table-Win 
FUNCTION fAdelanto RETURNS DECIMAL
  ( INPUT pCodCia AS INTEGER, INPUT pCodDoc AS CHAR, INPUT pNroDoc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR fImpLin AS DEC NO-UNDO.

  FOR EACH ccbddocu NO-LOCK WHERE ccbddocu.codcia = pcodcia
      AND ccbddocu.coddoc = pcoddoc
      AND ccbddocu.nrodoc = pnrodoc
      AND implin < 0:
      fImpLin = fImpLin + ccbddocu.implin.
  END.
  RETURN ABSOLUTE(fImpLin).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fBanco L-table-Win 
FUNCTION fBanco RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR pValor AS CHAR NO-UNDO.

  IF Ccbcdocu.coddoc <> "LET" THEN RETURN "".   /* Function return value. */

  RUN vta2/p-estado-letras ("Banco", Ccbcdocu.codcta, OUTPUT pValor).
  RETURN pValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEstado L-table-Win 
FUNCTION fEstado RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    IF CcbCDocu.FlgEst = "P" THEN
        ASSIGN
            X-EST = "PEN" .
    ELSE
       IF CcbCDocu.FlgEst = "C" THEN
          ASSIGN
              X-EST = "CAN" .    
       ELSE       
          IF CcbcDocu.FlgEst = "A" THEN
             ASSIGN
                 X-EST = "ANU" .            
  CASE Ccbcdocu.flgest:
    WHEN 'P' THEN RETURN 'PEN'.
    WHEN 'C' THEN RETURN 'CAN'.
    WHEN 'A' THEN RETURN 'ANU'.
    OTHERWISE RETURN '???'.
  END CASE.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEstado-1 L-table-Win 
FUNCTION fEstado-1 RETURNS CHARACTER
  ( INPUT pFlgEst AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  CASE pflgest:
    WHEN 'P' THEN RETURN 'PEN'.
    WHEN 'C' THEN RETURN 'CAN'.
    WHEN 'A' THEN RETURN 'ANU'.
    OTHERWISE RETURN '???'.
  END CASE.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFecha L-table-Win 
FUNCTION fFecha RETURNS DATE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST Ccbaudit OF Ccbcdocu WHERE Ccbaudit.evento = 'DELETE' NO-LOCK NO-ERROR.
  IF AVAILABLE Ccbaudit  THEN RETURN Ccbaudit.fecha.
  RETURN ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fHora L-table-Win 
FUNCTION fHora RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Ccbaudit OF Ccbcdocu WHERE Ccbaudit.evento = 'DELETE' NO-LOCK NO-ERROR.
  IF AVAILABLE Ccbaudit  THEN RETURN Ccbaudit.hora.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fNombre L-table-Win 
FUNCTION fNombre RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Ccbaudit OF Ccbcdocu WHERE Ccbaudit.evento = 'DELETE' NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Ccbaudit  THEN RETURN "".
  FIND Ccbtabla WHERE Ccbtabla.codcia = Ccbcdocu.codcia
      AND Ccbtabla.tabla = 'MA'
      AND Ccbtabla.codigo = Ccbaudit.codref
      NO-LOCK NO-ERROR.
  IF AVAILABLE Ccbtabla THEN RETURN ccbtabla.nombre.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSituacion L-table-Win 
FUNCTION fSituacion RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR pValor AS CHAR NO-UNDO.

  IF Ccbcdocu.coddoc <> "LET" THEN RETURN "".   /* Function return value. */

  RUN vta2/p-estado-letras ("Situacion", Ccbcdocu.flgsit, OUTPUT pValor).
  RETURN pValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fUbicacion L-table-Win 
FUNCTION fUbicacion RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR pValor AS CHAR NO-UNDO.

  IF Ccbcdocu.coddoc <> "LET" THEN RETURN "".   /* Function return value. */

  RUN vta2/p-estado-letras ("Ubicacion", Ccbcdocu.flgubi, OUTPUT pValor).
  RETURN pValor.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fUsuario L-table-Win 
FUNCTION fUsuario RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Ccbaudit OF Ccbcdocu WHERE Ccbaudit.evento = 'DELETE' NO-LOCK NO-ERROR.
  IF AVAILABLE Ccbaudit  THEN RETURN Ccbaudit.usuario.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

