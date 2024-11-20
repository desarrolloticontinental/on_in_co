&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE COMPROBANTES NO-UNDO LIKE CcbCDocu
       FIELD NomCta AS CHAR FORMAT 'x(60)'
       .
DEFINE TEMP-TABLE t-gn-clie NO-UNDO LIKE gn-clie.



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
DEFINE SHARED VARIABLE CL-CODCIA AS INTEGER.
DEFINE SHARED VARIABLE S-NOMCIA AS CHAR.
DEFINE SHARED VARIABLE S-CODDIV AS CHAR.
DEFINE SHARED VARIABLE pCodDiv AS CHAR.

DEFINE VAR ss-CodDiv LIKE s-CodDiv.     /* ARTIFICIO */
DEFINE VAR F-ESTADO AS CHAR INIT 'P,C,A,J'.         /* Se add J, a pedido de Julissa correo del 08Mar2018 - AYUDA 3: DOCUMENTOS COBRANZA DUDOSA */
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
/*x-Comprobantes = 'BOL,FAC,N/C,N/D,CHQ,CHC,LET,TCK,BD,A/R,A/C,DCO'.*/
FOR EACH FacDocum NO-LOCK WHERE FacDocum.CodCia = s-codcia AND FacDocum.TpoDoc <> ?:
    x-Comprobantes = x-Comprobantes + (IF x-Comprobantes = '' THEN '' ELSE ',') + FacDocum.CodDoc.
END.

&SCOPED-DEFINE CONDICION CcbcDocu.CodCia = S-CODCIA ~
        AND Ccbcdocu.divori = gn-divi.coddiv ~
        AND LOOKUP(TRIM(CcbcDocu.CodDoc), x-Comprobantes) > 0 ~
        AND (c-Serie = '' OR CcbcDocu.NroDoc = c-Serie) ~
        AND (CcbcDocu.FchDoc >= f-desde AND CcbCdocu.FchDoc <= f-hasta ) ~
        AND (x-FchVto-1 = ? OR CcbCDocu.FchVto >= x-FchVto-1) ~
        AND (x-FchVTo-2 = ? OR CcbCDocu.FchVto <= x-FchVto-2) ~
        AND LOOKUP(CcbcDocu.FlgEst, F-ESTADO) > 0 ~
        AND (FILL-IN-CodCli = '' OR CcbCDocu.CodCli = FILL-IN-CodCli) ~
        AND (F-CONDI = '' OR CcbcDocu.FmaPgo = F-CONDI) ~
        AND (x-CodVen = '' OR CcbCdocu.CodVen = x-codven) ~
        AND (xcodmod = 3 OR ccbcdocu.codmon = xcodmod)

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
DEF TEMP-TABLE t-ccbcdocu NO-UNDO LIKE ccbcdocu.

DEFINE TEMP-TABLE ttCanjes NO-UNDO
    FIELD   tcoddoc     AS  CHAR    FORMAT 'x(5)'
    FIELD   tnrodoc     AS  CHAR    FORMAT 'x(15)'
    FIELD   timptot     AS  DEC     format  '->>,>>>,>>9.99' INIT 0
    FIELD   timppapel     AS  DEC     format  '->>,>>>,>>9.99' INIT 0
    .

DEFINE BUFFER x-ccbcdocu FOR ccbcdocu.

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

DEFINE VAR x-Archivo AS CHAR.

DEFINE TEMP-TABLE tt-reg-tempo NO-UNDO
    FIELD A AS CHAR FORMAT 'x(80)'
    FIELD B AS CHAR FORMAT 'x(80)'
    FIELD C AS CHAR FORMAT 'x(80)'
    FIELD D AS CHAR FORMAT 'x(80)'
    FIELD E AS CHAR FORMAT 'x(80)'
    FIELD F AS CHAR FORMAT 'x(80)'
    FIELD G AS CHAR FORMAT 'x(100)'
    FIELD H AS CHAR FORMAT 'x(80)'
    FIELD I AS CHAR FORMAT 'x(80)'
    FIELD J AS CHAR FORMAT 'x(80)'
    FIELD K AS CHAR FORMAT 'x(80)'
    FIELD L AS CHAR FORMAT 'x(15)'
    FIELD M AS CHAR FORMAT 'x(15)'
    FIELD N AS CHAR FORMAT 'x(15)'
    FIELD O AS CHAR FORMAT 'x(15)'
    FIELD P LIKE t-ccbcdocu.imptot
    FIELD Q LIKE t-ccbcdocu.imptot
    FIELD R LIKE t-ccbcdocu.imptot
    FIELD S AS CHAR FORMAT 'x(50)'
    FIELD T AS CHAR FORMAT 'x(30)'  /*LIKE ccbaudit.usuario*/
    FIELD U AS CHAR FORMAT 'x(30)'  /*LIKE ccbaudit.fecha*/
    FIELD V AS CHAR FORMAT 'x(80)'
    FIELD W AS CHAR FORMAT 'x(80)'
    FIELD X AS CHAR FORMAT 'x(80)'
    FIELD Y AS CHAR FORMAT 'x(30)'  /*AS DATE /*x-today*/*/
    FIELD Z AS CHAR FORMAT 'x(60)' /*fubicacion*/ 
    FIELD AA AS CHAR FORMAT 'x(50)' /*fBanco*/
    FIELD AB AS CHAR FORMAT 'x(25)' /*fSituacion*/
    FIELD AC AS CHAR FORMAT 'x(25)' /*AS DATE    /*lFchEntrega*/*/
    FIELD AD AS CHAR FORMAT 'x(25)' /*LIKE t-ccbcdocu.fchcbd*/
    FIELD AE AS CHAR FORMAT 'x(100)'
    FIELD AF AS CHAR FORMAT 'x(25)' /*AS DATE /*lFchEntrega.*/*/
    FIELD AG AS CHAR FORMAT 'x(25)' /*LIKE t-ccbcdocu.fchcbd*/
    FIELD AH AS CHAR FORMAT 'x(100)'
    FIELD AI AS CHAR FORMAT 'x(60)'     /*pNomDistr.*/
    FIELD AJ AS CHAR FORMAT 'x(80)'
    FIELD AK AS CHAR FORMAT 'x(80)' /*x-CodCta*/
    FIELD AL AS CHAR FORMAT 'x(80)' /*x-NomCta*/
    FIELD AN LIKE t-ccbcdocu.imptot /*lde_ImpFoto.*/
    FIELD AM AS CHAR FORMAT 'x(80)'
    FIELD AO LIKE t-ccbcdocu.imptot /*lde_ImpFoto.*/
    FIELD AP AS CHAR FORMAT 'x(80)' /*x-codCta.*/
    FIELD AQ AS CHAR FORMAT 'x(60)' /*x-NomCta.*/
.

/* Output constants */
&scoped-define lf  chr(10)
&scoped-define cr  chr(13)
&scoped-define qt  chr(34)


DEFINE IMAGE IMAGE-1 FILENAME "IMG\Coti.ico" SIZE 12 BY 1.5.
DEF VAR FI-MENSAJE AS CHAR FORMAT "X(40)" .

DEFINE FRAME F-Proceso
     IMAGE-1 AT ROW 1.5 COL 5
     "Espere un momento" VIEW-AS TEXT
          SIZE 18 BY 1 AT ROW 1.5 COL 16 FONT 6
     "por favor ...." VIEW-AS TEXT
          SIZE 10 BY 1 AT ROW 2.5 COL 19 FONT 6
          SKIP
     Fi-Mensaje NO-LABEL FONT 6
     SKIP     
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
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
&Scoped-define FIELDS-IN-QUERY-br_table fEstado() @ X-EST COMPROBANTES.DivOri COMPROBANTES.CodDoc COMPROBANTES.NroDoc COMPROBANTES.CodRef COMPROBANTES.NroRef COMPROBANTES.Libre_c04 COMPROBANTES.NroOrd COMPROBANTES.CodCli COMPROBANTES.NomCli COMPROBANTES.RucCli COMPROBANTES.FmaPgo COMPROBANTES.CodVen COMPROBANTES.FchDoc COMPROBANTES.FchAte COMPROBANTES.FchCbd COMPROBANTES.FchVto COMPROBANTES.FchCan X-MON @ X-MON COMPROBANTES.ImpTot COMPROBANTES.SdoAct COMPROBANTES.ImpDto fUsuario() @ x-Usuario fFecha() @ x-Fecha fHora() @ x-Hora fNombre() @ x-Nombre fUbicacion() @ x-Ubicacion fBanco() @ x-Banco fSituacion() @ x-Situacion CcbCDocu.NroSal CcbCDocu.CodRef CcbCDocu.NroRef COMPROBANTES.ImpTot + fAdelanto(COMPROBANTES.CodCia,COMPROBANTES.CodDoc,COMPROBANTES.NroDoc) @ x-ImpTot COMPROBANTES.CodCta COMPROBANTES.NomCta   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH COMPROBANTES WHERE ~{&KEY-PHRASE} NO-LOCK, ~
             FIRST CcbCDocu OF COMPROBANTES NO-LOCK     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH COMPROBANTES WHERE ~{&KEY-PHRASE} NO-LOCK, ~
             FIRST CcbCDocu OF COMPROBANTES NO-LOCK     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table COMPROBANTES CcbCDocu
&Scoped-define FIRST-TABLE-IN-QUERY-br_table COMPROBANTES
&Scoped-define SECOND-TABLE-IN-QUERY-br_table CcbCDocu


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-62 FILL-IN-CodCli CMB-filtro ~
FILL-IN-filtro BUTTON-19 BUTTON-20 BUTTON-Division xcodmod C-CMPBTE c-Serie ~
f-desde f-hasta x-CndCre x-FchVto-1 x-FchVto-2 x-FchCan-desde ~
x-FchCan-hasta COMBO-BOX-5 F-FMAPGO f-codven RADIO-SET-CndVta br_table 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-CodCli CMB-filtro FILL-IN-filtro ~
x-CodDiv xcodmod C-CMPBTE c-Serie f-desde f-hasta x-CndCre x-FchVto-1 ~
x-FchVto-2 x-FchCan-desde x-FchCan-hasta COMBO-BOX-5 F-FMAPGO F-CndVta ~
f-codven F-nomven RADIO-SET-CndVta f-totsol f-totdol f-sdosol f-sdodol ~
FILL-IN-Mensaje 

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
<FOREIGN-KEYS></FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-get-referencia L-table-Win 
FUNCTION f-get-referencia RETURNS CHARACTER
  ( INPUT pCodDiv AS CHAR, INPUT pCodDoc AS CHAR, INPUT pNroDoc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD importe-fotocopia L-table-Win 
FUNCTION importe-fotocopia RETURNS DECIMAL
  ( INPUT pCodDoc AS CHAR, INPUT pNroDoc AS CHAR, pCodCje AS CHAR, INPUT pNroCje AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-19 
     LABEL "APLICAR FILTRO" 
     SIZE 21 BY 1.62
     FONT 6.

DEFINE BUTTON BUTTON-20 
     IMAGE-UP FILE "img/excel.bmp":U
     LABEL "" 
     SIZE 7 BY 1.62 TOOLTIP "Exportar a EXCEL".

DEFINE BUTTON BUTTON-Division 
     LABEL "..." 
     SIZE 4 BY .77 TOOLTIP "Selecciona Divisiones".

DEFINE VARIABLE CMB-filtro AS CHARACTER FORMAT "X(256)":U INITIAL "Todos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Todos","Que inicien con","Que contegan" 
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

DEFINE VARIABLE x-CodDiv AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 63 BY 2.5 NO-UNDO.

DEFINE VARIABLE c-Serie AS CHARACTER FORMAT "X(256)":U 
     LABEL "Serie" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-CndVta AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37.72 BY .69 NO-UNDO.

DEFINE VARIABLE f-codven AS CHARACTER FORMAT "X(3)":U 
     LABEL "Vendedor" 
     VIEW-AS FILL-IN 
     SIZE 5.14 BY .69
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE f-desde AS DATE FORMAT "99/99/9999":U 
     LABEL "Emitidos desde" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-FMAPGO AS CHARACTER FORMAT "x(8)":U 
     LABEL "Condici�n de venta" 
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

DEFINE VARIABLE FILL-IN-filtro AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-Mensaje AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 53 BY .81 NO-UNDO.

DEFINE VARIABLE x-FchCan-desde AS DATE FORMAT "99/99/9999":U 
     LABEL "Cancelados desde" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE x-FchCan-hasta AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81
     BGCOLOR 15  NO-UNDO.

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

DEFINE VARIABLE RADIO-SET-CndVta AS CHARACTER INITIAL "0" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todas", "0",
"Solo Contado", "1",
"Solo Cr�dito", "2"
     SIZE 12 BY 2.15 NO-UNDO.

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

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 139 BY 11.04.

DEFINE VARIABLE C-CMPBTE AS CHARACTER INITIAL "Todos" 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Todos","Todos" 
     SIZE 32 BY 3.65 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      COMPROBANTES, 
      CcbCDocu SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table L-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      fEstado() @ X-EST COLUMN-LABEL "Estado" FORMAT "x(15)":U
      COMPROBANTES.DivOri FORMAT "x(5)":U
      COMPROBANTES.CodDoc COLUMN-LABEL "Tipo" FORMAT "x(3)":U
      COMPROBANTES.NroDoc FORMAT "XXX-XXXXXXXX":U WIDTH 12
      COMPROBANTES.CodRef COLUMN-LABEL "Refer." FORMAT "x(3)":U
      COMPROBANTES.NroRef FORMAT "X(12)":U WIDTH 12
      COMPROBANTES.Libre_c04 FORMAT 'x(12)' WIDTH 12 COLUMN-LABEL 'G/R Entrega!Final'
      COMPROBANTES.NroOrd FORMAT "x(15)":U COLUMN-LABEL "Pedido Cliente"
      COMPROBANTES.CodCli FORMAT "x(15)":U
      COMPROBANTES.NomCli FORMAT "x(40)":U
      COMPROBANTES.RucCli FORMAT "x(15)":U
      COMPROBANTES.FmaPgo COLUMN-LABEL "Cnd.!Venta" FORMAT "X(3)":U
      COMPROBANTES.CodVen COLUMN-LABEL "Ven." FORMAT "x(5)":U
      COMPROBANTES.FchDoc COLUMN-LABEL "     Fecha    !    Emision" FORMAT "99/99/9999":U
      COMPROBANTES.FchAte COLUMN-LABEL "     Fecha    !de Deposito" FORMAT "99/99/9999":U
      COMPROBANTES.FchCbd COLUMN-LABEL "Fecha!Recepci�n" FORMAT "99/99/9999":U
      COMPROBANTES.FchVto FORMAT "99/99/9999":U
      COMPROBANTES.FchCan COLUMN-LABEL "Fecha de!Cancelacion" FORMAT "99/99/9999":U
      X-MON @ X-MON COLUMN-LABEL "Mon." FORMAT "XXXX":U
      COMPROBANTES.ImpTot COLUMN-LABEL "Importe Total" FORMAT "->>>,>>9.99":U
      COMPROBANTES.SdoAct COLUMN-LABEL "Saldo" FORMAT "->>>,>>9.99":U
      COMPROBANTES.ImpDto COLUMN-LABEL "Imp. Descuento" FORMAT "->>>,>>9.99":U WIDTH 11
      fUsuario() @ x-Usuario COLUMN-LABEL "Anulado por" FORMAT "x(10)":U
      fFecha() @ x-Fecha COLUMN-LABEL "Fecha" FORMAT "99/99/99":U WIDTH 6.57
      fHora() @ x-Hora COLUMN-LABEL "Hora" FORMAT "x(5)":U
      fNombre() @ x-Nombre COLUMN-LABEL "Motivo de anulaci�n" FORMAT "x(40)":U
      fUbicacion() @ x-Ubicacion COLUMN-LABEL "Ubicacion" FORMAT "x(10)":U
      fBanco() @ x-Banco COLUMN-LABEL "Banco" FORMAT "x(15)":U
      fSituacion() @ x-Situacion COLUMN-LABEL "Situaci�n" FORMAT "x(15)":U
      CcbCDocu.NroSal COLUMN-LABEL "Numero Unico" FORMAT "X(15)":U
      CcbCDocu.CodRef COLUMN-LABEL "Refer." FORMAT "x(4)":U
      CcbCDocu.NroRef COLUMN-LABEL "Numero Refer." FORMAT "X(12)":U
      COMPROBANTES.ImpTot + fAdelanto(COMPROBANTES.CodCia,COMPROBANTES.CodDoc,COMPROBANTES.NroDoc) @ x-ImpTot COLUMN-LABEL "Importe SIN!Adelanto" FORMAT "->>>,>>9.99":U
      COMPROBANTES.CodCta COLUMN-LABEL 'Concepto N/C Otras'
      COMPROBANTES.NomCta COLUMN-LABEL 'Descripcion'
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 139 BY 11
         BGCOLOR 15 FGCOLOR 0 FONT 4 ROW-HEIGHT-CHARS .5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN-CodCli AT ROW 1.27 COL 16 COLON-ALIGNED
     CMB-filtro AT ROW 1.27 COL 29 NO-LABEL
     FILL-IN-filtro AT ROW 1.27 COL 51 NO-LABEL
     BUTTON-19 AT ROW 1.27 COL 111 WIDGET-ID 86
     BUTTON-20 AT ROW 1.27 COL 132 WIDGET-ID 98
     x-CodDiv AT ROW 2.35 COL 18 NO-LABEL WIDGET-ID 90
     BUTTON-Division AT ROW 2.35 COL 81 WIDGET-ID 76
     xcodmod AT ROW 4.77 COL 128 NO-LABEL
     C-CMPBTE AT ROW 5.04 COL 18 NO-LABEL WIDGET-ID 6
     c-Serie AT ROW 5.04 COL 54 COLON-ALIGNED
     f-desde AT ROW 5.04 COL 84 COLON-ALIGNED
     f-hasta AT ROW 5.04 COL 100 COLON-ALIGNED
     x-CndCre AT ROW 5.85 COL 56 NO-LABEL
     x-FchVto-1 AT ROW 5.85 COL 84 COLON-ALIGNED
     x-FchVto-2 AT ROW 5.85 COL 100 COLON-ALIGNED
     x-FchCan-desde AT ROW 6.65 COL 84 COLON-ALIGNED WIDGET-ID 94
     x-FchCan-hasta AT ROW 6.65 COL 100 COLON-ALIGNED WIDGET-ID 96
     COMBO-BOX-5 AT ROW 7.73 COL 84 COLON-ALIGNED
     F-FMAPGO AT ROW 8.81 COL 16 COLON-ALIGNED
     F-CndVta AT ROW 8.81 COL 22 COLON-ALIGNED NO-LABEL
     f-codven AT ROW 8.81 COL 84 COLON-ALIGNED
     F-nomven AT ROW 8.81 COL 89 COLON-ALIGNED NO-LABEL
     RADIO-SET-CndVta AT ROW 9.62 COL 18 NO-LABEL WIDGET-ID 108
     br_table AT ROW 12.31 COL 2
     f-totsol AT ROW 23.92 COL 81.72 RIGHT-ALIGNED NO-LABEL
     f-totdol AT ROW 23.92 COL 83.72 NO-LABEL
     f-sdosol AT ROW 24.69 COL 67.72 NO-LABEL
     f-sdodol AT ROW 24.69 COL 83.72 NO-LABEL
     FILL-IN-Mensaje AT ROW 24.96 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 88
     "Documentos en S/." VIEW-AS TEXT
          SIZE 14 BY .5 AT ROW 23.35 COL 67.72
     "Moneda:" VIEW-AS TEXT
          SIZE 6 BY .5 AT ROW 4.96 COL 121 WIDGET-ID 78
     "(*)  Click Derecho - Visualiza Pagos" VIEW-AS TEXT
          SIZE 29 BY .73 AT ROW 23.42 COL 2
          FONT 6
     "Importe Total" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 23.92 COL 57.72
     "Documentos en US$/" VIEW-AS TEXT
          SIZE 15 BY .5 AT ROW 23.35 COL 83.72
     "Saldo Total" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 24.69 COL 57.72
     "Comprobante:" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 5.12 COL 8 WIDGET-ID 8
     "N/C:" VIEW-AS TEXT
          SIZE 4 BY .5 AT ROW 5.92 COL 52 WIDGET-ID 80
     "Divisi�n Origen:" VIEW-AS TEXT
          SIZE 11 BY .5 AT ROW 2.23 COL 7 WIDGET-ID 92
     "(*)  Doble Click - Visualiza Detalle" VIEW-AS TEXT
          SIZE 29 BY .73 AT ROW 24.08 COL 2.14
          FONT 6
     RECT-62 AT ROW 1 COL 2 WIDGET-ID 106
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
      ADDITIONAL-FIELDS:
          FIELD NomCta AS CHAR FORMAT 'x(60)'
          
      END-FIELDS.
      TABLE: t-gn-clie T "?" NO-UNDO INTEGRAL gn-clie
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
         HEIGHT             = 25.04
         WIDTH              = 141.86.
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
/* BROWSE-TAB br_table RADIO-SET-CndVta F-Main */
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
/* SETTINGS FOR EDITOR x-CodDiv IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH COMPROBANTES WHERE ~{&KEY-PHRASE} NO-LOCK,
      FIRST CcbCDocu OF COMPROBANTES NO-LOCK
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST"
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


&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 L-table-Win
ON CHOOSE OF BUTTON-19 IN FRAME F-Main /* APLICAR FILTRO */
DO:
  MESSAGE 'Procedemos con la carga de datos?' VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO UPDATE rpta AS LOG.
  IF rpta = NO THEN RETURN NO-APPLY.

  ASSIGN
      CMB-filtro FILL-IN-CodCli FILL-IN-filtro
      x-CodDiv xcodmod 
      C-CMPBTE c-Serie x-CndCre 
      f-desde f-hasta x-FchVto-1 x-FchVto-2
      COMBO-BOX-5  
      f-codven F-FMAPGO
      x-fchcan-desde x-fchcan-hasta.
  ASSIGN
      RADIO-SET-CndVta.

  IF f-desde = ? OR f-hasta = ? THEN DO:
      MESSAGE "Fechas de EMISION estan incorrectas" VIEW-AS ALERT-BOX INFORMATION.
      APPLY 'ENTRY':U TO f-Desde.
      RETURN NO-APPLY.
  END.

  IF f-desde > f-hasta THEN DO:
      MESSAGE "Fecha de emision DESDE debe ser menor/igual a HASTA" VIEW-AS ALERT-BOX INFORMATION.
      RETURN NO-APPLY.
  END.
  /*
  IF f-Hasta > ADD-INTERVAL(f-desde,6,'months') THEN DO:
      MESSAGE "Rango de Fechas de emision no debe exceder a 6 meses" VIEW-AS ALERT-BOX INFORMATION.
      APPLY 'ENTRY':U TO f-Desde.
      RETURN NO-APPLY.
  END.
  */

/*   IF (x-fchcan-hasta - x-fchcan-desde) > 95 THEN DO:                                                */
/*       MESSAGE "Rango de Fechas de emision no debe exceder a 3 meses" VIEW-AS ALERT-BOX INFORMATION. */
/*       RETURN NO-APPLY.                                                                              */
/*   END.                                                                                              */

  IF FILL-IN-CodCli > "" THEN DO:
      IF NOT CAN-FIND(FIRST gn-clie WHERE gn-clie.codcia = cl-codcia AND gn-clie.codcli = FILL-IN-CodCli NO-LOCK)
          THEN DO:
          MESSAGE 'Cliente no registrado' VIEW-AS ALERT-BOX ERROR.
          APPLY 'ENTRY':U TO FILL-IN-CodCli.
          RETURN NO-APPLY.
      END.
  END.

  IF TRUE <> (x-coddiv > '') THEN DO:
      MESSAGE 'Debe seleccionar al menos una divisi�n' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.

  RUN Carga-Temporal.
  /*RUN dispatch IN THIS-PROCEDURE ('open-query':U).*/
  {&OPEN-QUERY-{&BROWSE-NAME}}
  RUN Calcula-Importe.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-20
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-20 L-table-Win
ON CHOOSE OF BUTTON-20 IN FRAME F-Main
DO:
  MESSAGE 'Procedemos con la exportaci�n a Excel?' VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO UPDATE rpta AS LOG.
  IF rpta = NO THEN RETURN NO-APPLY.

  /*RUN texto.*/
  RUN Excel.

  HIDE FRAME F-Proceso.
  MESSAGE 'Proceso Terminado' VIEW-AS ALERT-BOX INFO BUTTONS OK.

  {&OPEN-QUERY-{&BROWSE-NAME}}
  /*RUN dispatch IN THIS-PROCEDURE ('open-query':U).*/

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


&Scoped-define SELF-NAME COMBO-BOX-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-5 L-table-Win
ON VALUE-CHANGED OF COMBO-BOX-5 IN FRAME F-Main /* Estado */
DO:
  ASSIGN COMBO-BOX-5.
  CASE COMBO-BOX-5:
       WHEN "Pendiente" THEN 
          ASSIGN F-ESTADO = "P,J".
       WHEN "Cancelado" THEN 
          ASSIGN F-ESTADO = "C".
      WHEN "Cobranza Dudosa" THEN
          ASSIGN F-ESTADO = "J".
       WHEN "Anulado" THEN 
          ASSIGN F-ESTADO = "A".
       WHEN "Total Neto" THEN 
          ASSIGN F-ESTADO = "P,C,J".
       OTHERWISE
          ASSIGN F-ESTADO = "P,C,A,J".        
  END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-codven
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-codven L-table-Win
ON LEAVE OF f-codven IN FRAME F-Main /* Vendedor */
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
ON MOUSE-SELECT-DBLCLICK OF f-codven IN FRAME F-Main /* Vendedor */
DO:
  RUN lkup\c-vende.r("Maestro de Vendedores").
  IF output-var-2 = ? THEN output-var-2 = "".
  F-codven:SCREEN-VALUE = output-var-2.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-FMAPGO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-FMAPGO L-table-Win
ON LEAVE OF F-FMAPGO IN FRAME F-Main /* Condici�n de venta */
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
ON MOUSE-SELECT-DBLCLICK OF F-FMAPGO IN FRAME F-Main /* Condici�n de venta */
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
      SELF:SCREEN-VALUE = ''.
      RETURN NO-APPLY.
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

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&BROWSE-NAME}}

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

/*     X_TOT_IS = 0.                                      */
/*     X_TOT_SS = 0.                                      */
/*     X_TOT_ID = 0.                                      */
/*     X_TOT_SD = 0.                                      */
/*                                                        */
/*  SESSION:SET-WAIT-STATE('GENERAL').                    */
/*  GET FIRST {&BROWSE-NAME}.                             */
/*  DO WHILE AVAILABLE(COMPROBANTES):                     */
/*     IF COMPROBANTES.CodMon = 1 THEN                    */
/*         ASSIGN                                         */
/*             X_TOT_IS = X_TOT_IS + COMPROBANTES.ImpTot  */
/*             X_TOT_SS = X_TOT_SS + COMPROBANTES.Sdoact. */
/*     ELSE                                               */
/*         ASSIGN                                         */
/*             X_TOT_ID = X_TOT_ID + COMPROBANTES.ImpTot  */
/*             X_TOT_SD = X_TOT_SD + COMPROBANTES.Sdoact. */
/*     GET NEXT {&BROWSE-NAME}.                           */
/*  END.                                                  */
/*  SESSION:SET-WAIT-STATE('').                           */
 
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

DEF VAR iContador AS INTE NO-UNDO.

&SCOPED-DEFINE Rutina-General ~
/* Filtro principal */                                                                          ~
IF NOT (LOOKUP(Ccbcdocu.DivOri, x-coddiv) > 0) THEN NEXT.                     ~
IF NOT (TRUE <> (c-Serie > '') OR CcbcDocu.NroDoc BEGINS c-Serie) THEN NEXT.                    ~
IF NOT (CcbcDocu.FchDoc >= f-desde AND CcbCdocu.FchDoc <= f-hasta ) THEN NEXT.                  ~
IF NOT (x-FchVto-1 = ? OR CcbCDocu.FchVto >= x-FchVto-1) THEN NEXT.                             ~
IF NOT (x-FchVTo-2 = ? OR CcbCDocu.FchVto <= x-FchVto-2) THEN NEXT.                             ~
IF NOT (x-FchCan-desde = ? OR CcbCDocu.FchCan >= x-FchCan-desde) THEN NEXT.                     ~
IF NOT (x-FchCan-hasta = ? OR CcbCDocu.FchCan <= x-FchCan-hasta) THEN NEXT.                     ~
IF NOT (TRUE <> (F-CONDI > '') OR CcbcDocu.FmaPgo = F-CONDI) THEN NEXT.                         ~
IF NOT (TRUE <> (x-CodVen > '') OR CcbCdocu.CodVen = x-codven) THEN NEXT.                       ~
IF NOT (xcodmod = 3 OR ccbcdocu.codmon = xcodmod) THEN NEXT.                                    ~
/* 19/09/2024: Gina Condor, fitro general para las condiciones de venta */                      ~
IF RADIO-SET-CndVta <> '0' AND TRUE <> (F-CONDI > '') AND LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL') > 0 THEN DO: ~
    FIND gn-ConVt WHERE gn-ConVt.Codig = Ccbcdocu.fmapgo NO-LOCK NO-ERROR.                      ~
    IF AVAILABLE gn-ConVt AND gn-ConVt.TipVta <> RADIO-SET-CndVta THEN NEXT.                                           ~
END.                                                                                            ~
/* Fin de filtro principal */ ~                                                                 ~
IF Ccbcdocu.coddoc = "N/C" AND NOT (x-CndCre = 'T' OR CcbCDocu.CndCre = x-CndCre) THEN NEXT.    ~
Fi-Mensaje = Ccbcdocu.divori + " " + STRING(Ccbcdocu.fchdoc) + " " + Ccbcdocu.coddoc + " " + Ccbcdocu.nrodoc. ~
iContador = iContador + 1.                                                                      ~
IF iContador MODULO 1000 = 0 THEN DISPLAY Fi-Mensaje WITH FRAME F-Proceso.                      ~
CREATE COMPROBANTES.                                                                            ~
BUFFER-COPY Ccbcdocu EXCEPT ccbcdocu.fchate ccbcdocu.libre_c04 TO COMPROBANTES.                                    ~
IF Ccbcdocu.coddoc = 'BD' THEN COMPROBANTES.fchate = ccbcdocu.fchate.                           ~
ASSIGN x-codcta = '' x-nomcta = ''.                                                             ~
IF Ccbcdocu.CodDoc = 'N/C' AND Ccbcdocu.CndCre = 'N' THEN DO:                                   ~
    FIND FIRST CcbTabla WHERE CcbTabla.CodCia = s-CodCia                                        ~
        AND CcbTabla.Tabla = Ccbcdocu.CodDoc                                                    ~
        AND CcbTabla.Codigo = Ccbcdocu.CodCta                                                   ~
        NO-LOCK NO-ERROR.                                                                       ~
    IF AVAILABLE CcbTabla THEN ASSIGN x-CodCta = CCbTabla.Codigo x-NomCta = CcbTabla.Nombre.    ~
END.                                                                                            ~
ASSIGN                                                                                          ~
    COMPROBANTES.CodCta = x-CodCta                                                              ~
    COMPROBANTES.NomCta = x-NomCta.                                                             ~
x-Factor = 1.                                                                                   ~
FIND FacDocum OF CcbCDocu NO-LOCK NO-ERROR.                                                     ~
IF AVAILABLE FacDocum THEN x-Factor = IF FacDocum.TpoDoc = NO THEN -1 ELSE 1.                   ~
IF Ccbcdocu.flgest = 'A' THEN x-Factor = 0.                                                     ~
ASSIGN                                                                                          ~
    COMPROBANTES.ImpTot = x-Factor * COMPROBANTES.ImpTot                                        ~
    COMPROBANTES.SdoAct = x-Factor * COMPROBANTES.SdoAct                                        ~
    COMPROBANTES.ImpDto = x-Factor * COMPROBANTES.ImpDto.                                       ~
x-doc-ref = f-get-referencia(COMPROBANTES.coddiv, COMPROBANTES.coddoc, COMPROBANTES.nrodoc).    ~
ASSIGN                                                                                          ~
    COMPROBANTES.codref = ENTRY(1,x-doc-ref,"|")                                                ~
    COMPROBANTES.nroref = ENTRY(2,x-doc-ref,"|").                                               ~
IF COMPROBANTES.ruccli = '' THEN DO:                                                            ~
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia                                               ~
        AND gn-clie.codcli = ccbcdocu.codcli                                                    ~
        NO-LOCK NO-ERROR.                                                                       ~
    IF AVAILABLE gn-clie THEN COMPROBANTES.ruccli = gn-clie.ruc.                                ~
END.                                                                                            ~
IF COMPROBANTES.CodMon = 1 THEN                                                                 ~
    ASSIGN                                                                                      ~
        X_TOT_IS = X_TOT_IS + COMPROBANTES.ImpTot                                               ~
        X_TOT_SS = X_TOT_SS + COMPROBANTES.Sdoact.                                              ~
ELSE                                                                                            ~
    ASSIGN                                                                                      ~
        X_TOT_ID = X_TOT_ID + COMPROBANTES.ImpTot                                               ~
        X_TOT_SD = X_TOT_SD + COMPROBANTES.Sdoact.


DEF VAR x-Factor AS INT NO-UNDO.
DEF VAR x-CodDoc AS CHAR NO-UNDO.
DEF VAR k AS INT NO-UNDO.

DEFINE VAR x-fecha AS DATE.

DEFINE VAR x-doc-ref AS CHAR.

IF LOOKUP('Todos',c-CmpBte) > 0 THEN x-CodDoc = x-Comprobantes.
ELSE DO:
    DO k = 1 TO NUM-ENTRIES(c-CmpBte):
        x-CodDoc = x-CodDoc + (IF x-CodDoc = '' THEN '' ELSE ',') + ENTRY(k, c-CmpBte).
    END.
END.

EMPTY TEMP-TABLE COMPROBANTES. 

DEF VAR j AS INT NO-UNDO.
DEF VAR cFlgEst AS CHAR NO-UNDO.
DEF VAR cCodDoc AS CHAR NO-UNDO.
DEF VAR x-CodCta AS CHAR NO-UNDO.
DEF VAR x-NomCta AS CHAR NO-UNDO.
/* Variables para los totales */
X_TOT_IS = 0.
X_TOT_SS = 0.
X_TOT_ID = 0.
X_TOT_SD = 0.

CASE TRUE:
    WHEN FILL-IN-CodCli > '' THEN DO:
        FOR EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = 1 AND
            Ccbcdocu.codcli = FILL-IN-CodCli AND
            LOOKUP(Ccbcdocu.flgest, f-Estado) > 0 AND
            LOOKUP(Ccbcdocu.coddoc, x-coddoc) > 0:
            {&RUTINA-GENERAL}
        END.
    END.
    WHEN FILL-IN-filtro > '' AND CMB-filtro <> 'Todos' THEN DO:
        EMPTY TEMP-TABLE t-gn-clie.
        CASE CMB-Filtro: 
            WHEN 'Que inicien con' THEN DO:
                FOR EACH gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia AND
                    gn-clie.nomcli BEGINS FILL-IN-Filtro:
                    CREATE t-gn-clie.
                    BUFFER-COPY gn-clie USING gn-clie.codcia gn-clie.codcli TO t-gn-clie.
                END.
            END.
            WHEN 'Que contegan' THEN DO:
                FOR EACH gn-clie NO-LOCK WHERE gn-clie.codcia = cl-codcia AND
                    INDEX(Ccbcdocu.nomcli,FILL-IN-filtro) > 0:
                    CREATE t-gn-clie.
                    BUFFER-COPY gn-clie USING gn-clie.codcia gn-clie.codcli TO t-gn-clie.
                END.
            END.
        END CASE.
        FOR EACH t-gn-clie NO-LOCK:
            FILL-IN-CodCli = t-gn-clie.codcli.
            FOR EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = 1 AND
                Ccbcdocu.codcli = FILL-IN-CodCli AND
                LOOKUP(Ccbcdocu.flgest, f-Estado) > 0 AND
                LOOKUP(Ccbcdocu.coddoc, x-coddoc) > 0:
                {&RUTINA-GENERAL}
            END.
            FILL-IN-CodCli = "".
        END.
    END.
    OTHERWISE DO:
        FOR EACH gn-divi NO-LOCK WHERE gn-divi.codcia = s-codcia AND LOOKUP(gn-divi.coddiv, x-CODDIV) > 0:
            DO j = 1 TO NUM-ENTRIES(x-CodDoc):
                cCodDoc = ENTRY(j, x-CodDoc).
                FOR EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-codcia
                    AND Ccbcdocu.DivOri = Gn-divi.CodDiv
                    AND Ccbcdocu.FchDoc >= f-Desde
                    AND Ccbcdocu.FchDoc <= f-Hasta
                    AND Ccbcdocu.CodDoc = cCodDoc
                    AND LOOKUP(Ccbcdocu.flgest, f-Estado) > 0:
                    {&RUTINA-GENERAL}
                END.
            END.
        END.
    END.
END CASE.
/* 04/11/2024 : Gina Condor agregar G/R segundo tramo */
FOR EACH COMPROBANTES EXCLUSIVE-LOCK WHERE LOOKUP(COMPROBANTES.coddoc, 'FAC,BOL') > 0:
    FOR EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-codcia AND
        Ccbcdocu.codref = COMPROBANTES.coddoc AND
        Ccbcdocu.nroref = COMPROBANTES.nrodoc AND
        Ccbcdocu.coddoc = "G/R" AND
        Ccbcdocu.coddiv <> COMPROBANTES.coddiv AND
        Ccbcdocu.flgest <> "A":
        ASSIGN
            COMPROBANTES.Libre_c04 = Ccbcdocu.nrodoc.
        LEAVE.
    END.
END.

HIDE FRAME F-Proceso.

FILL-IN-Mensaje:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ''.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel L-table-Win 
PROCEDURE Excel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pto AS LOGICAL NO-UNDO.

EMPTY TEMP-TABLE t-ccbcdocu.
EMPTY TEMP-TABLE ttCanjes.

FOR EACH COMPROBANTES:
    CREATE t-ccbcdocu.
    BUFFER-COPY COMPROBANTES TO t-ccbcdocu.
END.
FIND FIRST t-ccbcdocu NO-LOCK NO-ERROR.
IF NOT AVAILABLE t-ccbcdocu THEN DO:
    MESSAGE 'NO hay informaci�n' VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.

DEFINE VAR lde_ImpFoto AS DEC INIT 0.
DEFINE VAR lde_Imptotal AS DEC NO-UNDO.
DEFINE VAR x-factor AS DEC INIT 0.
DEFINE VAR x-ImpTotal AS DEC NO-UNDO.
DEFINE VAR x-SdoAct AS DEC NO-UNDO.
/* create a new Excel Application object */
CREATE "Excel.Application" chExcelApplication.
/* create a new Workbook */
chWorkbook = chExcelApplication:Workbooks:Add().
/* get the active Worksheet */
chWorkSheet = chExcelApplication:Sheets:Item(1).
chWorkSheet:Range("A2"):Value = "Doc".
chWorkSheet:Range("B2"):Value = "Numero".
chWorkSheet:COLUMNS("B"):NumberFormat = "@".
chWorkSheet:Range("C2"):Value = "Ref.".
chWorkSheet:Range("D2"):Value = "Numero".
chWorkSheet:COLUMNS("D"):NumberFormat = "@".

chWorkSheet:Range("E2"):Value = "G/R Entrega Final".
chWorkSheet:COLUMNS("E"):NumberFormat = "@".


chWorkSheet:Range("F2"):Value = "Pedido Cliente".
chWorkSheet:COLUMNS("F"):NumberFormat = "@".
chWorkSheet:Range("G2"):Value = "C�digo".
chWorkSheet:COLUMNS("G"):NumberFormat = "@".
chWorkSheet:Range("H2"):Value = "Cliente".
chWorkSheet:COLUMNS("H"):NumberFormat = "@".
chWorkSheet:Range("I2"):Value = "RUC".
chWorkSheet:COLUMNS("I"):NumberFormat = "@".
chWorkSheet:Range("J2"):Value = "Cond. Vta.".
chWorkSheet:COLUMNS("J"):NumberFormat = "@".
chWorkSheet:Range("K2"):Value = "Vendedor".
chWorkSheet:COLUMNS("K"):NumberFormat = "@".
chWorkSheet:Range("L2"):Value = "Nombre del Vendedor".
chWorkSheet:Range("M2"):Value = "Emision".
chWorkSheet:COLUMNS("M"):NumberFormat = "dd/mm/yyyy".
chWorkSheet:Range("N2"):Value = "Deposito".
chWorkSheet:COLUMNS("N"):NumberFormat = "dd/mm/yyyy".
chWorkSheet:Range("O2"):Value = "Vencimiento".
chWorkSheet:COLUMNS("O"):NumberFormat = "dd/mm/yyyy".
chWorkSheet:Range("P2"):Value = "Recepci�n".
chWorkSheet:COLUMNS("P"):NumberFormat = "dd/mm/yyyy".
chWorkSheet:Range("Q2"):Value = "Cancelaci�n".
chWorkSheet:COLUMNS("Q"):NumberFormat = "dd/mm/yyyy".

RUN Excel-Cabecera.

/* Documento de la Hoja de Ruta */
DEFINE VAR lCodRef AS CHAR.
DEFINE VAR lNroRef AS CHAR.
DEFINE VAR lFchEntrega AS DATE.
DEF VAR x-Canje AS CHAR NO-UNDO.
DEF VAR x-CodCta AS CHAR NO-UNDO.
DEF VAR x-NomCta AS CHAR NO-UNDO.

t-column = 2.
FOR EACH t-ccbcdocu NO-LOCK, FIRST ccbcdocu OF t-ccbcdocu NO-LOCK:
    ASSIGN 
        x-codcta = '' 
        x-nomcta = ''.
    IF Ccbcdocu.CodDoc = "N/C" AND Ccbcdocu.CndCre = "N" THEN DO:
        FIND FIRST CcbTabla WHERE CcbTabla.CodCia = s-CodCia
            AND CcbTabla.Tabla = Ccbcdocu.CodDoc
            AND CcbTabla.Codigo = Ccbcdocu.CodCta
            NO-LOCK NO-ERROR.
        IF AVAILABLE CcbTabla THEN ASSIGN x-CodCta = CCbTabla.Codigo x-NomCta = CcbTabla.Nombre.
    END.
    /* Para buscarlo en la hoja de RUTA */
    lCodRef = t-ccbcdocu.coddoc.
    lNroRef = t-ccbcdocu.nrodoc.
    lFchEntrega = ?.
    IF t-ccbcdocu.codref = 'G/R' THEN DO:
        lCodRef = t-ccbcdocu.codref.
        lNroRef = t-ccbcdocu.nroref.
    END.
    /* Buscar la el Documento en la HR */
    FOR EACH di-RutaD USE-INDEX llave02 NO-LOCK WHERE di-RutaD.codcia = s-codcia AND 
        di-RutaD.coddoc = 'H/R' AND 
        di-RutaD.codref = lCodRef AND 
        di-RutaD.NroRef = lNroRef AND
        di-RutaD.flgest <> 'A', 
        FIRST di-rutaC OF di-rutaD NO-LOCK WHERE di-RutaC.flgest <> 'A':
        IF lFchEntrega = ? THEN lFchEntrega = di-RutaC.fchsal.
        IF lFchEntrega < di-RutaC.fchsal THEN lFchEntrega = di-RutaC.fchsal.
    END.
    t-column = t-column + 1.
    cColumn = STRING(t-Column).
    cRange = "A" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.coddoc.
    cRange = "B" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.nrodoc.
    cRange = "C" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.codref.
    cRange = "D" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.nroref.

    cRange = "E" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.libre_c04.

    cRange = "F" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.nroord.
    cRange = "G" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.codcli.
    cRange = "H" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.nomcli.
    cRange = "I" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.ruccli.
    cRange = "J" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.fmapgo.
    cRange = "K" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.codven.
    FIND gn-ven WHERE gn-ven.CodCia = s-codcia
        AND gn-ven.CodVen = t-ccbcdocu.codven NO-LOCK NO-ERROR.
    IF AVAILABLE gn-ven THEN DO:
        cRange = "L" + cColumn.
        chWorkSheet:Range(cRange):Value = gn-ven.NomVen.
    END.
    cRange = "M" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.fchdoc.
    cRange = "N" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.fchate.
    cRange = "O" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.fchvto.
    cRange = "P" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.fchcbd.
    cRange = "Q" + cColumn.
    chWorkSheet:Range(cRange):Value = t-ccbcdocu.fchcan.
    x-ImpTot = t-ccbcdocu.imptot + fAdelanto(t-ccbcdocu.codcia, t-ccbcdocu.coddoc, t-ccbcdocu.nrodoc).
    x-SdoAct = t-ccbcdocu.sdoact.
    FIND Ccbaudit OF t-ccbcdocu NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbaudit THEN FIND ccbtabla WHERE ccbtabla.codcia = t-ccbcdocu.codcia
        AND ccbtabla.tabla = 'MA' AND ccbtabla.codigo = ccbaudit.codref NO-LOCK NO-ERROR.

    CASE xcodmod:
        WHEN 1 OR WHEN 2 THEN DO:
            cRange = "R" + cColumn.
            chWorkSheet:Range(cRange):Value = t-ccbcdocu.imptot.
            cRange = "S" + cColumn.
            chWorkSheet:Range(cRange):Value = x-ImpTot.
            cRange = "T" + cColumn.
            chWorkSheet:Range(cRange):Value = x-SdoAct.
            cRange = "U" + cColumn.
            /*chWorkSheet:Range(cRange):Value = fEstado-1(t-ccbcdocu.flgest).*/
            chWorkSheet:Range(cRange):Value = fEstado().
            IF AVAILABLE ccbtabla AND AVAILABLE ccbaudit THEN DO:
                cRange = "V" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbaudit.usuario.
                cRange = "W" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbaudit.fecha.
                cRange = "X" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbaudit.hora.
                cRange = "Y" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbtabla.nombre.
            END.
            cRange = "Z" + cColumn.
            chWorkSheet:Range(cRange):Value = "'" + t-ccbcdocu.DivOri.
            cRange = "AA" + cColumn.
            chWorkSheet:Range(cRange):Value = TODAY.
            cRange = "AB" + cColumn.
            chWorkSheet:Range(cRange):Value = fUbicacion().
            cRange = "AC" + cColumn.
            chWorkSheet:Range(cRange):Value = fBanco().
            cRange = "AD" + cColumn.
            chWorkSheet:Range(cRange):Value = fSituacion().
            cRange = "AE" + cColumn.
            chWorkSheet:Range(cRange):Value = lFchEntrega.
            cRange = "AF" + cColumn.
            chWorkSheet:Range(cRange):Value = t-ccbcdocu.fchcbd.
            cRange = "AG" + cColumn.
            chWorkSheet:Range(cRange):Value = t-ccbcdocu.dircli.

            cRange = "AM" + cColumn.
            chWorkSheet:Range(cRange):Value = x-CodCta.
            cRange = "AN" + cColumn.
            chWorkSheet:Range(cRange):Value = x-NomCta.

        END.
        WHEN 3 THEN DO:
            IF t-ccbcdocu.codmon = 1 THEN DO:
                cRange = "R" + cColumn.
                chWorkSheet:Range(cRange):Value = t-ccbcdocu.imptot.
                cRange = "S" + cColumn.
                chWorkSheet:Range(cRange):Value = x-ImpTot.
                cRange = "T" + cColumn.
                chWorkSheet:Range(cRange):Value = x-sdoact.
            END.
            IF t-ccbcdocu.codmon = 2 THEN DO:
                cRange = "U" + cColumn.
                chWorkSheet:Range(cRange):Value = t-ccbcdocu.imptot.
                cRange = "V" + cColumn.
                chWorkSheet:Range(cRange):Value = x-ImpTot.
                cRange = "W" + cColumn.
                chWorkSheet:Range(cRange):Value = x-sdoact.
            END.
            cRange = "X" + cColumn.
            chWorkSheet:Range(cRange):Value = fEstado().
            IF AVAILABLE ccbtabla AND AVAILABLE ccbaudit THEN DO:
                cRange = "Y" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbaudit.usuario.
                cRange = "Z" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbaudit.fecha.
                cRange = "AA" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbaudit.hora.
                cRange = "AB" + cColumn.
                chWorkSheet:Range(cRange):Value = ccbtabla.nombre.
            END.
            cRange = "AC" + cColumn.
            chWorkSheet:Range(cRange):Value = "'" + t-ccbcdocu.DivOri.
            cRange = "AD" + cColumn.
            chWorkSheet:Range(cRange):Value = TODAY.
            cRange = "AE" + cColumn.
            chWorkSheet:Range(cRange):Value = fUbicacion().
            cRange = "AF" + cColumn.
            chWorkSheet:Range(cRange):Value = fBanco().
            cRange = "AG" + cColumn.
            chWorkSheet:Range(cRange):Value = fSituacion().
            cRange = "AH" + cColumn.
            chWorkSheet:Range(cRange):Value = lFchEntrega.
            cRange = "AI" + cColumn.
            chWorkSheet:Range(cRange):Value = t-ccbcdocu.fchcbd.
            cRange = "AJ" + cColumn.
            chWorkSheet:Range(cRange):Value = t-ccbcdocu.dircli.

            cRange = "AR" + cColumn.
            chWorkSheet:Range(cRange):Value = x-CodCta.
            cRange = "AS" + cColumn.
            chWorkSheet:Range(cRange):Value = x-NomCta.

        END.
    END CASE.
    /* RHC 03/04/17 Verifica si tiene canje pendiente */
    x-Canje = "".
    IF LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0
        AND ccbcdocu.flgest = "P"
        AND ccbcdocu.FlgSit = 'X' THEN DO:
        /* Buscamos canje */
        CANJE:
        FOR EACH ccbcmvto NO-LOCK WHERE CcbCMvto.CodCia = ccbcdocu.codcia
            AND CcbCMvto.CodDoc = "CJE"
            AND CcbCMvto.FchDoc >= ccbcdocu.fchdoc
            AND CcbCMvto.CodCli = ccbcdocu.codcli
            AND CcbCMvto.FlgEst = "P":
            FOR EACH Ccbdmvto NO-LOCK WHERE CcbDMvto.CodCia = Ccbcmvto.codcia
                AND CcbDMvto.CodDoc = Ccbcmvto.coddoc
                AND CcbDMvto.NroDoc = Ccbcmvto.nrodoc
                AND CcbDMvto.TpoRef = "O"
                AND CcbDMvto.CodRef = Ccbcdocu.coddoc
                AND CcbDMvto.NroRef = Ccbcdocu.nrodoc:
                x-Canje = "CANJE DE LETRA".
                LEAVE CANJE.
            END.
        END.
    END.
    IF x-Canje > '' THEN DO:
        CASE xcodmod:
            WHEN 1 OR WHEN 2 THEN DO:
                cRange = "AH" + cColumn.
                chWorkSheet:Range(cRange):Value = x-Canje.
            END.
            WHEN 3 THEN DO:
                cRange = "AK" + cColumn.
                chWorkSheet:Range(cRange):Value = x-Canje.
            END.
        END CASE.
    END.
    /* RHC 06/06/2017 Datos adicionales Julissa Calderon */
    DEF VAR pCodDepto AS CHAR.
    DEF VAR pNomDepto AS CHAR.
    DEF VAR pCodProvi AS CHAR.
    DEF VAR pNomProvi AS CHAR.
    DEF VAR pCodDistr AS CHAR.
    DEF VAR pNomDistr AS CHAR.
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia AND gn-clie.codcli = t-ccbcdocu.codcli
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie THEN DO:
        RUN gn/ubigeo-cliente (
            INPUT gn-clie.CodCia ,
            INPUT gn-clie.CodCli ,
            OUTPUT pCodDepto, 
            OUTPUT pNomDepto, 
            OUTPUT pCodProvi, 
            OUTPUT pNomProvi, 
            OUTPUT pCodDistr, 
            OUTPUT pNomDistr 
            ).
        CASE xcodmod:
            WHEN 1 OR WHEN 2 THEN DO:
                cRange = "AI" + cColumn.
                chWorkSheet:Range(cRange):Value = pNomDepto.
                cRange = "AJ" + cColumn.
                chWorkSheet:Range(cRange):Value = pNomProvi.
                cRange = "AK" + cColumn.
                chWorkSheet:Range(cRange):Value = pNomDistr.
                cRange = "AL" + cColumn.
                chWorkSheet:Range(cRange):Value = (IF t-ccbcdocu.coddoc = 'LET' THEN t-ccbcdocu.nrosal ELSE '').
            END.
            WHEN 3 THEN DO:
                cRange = "AL" + cColumn.
                chWorkSheet:Range(cRange):Value = pNomDepto.
                cRange = "AM" + cColumn.
                chWorkSheet:Range(cRange):Value = pNomProvi.
                cRange = "AN" + cColumn.
                chWorkSheet:Range(cRange):Value = pNomDistr.
                cRange = "AO" + cColumn.
                chWorkSheet:Range(cRange):Value = (IF t-ccbcdocu.coddoc = 'LET' THEN t-ccbcdocu.nrosal ELSE '').
            END.
        END CASE.
    END.
    /* Ic - 30Oct2018, pedido de Julissa Calderon*/
    lde_ImpFoto = 0.
    lde_ImpTotal = 0.
    x-factor = 0.
    IF t-ccbcdocu.sdoact > 0 THEN DO:
        /* % Saldo pendiente */
        IF t-ccbcdocu.coddoc = 'LET' AND t-ccbcdocu.codref = 'CJE' THEN DO:
            x-factor = importe-fotocopia(INPUT t-ccbcdocu.coddoc, INPUT t-ccbcdocu.nrodoc, 
                                            INPUT t-ccbcdocu.codref, INPUT t-ccbcdocu.nroref).
        END.
        ELSE DO:
            FOR EACH ccbddocu OF t-ccbcdocu NO-LOCK, FIRST Almmmatg NO-LOCK WHERE Almmmatg.codcia = Ccbddocu.codcia
                AND Almmmatg.codmat = Ccbddocu.codmat :

                IF Almmmatg.codfam = '011' THEN lde_ImpFoto = lde_ImpFoto + Ccbddocu.ImpLin.
                lde_ImpTotal = lde_ImpTotal + Ccbddocu.ImpLin.
            END.
            x-factor = ROUND (lde_ImpFoto / lde_ImpTotal,4).
        END.
        lde_ImpFoto = t-ccbcdocu.sdoact * x-factor.
    END.
    /* Es papel fotocopia */
    IF lde_ImpFoto > 0 THEN DO:        
        IF t-ccbcdocu.codmon = 2 THEN DO:
            cRange = "AP" + cColumn.
            chWorkSheet:Range(cRange):Value = lde_ImpFoto.    /*lde_ImpFoto.*/
        END.
        ELSE DO:
            cRange = "AP" + cColumn.
            chWorkSheet:Range(cRange):Value = lde_ImpFoto.     /*lde_ImpFoto.*/
        END.
    END.
    DISPLAY Fi-Mensaje WITH FRAME F-Proceso.
END.
/* launch Excel so it is visible to the user */
chExcelApplication:Visible = TRUE.
/* release com-handles */
RELEASE OBJECT chExcelApplication.      
RELEASE OBJECT chWorkbook.
RELEASE OBJECT chWorksheet.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Excel-Cabecera L-table-Win 
PROCEDURE Excel-Cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

CASE xcodmod:
    WHEN 1 THEN DO:
        chWorkSheet:Range("R2"):Value = "Importe S/.".
        chWorkSheet:Range("S2"):Value = "Importe S/. SIN Adelanto".
        chWorkSheet:Range("T2"):Value = "Saldo S/.".
        chWorkSheet:Range("U2"):Value = "Estado".
        chWorkSheet:Range("V2"):Value = "Usuario anulaci�n".
        chWorkSheet:Range("W2"):Value = "Fecha".
        chWorkSheet:COLUMNS("W"):NumberFormat = "dd/mm/yyyy".
        chWorkSheet:Range("X2"):Value = "Hora".
        chWorkSheet:Range("Y2"):Value = "Motivo anulaci�n".
        chWorkSheet:Range("Z2"):Value = "Divisi�n".
        chWorkSheet:COLUMNS("Z"):NumberFormat = "@".
        chWorkSheet:Range("AA2"):Value = "Fecha Reporte".
        chWorkSheet:COLUMNS("AA"):NumberFormat = "dd/mm/yyyy".
        chWorkSheet:Range("AB2"):Value = "Ubicaci�n".
        chWorkSheet:Range("AC2"):Value = "Banco".
        chWorkSheet:Range("AD2"):Value = "Situaci�n".
        chWorkSheet:Range("AE2"):Value = "F.Entrega(DIST)".
        chWorkSheet:Range("AF2"):Value = "F.Entrega(CRED)".
        chWorkSheet:Range("AG2"):Value = "DIRECC. CLIENTE".
        chWorkSheet:Range("AH2"):Value = "CANJE".
        chWorkSheet:Range("AI2"):Value = "DEPARTAMENTO".
        chWorkSheet:Range("AJ2"):Value = "PROVINCIA".
        chWorkSheet:Range("AK2"):Value = "DISTRITO".
        chWorkSheet:Range("AL2"):Value = "N� UNICO".
        chWorkSheet:Range("AM2"):Value = "Concepto N/C Otras".
        chWorkSheet:COLUMNS("AM"):NumberFormat = "@".
        chWorkSheet:Range("AN2"):Value = "Descripci�n".
    END.
    WHEN 2 THEN DO:
        chWorkSheet:Range("R2"):Value = "Importe US$".
        chWorkSheet:Range("S2"):Value = "Importe US$ SIN Adelanto".
        chWorkSheet:Range("T2"):Value = "Saldo US$".
        chWorkSheet:Range("U2"):Value = "Estado".
        chWorkSheet:Range("V2"):Value = "Usuario anulaci�n".
        chWorkSheet:Range("W2"):Value = "Fecha".
        chWorkSheet:COLUMNS("W"):NumberFormat = "dd/mm/yyyy".
        chWorkSheet:Range("X2"):Value = "Hora".
        chWorkSheet:Range("Y2"):Value = "Motivo anulaci�n".
        chWorkSheet:Range("Z2"):Value = "Divisi�n".
        chWorkSheet:COLUMNS("Z"):NumberFormat = "@".
        chWorkSheet:Range("AA2"):Value = "Fecha Reporte".
        chWorkSheet:COLUMNS("AA"):NumberFormat = "dd/mm/yyyy".
        chWorkSheet:Range("AA2"):Value = "Ubicaci�n".
        chWorkSheet:Range("AC2"):Value = "Banco".
        chWorkSheet:Range("AD2"):Value = "Situaci�n".
        chWorkSheet:Range("AE2"):Value = "F.Entrega(DIST)".
        chWorkSheet:Range("AF2"):Value = "F.Entrega(CRED)".
        chWorkSheet:Range("AG2"):Value = "DIRECC. CLIENTE".
        chWorkSheet:Range("AH2"):Value = "CANJE".
        chWorkSheet:Range("AI2"):Value = "DEPARTAMENTO".
        chWorkSheet:Range("AJ2"):Value = "PROVINCIA".
        chWorkSheet:Range("AK2"):Value = "DISTRITO".
        chWorkSheet:Range("AL2"):Value = "N� UNICO".
        chWorkSheet:Range("AM2"):Value = "Concepto N/C Otras".
        chWorkSheet:COLUMNS("AM"):NumberFormat = "@".
        chWorkSheet:Range("AN2"):Value = "Descripci�n".
    END.
    WHEN 3 THEN DO:
        chWorkSheet:Range("R2"):Value = "Importe S/.".
        chWorkSheet:Range("S2"):Value = "Importe S/. SIN Adelanto".
        chWorkSheet:Range("T2"):Value = "Saldo S/.".
        chWorkSheet:Range("U2"):Value = "Importe US$".
        chWorkSheet:Range("V2"):Value = "Importe US$ SIN Adelanto".
        chWorkSheet:Range("W2"):Value = "Saldo US$".
        chWorkSheet:Range("X2"):Value = "Estado".
        chWorkSheet:Range("Y2"):Value = "Usuario anulaci�n".
        chWorkSheet:Range("Z2"):Value = "Fecha".
        chWorkSheet:COLUMNS("Z"):NumberFormat = "dd/mm/yyyy".
        chWorkSheet:Range("AA2"):Value = "Hora".
        chWorkSheet:Range("AB2"):Value = "Motivo anulaci�n".
        chWorkSheet:Range("AC2"):Value = "Divisi�n".
        chWorkSheet:COLUMNS("AC"):NumberFormat = "@".
        chWorkSheet:Range("AD2"):Value = "Fecha Reporte".
        chWorkSheet:COLUMNS("AD"):NumberFormat = "dd/mm/yyyy".
        chWorkSheet:Range("AE2"):Value = "Ubicaci�n".
        chWorkSheet:Range("AF2"):Value = "Banco".
        chWorkSheet:Range("AG2"):Value = "Situaci�n".
        chWorkSheet:Range("AH2"):Value = "F.Entrega(DIST)".
        chWorkSheet:Range("AI2"):Value = "F.Entrega(CRED)".
        chWorkSheet:Range("AJ2"):Value = "DIRECC. CLIENTE".
        chWorkSheet:Range("AK2"):Value = "CANJE".
        chWorkSheet:Range("AL2"):Value = "DEPARTAMENTO".
        chWorkSheet:Range("AM2"):Value = "PROVINCIA".
        chWorkSheet:Range("AN2"):Value = "DISTRITO".
        chWorkSheet:Range("AO2"):Value = "N� UNICO".
        chWorkSheet:Range("AP2"):Value = "Papel Fotocopia (S/)".
        chWorkSheet:Range("AQ2"):Value = "Papel Fotocopia ($)".
        chWorkSheet:Range("AR2"):Value = "Concepto N/C Otras".
        chWorkSheet:COLUMNS("AR"):NumberFormat = "@".
        chWorkSheet:Range("AS2"):Value = "Descripci�n".
    END.
END.

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

  EMPTY TEMP-TABLE t-ccbcdocu.
       
  GET FIRST {&BROWSE-NAME}.
  REPEAT WHILE AVAILABLE COMPROBANTES:
    CREATE t-ccbcdocu.
    BUFFER-COPY COMPROBANTES TO t-ccbcdocu.
    GET NEXT {&BROWSE-NAME}.
  END.
  
   CASE s-salida-impresion:
       WHEN 1 THEN OUTPUT TO VALUE(s-print-file) PAGED PAGE-SIZE 62. /* Pantalla */
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
/*     FIND FacDocum OF t-CcbCDocu NO-LOCK NO-ERROR.           */
/*     IF AVAILABLE FacDocum                                   */
/*     THEN x-Factor = IF FacDocum.TpoDoc = NO THEN -1 ELSE 1. */

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
   
  
  EMPTY TEMP-TABLE t-ccbcdocu.
       
  GET FIRST {&BROWSE-NAME}.
  REPEAT WHILE AVAILABLE COMPROBANTES:
    CREATE t-ccbcdocu.
    BUFFER-COPY COMPROBANTES TO t-ccbcdocu.
    GET NEXT {&BROWSE-NAME}.
  END.
  FOR EACH t-ccbcdocu where t-ccbcdocu.codmon = 1 BREAK BY t-ccbcdocu.codcia BY t-ccbcdocu.fchdoc:
    x-Factor = 1.
/*     FIND FacDocum OF t-CcbCDocu NO-LOCK NO-ERROR.           */
/*     IF AVAILABLE FacDocum                                   */
/*     THEN x-Factor = IF FacDocum.TpoDoc = NO THEN -1 ELSE 1. */
    IF t-ccbcdocu.flgest <> 'A'     /* NO ANULADOS */
    THEN DO:
        IF t-ccbcdocu.CodMon = 1 THEN
            ASSIGN
                X_TOT_IS = X_TOT_IS + t-ccbcdocu.ImpTot * x-Factor
                X_TOT_SS = X_TOT_SS + t-ccbcdocu.Sdoact * x-Factor.
    END.
    IF t-CcbcDocu.CodMon = 1 THEN
        ASSIGN
            X-MON = "S/.".
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
   
EMPTY TEMP-TABLE t-ccbcdocu.  
       
  GET FIRST {&BROWSE-NAME}.
  REPEAT WHILE AVAILABLE COMPROBANTES:
    CREATE t-ccbcdocu.
    BUFFER-COPY COMPROBANTES TO t-ccbcdocu.
    GET NEXT {&BROWSE-NAME}.
  END.
  FOR EACH t-ccbcdocu where t-ccbcdocu.codmon = 2 BREAK BY t-ccbcdocu.codcia BY t-ccbcdocu.fchdoc:
    x-Factor = 1.
/*     FIND FacDocum OF t-CcbCDocu NO-LOCK NO-ERROR.           */
/*     IF AVAILABLE FacDocum                                   */
/*     THEN x-Factor = IF FacDocum.TpoDoc = NO THEN -1 ELSE 1. */
    IF t-ccbcdocu.flgest <> 'A'     /* NO ANULADOS */
    THEN DO:
        IF t-ccbcdocu.CodMon = 2 THEN
            ASSIGN
                X_TOT_ID = X_TOT_ID + t-ccbcdocu.ImpTot * x-Factor
                X_TOT_SD = X_TOT_SD + t-ccbcdocu.Sdoact * x-Factor.
    END.
    IF t-CcbcDocu.CodMon = 2 THEN
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
    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH FacDocum NO-LOCK WHERE FacDocum.CodCia = s-codcia AND FacDocum.TpoDoc <> ?:
            c-CmpBte:ADD-LAST(FacDocum.NomDoc, FacDocum.CodDoc).
        END.
        IF pCodDiv <> 'Todos' THEN DO:
            x-CodDiv:SCREEN-VALUE = pCodDiv.
            BUTTON-Division:SENSITIVE = NO.
        END.
    END.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE texto L-table-Win 
PROCEDURE texto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pto AS LOGICAL NO-UNDO.

  EMPTY TEMP-TABLE t-ccbcdocu.
  EMPTY TEMP-TABLE ttCanjes.

GET FIRST {&browse-name}.
  REPEAT WHILE AVAILABLE(COMPROBANTES):
      CREATE t-ccbcdocu.
      BUFFER-COPY COMPROBANTES TO t-ccbcdocu.
      GET NEXT {&browse-name}.
  END.
  FIND FIRST t-ccbcdocu NO-LOCK NO-ERROR.
  IF NOT AVAILABLE t-ccbcdocu THEN RETURN ERROR.

  DEFINE VAR x-rpta AS LOG.                 

    x-Archivo = 'CmptesPtoOrigen.txt'.
    SYSTEM-DIALOG GET-FILE x-Archivo
      FILTERS 'Texto' '*.txt'
      ASK-OVERWRITE
      CREATE-TEST-FILE
      DEFAULT-EXTENSION '.txt'
      INITIAL-DIR 'c:\tmp'
      RETURN-TO-START-DIR 
      USE-FILENAME
      SAVE-AS
      UPDATE x-rpta.
    IF x-rpta = NO THEN RETURN.

DEFINE VAR lde_ImpFoto AS DEC INIT 0.
DEFINE VAR lde_Imptotal AS DEC NO-UNDO.
DEFINE VAR x-factor AS DEC INIT 0.
DEFINE VAR x-ImpTotal AS DEC NO-UNDO.
DEFINE VAR x-SdoAct AS DEC NO-UNDO.
DEFINE VAR x-linea AS INT.

DEFINE VAR cOutputFileTmp AS CHAR.
DEFINE VAR x-separador AS CHAR INIT "|".
DEFINE VAR x-line-data AS CHAR.

/*OUTPUT STREAM REPORT TO VALUE(x-Archivo).*/

SESSION:SET-WAIT-STATE("GENERAL").

DEFINE VAR x-tmp AS CHAR.

x-tmp = STRING(NOW,"99/99/9999 HH:MM:SS").
x-tmp = REPLACE(x-tmp,"/","").
x-tmp = REPLACE(x-tmp,":","").
x-tmp = REPLACE(x-tmp," ","-").

cOutputFileTmp = SESSION:TEMP-DIRECTORY + x-tmp + ".txt".

output to value(x-Archivo).

RUN texto-Cabecera.

/* Documento de la Hoja de Ruta */
DEFINE VAR lCodRef AS CHAR.
DEFINE VAR lNroRef AS CHAR.
DEFINE VAR lFchEntrega AS DATE.
DEF VAR x-Canje AS CHAR NO-UNDO.
DEF VAR x-CodCta AS CHAR NO-UNDO.
DEF VAR x-NomCta AS CHAR NO-UNDO.

DEFINE VAR x-msg AS LOG INIT NO.

FOR EACH t-ccbcdocu, FIRST ccbcdocu OF t-ccbcdocu NO-LOCK:

    EMPTY TEMP-TABLE tt-reg-tempo.
    CREATE tt-reg-tempo.

    ASSIGN x-codcta = '' x-nomcta = ''.
    IF Ccbcdocu.CodDoc = "N/C" AND Ccbcdocu.CndCre = "N" THEN DO:
        FIND FIRST CcbTabla WHERE CcbTabla.CodCia = s-CodCia
            AND CcbTabla.Tabla = Ccbcdocu.CodDoc
            AND CcbTabla.Codigo = Ccbcdocu.CodCta
            NO-LOCK NO-ERROR.
        IF AVAILABLE CcbTabla THEN ASSIGN x-CodCta = CCbTabla.Codigo x-NomCta = CcbTabla.Nombre.
    END.
    /* Para buscarlo en la hoja de RUTA */
    lCodRef = t-ccbcdocu.coddoc.
    lNroRef = t-ccbcdocu.nrodoc.
    lFchEntrega = ?.
    IF t-ccbcdocu.codref = 'G/R' THEN DO:
        lCodRef = t-ccbcdocu.codref.
        lNroRef = t-ccbcdocu.nroref.
    END.
    /* Buscar el Documento en la HR */
    FOR EACH di-RutaD USE-INDEX llave02 NO-LOCK WHERE di-RutaD.codcia = s-codcia AND 
        di-RutaD.coddoc = 'H/R' AND 
        di-RutaD.codref = lCodRef AND 
        di-RutaD.NroRef = lNroRef AND
        di-RutaD.flgest <> 'A', 
        FIRST di-rutaC OF di-rutaD NO-LOCK WHERE di-RutaC.flgest <> 'A':
        IF lFchEntrega = ? THEN lFchEntrega = di-RutaC.fchsal.
        IF lFchEntrega < di-RutaC.fchsal THEN lFchEntrega = di-RutaC.fchsal.
    END.
    t-column = t-column + 1.
    cColumn = STRING(t-Column).
    ASSIGN tt-reg-tempo.A = t-ccbcdocu.coddoc
            tt-reg-tempo.B = t-ccbcdocu.nrodoc
            tt-reg-tempo.C = t-ccbcdocu.codref
            tt-reg-tempo.D = t-ccbcdocu.nroref
            tt-reg-tempo.E = t-ccbcdocu.nroord
            tt-reg-tempo.F = t-ccbcdocu.codcli
            tt-reg-tempo.G = t-ccbcdocu.nomcli
            tt-reg-tempo.H = t-ccbcdocu.ruccli
            tt-reg-tempo.I = t-ccbcdocu.fmapgo
            tt-reg-tempo.J = t-ccbcdocu.codven
            .
    FIND gn-ven WHERE gn-ven.CodCia = s-codcia
        AND gn-ven.CodVen = t-ccbcdocu.codven NO-LOCK NO-ERROR.
    IF AVAILABLE gn-ven THEN DO:
        ASSIGN tt-reg-tempo.K = trim(gn-ven.NomVen).
    END.
    ASSIGN tt-reg-tempo.L = IF (t-ccbcdocu.fchdoc = ?) THEN "" ELSE STRING(t-ccbcdocu.fchdoc,"99/99/9999").
            ASSIGN tt-reg-tempo.M = IF (t-ccbcdocu.fchvto = ?) THEN "" ELSE STRING(t-ccbcdocu.fchvto,"99/99/9999").
            ASSIGN tt-reg-tempo.N = IF (t-ccbcdocu.fchcbd = ?) THEN "" ELSE STRING(t-ccbcdocu.fchcbd,"99/99/9999").
            ASSIGN tt-reg-tempo.O = IF (t-ccbcdocu.fchcan = ?) THEN "" ELSE STRING(t-ccbcdocu.fchcan,"99/99/9999").

    x-ImpTot = t-ccbcdocu.imptot + fAdelanto(t-ccbcdocu.codcia, t-ccbcdocu.coddoc, t-ccbcdocu.nrodoc).
    x-SdoAct = t-ccbcdocu.sdoact.

    FIND Ccbaudit OF t-ccbcdocu NO-LOCK NO-ERROR.
    IF AVAILABLE Ccbaudit THEN FIND ccbtabla WHERE ccbtabla.codcia = t-ccbcdocu.codcia
        AND ccbtabla.tabla = 'MA' AND ccbtabla.codigo = ccbaudit.codref NO-LOCK NO-ERROR.
    CASE xcodmod:
        WHEN 1 OR WHEN 2 THEN DO:
            ASSIGN tt-reg-tempo.P = t-ccbcdocu.imptot
                    tt-reg-tempo.Q = x-ImpTot
                    tt-reg-tempo.R = x-SdoAct
                    tt-reg-tempo.S = fEstado().
            IF AVAILABLE ccbtabla AND AVAILABLE ccbaudit THEN DO:
                ASSIGN tt-reg-tempo.T = ccbaudit.usuario
                        tt-reg-tempo.U = IF (ccbaudit.fecha = ?) THEN "" ELSE STRING(ccbaudit.fecha,"99/99/9999")
                        tt-reg-tempo.V = ccbaudit.hora
                        tt-reg-tempo.W = ccbtabla.nombre.
            END.
            ASSIGN tt-reg-tempo.X = t-ccbcdocu.DivOri
                    tt-reg-tempo.Y = STRING(TODAY,"99/99/9999")
                    tt-reg-tempo.Z = fUbicacion()
                    tt-reg-tempo.AA = fBanco()
                    tt-reg-tempo.AB = fSituacion()
                    tt-reg-tempo.AC = IF (lFchEntrega = ?) THEN "" ELSE STRING(lFchEntrega,"99/99/9999")
                    tt-reg-tempo.AD = IF (t-ccbcdocu.fchcbd = ?) THEN "" ELSE STRING(t-ccbcdocu.fchcbd,"99/99/9999")
                    tt-reg-tempo.AE = t-ccbcdocu.dircli
                    tt-reg-tempo.AK = x-CodCta
                    tt-reg-tempo.AL = trim(x-NomCta).
        END.
        WHEN 3 THEN DO:
            IF t-ccbcdocu.codmon = 1 THEN DO:
                ASSIGN tt-reg-tempo.P = t-ccbcdocu.imptot
                        tt-reg-tempo.Q = x-ImpTot
                        tt-reg-tempo.R = x-sdoact.
            END.
            IF t-ccbcdocu.codmon = 2 THEN DO:
                    /*STRING(t-ccbcdocu.imptot,'>>,>>>,>>9.99')*/
                ASSIGN tt-reg-tempo.S = STRING(t-ccbcdocu.imptot,'>>>>>>>9.99')
                        tt-reg-tempo.T = STRING(x-ImpTot,'>>>>>>>9.99')
                        tt-reg-tempo.U = STRING(x-sdoact,'>>>>>>>9.99').
            END.
            ASSIGN tt-reg-tempo.V = fEstado().
            IF AVAILABLE ccbtabla AND AVAILABLE ccbaudit THEN DO:
                ASSIGN tt-reg-tempo.W = ccbaudit.usuario
                        tt-reg-tempo.X = IF (ccbaudit.fecha = ?) THEN "" ELSE STRING(ccbaudit.fecha,"99/99/9999")
                        tt-reg-tempo.Y = ccbaudit.hora
                        tt-reg-tempo.Z = ccbtabla.nombre. 
            END.
            ASSIGN tt-reg-tempo.AA = t-ccbcdocu.DivOri
                    tt-reg-tempo.AB = STRING(TODAY,"99/99/9999")
                    tt-reg-tempo.AC = fUbicacion()
                    tt-reg-tempo.AD = fBanco()
                    tt-reg-tempo.AE = fSituacion()
                    tt-reg-tempo.AF = IF (lFchEntrega = ?) THEN "" ELSE STRING(lFchEntrega,"99/99/9999")
                    tt-reg-tempo.AG = IF (t-ccbcdocu.fchcbd = ?) THEN "" ELSE STRING(t-ccbcdocu.fchcbd,"99/99/9999")
                    tt-reg-tempo.AH = t-ccbcdocu.dircli
                    tt-reg-tempo.AP = x-CodCta
                    tt-reg-tempo.AQ = x-NomCta.
        END.
    END CASE.
    /* RHC 03/04/17 Verifica si tiene canje pendiente */
    x-Canje = "".
    IF LOOKUP(ccbcdocu.coddoc, 'FAC,BOL') > 0
        AND ccbcdocu.flgest = "P"
        AND ccbcdocu.FlgSit = 'X' THEN DO:
        /* Buscamos canje */
        CANJE:
        FOR EACH ccbcmvto NO-LOCK WHERE CcbCMvto.CodCia = ccbcdocu.codcia
            AND CcbCMvto.CodCli = ccbcdocu.codcli
            AND CcbCMvto.CodDoc = "CJE"
            AND CcbCMvto.FlgEst = "P"
            AND CcbCMvto.FchDoc >= ccbcdocu.fchdoc:
            FOR EACH Ccbdmvto NO-LOCK WHERE CcbDMvto.CodCia = Ccbcmvto.codcia
                AND CcbDMvto.CodDoc = Ccbcmvto.coddoc
                AND CcbDMvto.NroDoc = Ccbcmvto.nrodoc
                AND CcbDMvto.TpoRef = "O"
                AND CcbDMvto.CodRef = Ccbcdocu.coddoc
                AND CcbDMvto.NroRef = Ccbcdocu.nrodoc:
                x-Canje = "CANJE DE LETRA".
                LEAVE CANJE.
            END.
        END.
    END.
    IF x-Canje > '' THEN DO:
        CASE xcodmod:
            WHEN 1 OR WHEN 2 THEN DO:
                ASSIGN tt-reg-tempo.AF = x-Canje.
            END.
            WHEN 3 THEN DO:
                ASSIGN tt-reg-tempo.AI = x-Canje.
            END.
        END CASE.
    END.
    /* RHC 06/06/2017 Datos adicionales Julissa Calderon */
    DEF VAR pCodDepto AS CHAR.
    DEF VAR pNomDepto AS CHAR.
    DEF VAR pCodProvi AS CHAR.
    DEF VAR pNomProvi AS CHAR.
    DEF VAR pCodDistr AS CHAR.
    DEF VAR pNomDistr AS CHAR.
    FIND gn-clie WHERE gn-clie.codcia = cl-codcia AND gn-clie.codcli = t-ccbcdocu.codcli
        NO-LOCK NO-ERROR.
    IF AVAILABLE gn-clie THEN DO:
        RUN gn/ubigeo-cliente (
            INPUT gn-clie.CodCia ,
            INPUT gn-clie.CodCli ,
            OUTPUT pCodDepto, 
            OUTPUT pNomDepto, 
            OUTPUT pCodProvi, 
            OUTPUT pNomProvi, 
            OUTPUT pCodDistr, 
            OUTPUT pNomDistr 
            ).
        CASE xcodmod:
            WHEN 1 OR WHEN 2 THEN DO:
                ASSIGN tt-reg-tempo.AG = pNomDepto
                        tt-reg-tempo.AH = pNomProvi
                        tt-reg-tempo.AI = pNomDistr
                        tt-reg-tempo.AJ = (IF t-ccbcdocu.coddoc = 'LET' THEN t-ccbcdocu.nrosal ELSE '').
            END.
            WHEN 3 THEN DO:
                ASSIGN tt-reg-tempo.AJ = pNomDepto
                        tt-reg-tempo.AK = pNomProvi
                        tt-reg-tempo.AL = pNomDistr
                        tt-reg-tempo.AM = (IF t-ccbcdocu.coddoc = 'LET' THEN t-ccbcdocu.nrosal ELSE '').
            END.
        END CASE.
    END.
    /* Ic - 30Oct2018, pedido de Julissa Calderon*/
    lde_ImpFoto = 0.
    lde_ImpTotal = 0.
    x-factor = 0.
    IF t-ccbcdocu.sdoact > 0 THEN DO:
        /* % Saldo pendiente */
        IF t-ccbcdocu.coddoc = 'LET' AND t-ccbcdocu.codref = 'CJE' THEN DO:
            x-factor = importe-fotocopia(INPUT t-ccbcdocu.coddoc, INPUT t-ccbcdocu.nrodoc, 
                                            INPUT t-ccbcdocu.codref, INPUT t-ccbcdocu.nroref).
        END.
        ELSE DO:
            FOR EACH ccbddocu OF t-ccbcdocu NO-LOCK, FIRST Almmmatg NO-LOCK WHERE Almmmatg.codcia = Ccbddocu.codcia
                AND Almmmatg.codmat = Ccbddocu.codmat :

                IF Almmmatg.codfam = '011' THEN lde_ImpFoto = lde_ImpFoto + Ccbddocu.ImpLin.
                lde_ImpTotal = lde_ImpTotal + Ccbddocu.ImpLin.
            END.
            x-factor = ROUND (lde_ImpFoto / lde_ImpTotal,4).
        END.
        lde_ImpFoto = t-ccbcdocu.sdoact * x-factor.
    END.
    /* Es papel fotocopia */
    IF lde_ImpFoto > 0 THEN DO:        
        IF t-ccbcdocu.codmon = 2 THEN DO:
            ASSIGN tt-reg-tempo.AO = lde_ImpFoto.
        END.
        ELSE DO:
            ASSIGN tt-reg-tempo.AN = lde_ImpFoto.
        END.
    END.

    /**/
        x-linea = x-linea + 1.
        x-line-data = TRIM(tt-reg-tempo.A) + x-separador +
        trim(tt-reg-tempo.B) + x-separador + 
        trim(tt-reg-tempo.C) + x-separador +
        trim(tt-reg-tempo.D) +  x-separador + 
        trim(tt-reg-tempo.E) +  x-separador + 
        trim(tt-reg-tempo.F) +  x-separador + 
        trim(tt-reg-tempo.G) +  x-separador + 
        trim(tt-reg-tempo.H) +  x-separador + 
        trim(tt-reg-tempo.I) +  x-separador + 
        trim(tt-reg-tempo.J) +  x-separador + 
        trim(tt-reg-tempo.K) +  x-separador + 
        trim(tt-reg-tempo.L) +  x-separador + 
        trim(tt-reg-tempo.M) +  x-separador + 
        trim(tt-reg-tempo.N) +  x-separador + 
        trim(tt-reg-tempo.O) +  x-separador.
        x-line-data = x-line-data +
            STRING(tt-reg-tempo.P,">>,>>>,>>9.99") +  x-separador + 
        STRING(tt-reg-tempo.Q,">>,>>>,>>9.99") +  x-separador + 
        STRING(tt-reg-tempo.R,">>,>>>,>>9.99") +  x-separador + 
        trim(tt-reg-tempo.S) +  x-separador + 
        trim(tt-reg-tempo.T) +  x-separador + 
        trim(tt-reg-tempo.U) +  x-separador + 
        trim(tt-reg-tempo.V) +  x-separador + 
        trim(tt-reg-tempo.W) +  x-separador + 
        trim(tt-reg-tempo.X) +  x-separador + 
        trim(tt-reg-tempo.Y) +  x-separador + 
        trim(tt-reg-tempo.Z) +  x-separador + 
        trim(tt-reg-tempo.AA) +  x-separador + 
        trim(tt-reg-tempo.AB) +  x-separador + 
        trim(tt-reg-tempo.AC) +  x-separador + 
        trim(tt-reg-tempo.AD) +  x-separador + 
        trim(tt-reg-tempo.AE) +  x-separador + 
        trim(tt-reg-tempo.AF) +  x-separador + 
        trim(tt-reg-tempo.AG) +  x-separador + 
        trim(tt-reg-tempo.AH) +  x-separador + 
        trim(tt-reg-tempo.AI) +  x-separador + 
        trim(tt-reg-tempo.AJ) +  x-separador + 
        trim(tt-reg-tempo.AK) +  x-separador
        .

    CASE xcodmod:
        WHEN 1 THEN DO:
            x-line-data = x-line-data +
                TRIM(tt-reg-tempo.AL).
        END.
        WHEN 2 THEN DO:
            x-line-data = x-line-data + 
                TRIM(tt-reg-tempo.AL).
        END.
        WHEN 3 THEN DO:
            x-line-data = x-line-data + 
                TRIM(tt-reg-tempo.AL) + x-separador + 
                TRIM(tt-reg-tempo.AM) + x-separador +
                STRING(tt-reg-tempo.AN,">>,>>>,>>9.99") + x-separador + 
                STRING(tt-reg-tempo.AO,">>,>>>,>>9.99") + x-separador +
                TRIM(tt-reg-tempo.AP) + x-separador +
                TRIM(tt-reg-tempo.AQ).
        END.
    END.
    /*x-line-data = REPLACE(x-line-data,"?"," ").*/
    x-line-data =  x-line-data + {&lf}.
    /*PUT UNFORMATTED string(x-linea) + {&lf}.*/
    PUT UNFORMATTED x-line-data.
END.

HIDE FRAME F-Proceso.

OUTPUT CLOSE. 

SESSION:SET-WAIT-STATE("").

DEF VAR cComando AS CHAR NO-UNDO.

DEFINE VAR cOutputClientFile AS CHAR.

cOutputClientFile = x-Archivo.

cComando = "copy " + TRIM(cOutputFileTmp) + ' ' + TRIM(cOutputClientFile).
OS-COMMAND 
    SILENT 
    VALUE(cComando).
OS-DELETE VALUE(cOutputFileTmp).


MESSAGE 'Proceso Terminado'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

RUN dispatch IN THIS-PROCEDURE ('open-query':U).



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Texto-Cabecera L-table-Win 
PROCEDURE Texto-Cabecera :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VAR x-line-data AS CHAR.

x-line-data = "Doc|" + 
    "Numero|" +
    "Ref.|" +
    "Numero Ref|" + 
    "Pedido Cliente|" +
    "Codigo|" + 
    "Cliente|" + 
    "RUC|" + 
    "Cond. Vta.|" +
    "Vendedor|" +
    "Nombre del Vendedor|" +
    "Emision|" +
    "Vencimiento|" +
    "Recepcion|" +
    "Cancelacion|".


CASE xcodmod:
    WHEN 1 THEN DO:

        x-line-data = x-line-data +
        "Importe S/.|" + 
        "Importe S/. SIN Adelanto|" + 
        "Saldo S/.|" + 
        "Estado|" + 
        "Usuario anulacion|" + 
        "Fecha|" + 
        "Hora|" +
        "Motivo anulacion|" +
        "Divisi�n|" + 
        "Fecha Reporte|" +
        "Ubicacion|" + 
        "Banco|" + 
        "Situacion|" +
        "F.Entrega(DIST)|" + 
        "F.Entrega(CRED)|" + 
        "DIRECC. CLIENTE|" + 
        "CANJE|" + 
        "DEPARTAMENTO|" + 
        "PROVINCIA|" + 
        "DISTRITO|" + 
        "No UNICO|" + 
        "Concepto N/C Otras|" +
        "Descripcion".
    END.
    WHEN 2 THEN DO:
        x-line-data = x-line-data + 
        "Importe US$|" + 
        "Importe US$ SIN Adelanto|" + 
        "Saldo US$|" + 
        "Estado|" + 
        "Usuario anulacion|" + 
        "Fecha|" + 
        "Hora|" + 
        "Motivo anulacion|" + 
        "Divisi�n|" + 
        "Fecha Reporte|" + 
        "Ubicacion|" + 
        "Banco|" + 
        "Situacion|" + 
        "F.Entrega(DIST)|" + 
        "F.Entrega(CRED)|" + 
        "DIRECC. CLIENTE|" + 
        "CANJE|" + 
        "DEPARTAMENTO|" + 
        "PROVINCIA|" + 
        "DISTRITO|" + 
        "No UNICO|" + 
        "Concepto N/C Otras|" + 
        "Descripcion".
    END.
    WHEN 3 THEN DO:
        x-line-data = x-line-data + 
        "Importe S/.|" + 
        "Importe S/. SIN Adelanto|" + 
        "Saldo S/.|" + 
        "Importe US$|" + 
        "Importe US$ SIN Adelanto|" + 
        "Saldo US$|" + 
        "Estado|" + 
        "Usuario anulacion|" + 
        "Fecha|" + 
        "Hora|" + 
        "Motivo anulacion|" + 
        "Division|" + 
        "Fecha Reporte|" + 
        "Ubicacion|" + 
        "Banco|" + 
        "Situacion|" + 
        "F.Entrega(DIST)|" + 
        "F.Entrega(CRED)|" + 
        "DIRECC. CLIENTE|" + 
        "CANJE|" + 
        "DEPARTAMENTO|" + 
        "PROVINCIA|" + 
        "DISTRITO|" + 
        "No UNICO|" + 
        "Papel Fotocopia (S/)|" + 
        "Papel Fotocopia ($)|" + 
        "Concepto N/C Otras|" + 
        "Descripcion".
    END.
END.

PUT UNFORMATTED x-line-data {&lf}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-get-referencia L-table-Win 
FUNCTION f-get-referencia RETURNS CHARACTER
  ( INPUT pCodDiv AS CHAR, INPUT pCodDoc AS CHAR, INPUT pNroDoc AS CHAR ) :

  DEFINE BUFFER b-ccbcdocu FOR ccbcdocu.
  DEFINE VAR x-nro-gr AS CHAR.

  x-nro-gr = "|". 

  FIND FIRST b-ccbcdocu WHERE b-ccbcdocu.codcia = s-codcia AND       
                                b-ccbcdocu.coddiv = pCodDiv AND 
                                b-ccbcdocu.coddoc = pCodDoc AND
                                b-ccbcdocu.nrodoc = pNroDoc NO-LOCK NO-ERROR.
  IF AVAILABLE b-ccbcdocu THEN DO:
      xLoop:
      FOR EACH ccbcdocu WHERE ccbcdocu.codcia = s-codcia AND 
                              ccbcdocu.coddoc = 'G/R' AND 
                              ccbcdocu.codref = pCodDoc AND
                              ccbcdocu.nroref = pNroDoc AND
                              ccbcdocu.coddiv = pCodDiv NO-LOCK:
          x-nro-gr = TRIM(ccbcdocu.coddoc) + "|" + TRIM(ccbcdocu.nrodoc).
          LEAVE xLoop.
      END.
/*       IF TRUE <> (b-ccbcdocu.nroref > "") THEN DO:                               */
/*             xLoop:                                                               */
/*             FOR EACH ccbcdocu WHERE ccbcdocu.codcia = s-codcia AND               */
/*                                     ccbcdocu.coddoc = 'G/R' AND                  */
/*                                     ccbcdocu.codref = pCodDoc AND                */
/*                                     ccbcdocu.nroref = pNroDoc AND                */
/*                                     ccbcdocu.coddiv = pCodDiv NO-LOCK:           */
/*                 x-nro-gr = TRIM(ccbcdocu.coddoc) + "|" + TRIM(ccbcdocu.nrodoc).  */
/*                 LEAVE xLoop.                                                     */
/*             END.                                                                 */
/*       END.                                                                       */
/*       ELSE DO:                                                                   */
/*              x-nro-gr = TRIM(b-ccbcdocu.codref) + "|" + TRIM(b-ccbcdocu.nroref). */
/*       END.                                                                       */
  END.  
  
  RETURN x-nro-gr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

DEF VAR pEstado AS CHAR.

RUN gn/fFlgEstCCBv2 (ccbcdocu.coddoc,
                     ccbcdocu.flgest,
                     OUTPUT pEstado).
RETURN pEstado.
/*                                             */
/*     IF CcbCDocu.FlgEst = "P" THEN           */
/*         ASSIGN                              */
/*             X-EST = "PEN" .                 */
/*     ELSE                                    */
/*        IF CcbCDocu.FlgEst = "C" THEN        */
/*           ASSIGN                            */
/*               X-EST = "CAN" .               */
/*        ELSE                                 */
/*           IF CcbcDocu.FlgEst = "A" THEN     */
/*              ASSIGN                         */
/*                  X-EST = "ANU" .            */
/*   CASE Ccbcdocu.flgest:                     */
/*     WHEN 'P' THEN RETURN 'PEN'.             */
/*     WHEN 'C' THEN RETURN 'CAN'.             */
/*     WHEN 'A' THEN RETURN 'ANU'.             */
/*     OTHERWISE RETURN '???'.                 */
/*   END CASE.                                 */
/*                                             */
/*   RETURN "".   /* Function return value. */ */
/*                                             */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION importe-fotocopia L-table-Win 
FUNCTION importe-fotocopia RETURNS DECIMAL
  ( INPUT pCodDoc AS CHAR, INPUT pNroDoc AS CHAR, pCodCje AS CHAR, INPUT pNroCje AS CHAR) :

/* pCodDoc = LET */

/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR x-retval AS DEC INIT 0.

DEFINE VAR x-imp-papel AS DEC INIT 0.

DEFINE VAR x-sec AS INT.
DEFINE VAR x-total AS INT.
DEFINE VAR x-suma-cjes AS DEC.
DEFINE VAR x-suma-factor AS DEC.

FIND FIRST ttCanjes WHERE ttCanjes.tcoddoc = pCodCje AND 
                            ttCanjes.tnrodoc = pNroCje NO-ERROR.
IF NOT AVAILABLE ttCanjes THEN DO:
    FOR EACH ccbdcaja WHERE ccbdcaja.codcia = s-codcia AND
                            ccbdcaja.coddoc = pCodCje AND
                            ccbdcaja.nrodoc = pNroCje NO-LOCK:

        FIND FIRST x-ccbcdocu WHERE x-ccbcdocu.codcia = s-codcia AND
                                        x-ccbcdocu.coddoc = ccbdcaja.codref AND
                                        x-ccbcdocu.nrodoc = ccbdcaja.nroref NO-LOCK NO-ERROR.
        IF AVAILABLE x-ccbcdocu THEN DO:
            FOR EACH ccbddocu WHERE ccbddocu.codcia = s-codcia AND 
                                    ccbddocu.coddoc = ccbdcaja.codref AND
                                    ccbddocu.nrodoc = ccbdcaja.nroref NO-LOCK,
                                    FIRST Almmmatg OF ccbddocu NO-LOCK :
    
                IF Almmmatg.codfam = '011' THEN x-imp-papel = x-imp-papel + Ccbddocu.ImpLin.

                x-retval = x-retval + Ccbddocu.ImpLin.
            END.
        END.
    END.

    IF x-retval > 0 OR x-imp-papel > 0 THEN DO:
        CREATE ttCanjes.
            ASSIGN ttCanjes.tcoddoc = pCodCje
                    ttCanjes.tnrodoc = pNroCje
                    ttCanjes.timptot = x-retval
                    ttCanjes.timppapel = x-imp-papel
        .
    END.
END.

FIND FIRST ttCanjes WHERE ttCanjes.tcoddoc = pCodCje AND 
                            ttCanjes.tnrodoc = pNroCje NO-ERROR.
IF AVAILABLE ttCanjes THEN DO:
    x-retval = ROUND(ttCanjes.timppapel / ttCanjes.timptot,4).
END.

RETURN x-retval.

END FUNCTION.

/*
DEFINE TEMP-TABLE ttCanjesDetalle
    FIELD   tcoddoc     AS  CHAR    FORMAT 'x(5)'
    FIELD   tnrodoc     AS  CHAR    FORMAT 'x(15)'
    FIELD   timpcje     AS  DEC format  '->>,>>>,>>9.9999' INIT 0
    FIELD   timppapel   AS  DEC format  '->>,>>>,>>9.9999' INIT 0
    FIELD   tfactorcje     AS  DEC format  '->>,>>>,>>9.9999' INIT 0
    FIELD   tfactorpapel   AS  DEC format  '->>,>>>,>>9.9999' INIT 0
    .

*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

