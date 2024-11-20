&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-w-report NO-UNDO LIKE w-report.



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
DEFINE SHARED VAR s-codcia AS INT.
DEFINE SHARED VAR s-coddiv AS CHAR.
DEFINE SHARED VAR s-user-id AS CHAR.

DEFINE VAR x-cantidad-sku-col AS INT.
DEFINE VAR x-cantidad-bultos-col AS INT.
DEFINE VAR x-tiempo-observado-col AS CHAR.
DEFINE VAR x-nom-chequeador-col AS CHAR.
DEFINE VAR x-cliente-col AS CHAR.

DEFINE VAR x-codped AS CHAR.
DEFINE VAR x-nroped AS CHAR.

DEFINE TEMP-TABLE ttOrdenes
    FIELDS  tcoddoc     AS  CHAR    FORMAT  'x(5)'
    FIELDS  tnrodoc     AS  CHAR    FORMAT  'x(15)'
.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-14

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ChkTareas tt-w-report

/* Definitions for BROWSE BROWSE-14                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-14 ChkTareas.CodDoc ChkTareas.NroPed ~
cliente(chktareas.coddoc, chktareas.nroped) @ x-cliente-col ~
cantidad-sku(chktareas.coddoc, chktareas.nroped) @ x-cantidad-sku-col ~
cantidad-bultos(chktareas.coddoc, chktareas.nroped) @ x-cantidad-bultos-col ~
ChkTareas.Mesa ~
chequeador(chktareas.coddoc, chktareas.nroped) @ x-nom-chequeador-col ~
tiempo-observado(chktareas.fechainicio, chktareas.horainicio) @ x-tiempo-observado-col 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-14 
&Scoped-define QUERY-STRING-BROWSE-14 FOR EACH ChkTareas ~
      WHERE ChkTareas.codcia = s-codcia and ~
ChkTareas.FlgEst = 'E' ~
 AND ChkTareas.CodDiv = s-coddiv NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-14 OPEN QUERY BROWSE-14 FOR EACH ChkTareas ~
      WHERE ChkTareas.codcia = s-codcia and ~
ChkTareas.FlgEst = 'E' ~
 AND ChkTareas.CodDiv = s-coddiv NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-14 ChkTareas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-14 ChkTareas


/* Definitions for BROWSE BROWSE-15                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-15 tt-w-report.Campo-C[1] ~
tt-w-report.Campo-C[2] tt-w-report.Campo-C[3] tt-w-report.Campo-F[1] ~
tt-w-report.Campo-C[4] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-15 
&Scoped-define QUERY-STRING-BROWSE-15 FOR EACH tt-w-report NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-15 OPEN QUERY BROWSE-15 FOR EACH tt-w-report NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-15 tt-w-report
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-15 tt-w-report


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-14}~
    ~{&OPEN-QUERY-BROWSE-15}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-14 BROWSE-15 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cantidad-bultos W-Win 
FUNCTION cantidad-bultos RETURNS INTEGER
  ( INPUT pCodDoc AS CHAR, INPUT pNroDoc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cantidad-sku W-Win 
FUNCTION cantidad-sku RETURNS INTEGER
  ( INPUT pCodDoc AS CHAR, INPUT pNrodoc AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD chequeador W-Win 
FUNCTION chequeador RETURNS CHARACTER
  ( INPUT pCodDoc AS CHAR, INPUT pNroPed AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD cliente W-Win 
FUNCTION cliente RETURNS CHARACTER
  ( INPUT pCodDoc AS CHAR, INPUT pNroDOc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD tiempo-observado W-Win 
FUNCTION tiempo-observado RETURNS CHARACTER
  ( INPUT pFecha AS DATE, INPUT pHora AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-14 FOR 
      ChkTareas SCROLLING.

DEFINE QUERY BROWSE-15 FOR 
      tt-w-report SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-14 W-Win _STRUCTURED
  QUERY BROWSE-14 NO-LOCK DISPLAY
      ChkTareas.CodDoc COLUMN-LABEL "Cod." FORMAT "x(3)":U WIDTH 7.43
            COLUMN-FONT 0
      ChkTareas.NroPed COLUMN-LABEL "Nro.Orden" FORMAT "X(12)":U
            WIDTH 12.43 COLUMN-FONT 0
      cliente(chktareas.coddoc, chktareas.nroped) @ x-cliente-col COLUMN-LABEL "Nombre del Cliente" FORMAT "x(50)":U
            WIDTH 35.43 COLUMN-FONT 0
      cantidad-sku(chktareas.coddoc, chktareas.nroped) @ x-cantidad-sku-col COLUMN-LABEL "Cant. SKU" FORMAT ">>,>>9":U
            WIDTH 9 COLUMN-FONT 0
      cantidad-bultos(chktareas.coddoc, chktareas.nroped) @ x-cantidad-bultos-col COLUMN-LABEL "No. Bultos"
      ChkTareas.Mesa FORMAT "x(8)":U WIDTH 11.72 COLUMN-FONT 0
      chequeador(chktareas.coddoc, chktareas.nroped) @ x-nom-chequeador-col COLUMN-LABEL "Chequeador" FORMAT "x(40)":U
            WIDTH 20 COLUMN-FONT 0
      tiempo-observado(chktareas.fechainicio, chktareas.horainicio) @ x-tiempo-observado-col COLUMN-LABEL "Hora Terminado" FORMAT "x(25)":U
            WIDTH 17 COLUMN-FONT 0
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 129 BY 11.54
         FONT 4
         TITLE "Ordenes por Embalar" FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-15 W-Win _STRUCTURED
  QUERY BROWSE-15 NO-LOCK DISPLAY
      tt-w-report.Campo-C[1] COLUMN-LABEL "Codigo" FORMAT "X(8)":U
            COLUMN-FONT 0
      tt-w-report.Campo-C[2] COLUMN-LABEL "Descripcion Articulo" FORMAT "X(50)":U
            WIDTH 39.72 COLUMN-FONT 0
      tt-w-report.Campo-C[3] COLUMN-LABEL "Marca" FORMAT "X(30)":U
            WIDTH 18.43 COLUMN-FONT 0
      tt-w-report.Campo-F[1] COLUMN-LABEL "Cantidad" FORMAT "->>>,>>9.99":U
            WIDTH 10.72 COLUMN-FONT 0
      tt-w-report.Campo-C[4] COLUMN-LABEL "Unidad" FORMAT "X(8)":U
            WIDTH 7.57
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 10.58
         FONT 4
         TITLE "Detalle" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BROWSE-14 AT ROW 1.19 COL 2 WIDGET-ID 200
     BROWSE-15 AT ROW 12.92 COL 2 WIDGET-ID 300
     "DoubleClick en Orden - Cerrar EMBALADO" VIEW-AS TEXT
          SIZE 40.43 BY .96 AT ROW 13.12 COL 94 WIDGET-ID 4
          FGCOLOR 9 FONT 11
     "Considera documentos HPK" VIEW-AS TEXT
          SIZE 35 BY .96 AT ROW 14.27 COL 95 WIDGET-ID 6
          BGCOLOR 15 FGCOLOR 4 FONT 9
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 134.72 BY 22.62 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-w-report T "?" NO-UNDO INTEGRAL w-report
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Gestion de Ordenes Embaladas"
         HEIGHT             = 22.65
         WIDTH              = 134.72
         MAX-HEIGHT         = 24.12
         MAX-WIDTH          = 173.86
         VIRTUAL-HEIGHT     = 24.12
         VIRTUAL-WIDTH      = 173.86
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

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-14 1 F-Main */
/* BROWSE-TAB BROWSE-15 BROWSE-14 F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-14
/* Query rebuild information for BROWSE BROWSE-14
     _TblList          = "INTEGRAL.ChkTareas"
     _Options          = "NO-LOCK"
     _Where[1]         = "ChkTareas.codcia = s-codcia and
ChkTareas.FlgEst = 'E'
 AND ChkTareas.CodDiv = s-coddiv"
     _FldNameList[1]   > INTEGRAL.ChkTareas.CodDoc
"ChkTareas.CodDoc" "Cod." ? "character" ? ? 0 ? ? ? no ? no no "7.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > INTEGRAL.ChkTareas.NroPed
"ChkTareas.NroPed" "Nro.Orden" ? "character" ? ? 0 ? ? ? no ? no no "12.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"cliente(chktareas.coddoc, chktareas.nroped) @ x-cliente-col" "Nombre del Cliente" "x(50)" ? ? ? 0 ? ? ? no ? no no "35.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"cantidad-sku(chktareas.coddoc, chktareas.nroped) @ x-cantidad-sku-col" "Cant. SKU" ">>,>>9" ? ? ? 0 ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"cantidad-bultos(chktareas.coddoc, chktareas.nroped) @ x-cantidad-bultos-col" "No. Bultos" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > INTEGRAL.ChkTareas.Mesa
"ChkTareas.Mesa" ? ? "character" ? ? 0 ? ? ? no ? no no "11.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"chequeador(chktareas.coddoc, chktareas.nroped) @ x-nom-chequeador-col" "Chequeador" "x(40)" ? ? ? 0 ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"tiempo-observado(chktareas.fechainicio, chktareas.horainicio) @ x-tiempo-observado-col" "Hora Terminado" "x(25)" ? ? ? 0 ? ? ? no ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-14 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-15
/* Query rebuild information for BROWSE BROWSE-15
     _TblList          = "Temp-Tables.tt-w-report"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tt-w-report.Campo-C[1]
"tt-w-report.Campo-C[1]" "Codigo" ? "character" ? ? 0 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tt-w-report.Campo-C[2]
"tt-w-report.Campo-C[2]" "Descripcion Articulo" "X(50)" "character" ? ? 0 ? ? ? no ? no no "39.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tt-w-report.Campo-C[3]
"tt-w-report.Campo-C[3]" "Marca" "X(30)" "character" ? ? 0 ? ? ? no ? no no "18.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tt-w-report.Campo-F[1]
"tt-w-report.Campo-F[1]" "Cantidad" "->>>,>>9.99" "decimal" ? ? 0 ? ? ? no ? no no "10.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tt-w-report.Campo-C[4]
"tt-w-report.Campo-C[4]" "Unidad" ? "character" ? ? ? ? ? ? no ? no no "7.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-15 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Gestion de Ordenes Embaladas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Gestion de Ordenes Embaladas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-14
&Scoped-define SELF-NAME BROWSE-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-14 W-Win
ON ENTRY OF BROWSE-14 IN FRAME F-Main /* Ordenes por Embalar */
DO:
    x-codped = "".
    x-nroped = "".
    browse-15:TITLE = "".
    IF AVAILABLE chktareas THEN DO:
          x-codped = chktareas.coddoc.
          x-nroped = chktareas.nroped.

          browse-15:TITLE = chktareas.coddoc + " " + x-nroped + " " + cliente(x-codped, x-nroped).
    END.
    RUN carga-detalle.
    {&open-query-browse-15}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-14 W-Win
ON MOUSE-SELECT-DBLCLICK OF BROWSE-14 IN FRAME F-Main /* Ordenes por Embalar */
DO:
  IF AVAILABLE chktareas THEN DO:
        MESSAGE "La Orden " + chktareas.coddoc + " " + chktareas.nroped  + " fue embalada ?" VIEW-AS ALERT-BOX QUESTION
                BUTTONS YES-NO UPDATE rpta AS LOG.
        IF rpta = NO THEN RETURN NO-APPLY.
    
    RUN actualizar-orden.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-14 W-Win
ON VALUE-CHANGED OF BROWSE-14 IN FRAME F-Main /* Ordenes por Embalar */
DO:
  x-codped = "".
  x-nroped = "".

  browse-15:TITLE = "".
 
  IF AVAILABLE chktareas THEN DO:
        x-codped = chktareas.coddoc.
        x-nroped = chktareas.nroped.

        browse-15:TITLE = chktareas.coddoc + " " + x-nroped + " " + cliente(INPUT x-codped, INPUT x-nroped).
  END.
    RUN carga-detalle.
    {&open-query-browse-15}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualizar-orden W-Win 
PROCEDURE actualizar-orden :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR pRollos AS DEC NO-UNDO.
DEF VAR pOk AS LOG NO-UNDO.

/* ************************************************************** */
/* ************************************************************** */
/* ************************************************************** */
RUN logis/d-rollos-film (OUTPUT pRollos, OUTPUT pOk, INPUT chktareas.coddoc, INPUT chktareas.nroped).
IF pOk = NO THEN RETURN.
/* ************************************************************** */
/* ************************************************************** */

DEFINE VAR x-procesoOk AS CHAR NO-UNDO.
DEFINE VAR x-codigo AS CHAR.
DEFINE VAR x-orden AS INT.

x-procesoOk = 'Inicio'.

DEFINE BUFFER b-faccpedi FOR faccpedi.
DEFINE BUFFER b-vtacdocu FOR vtacdocu.
DEFINE BUFFER b-chktareas FOR chktareas.
DEFINE BUFFER b-LogTrkDocs FOR LogTrkDocs.

/**/
RUN recopilar-ordenes(INPUT chktareas.coddoc, INPUT chktareas.nroped).

GRABAR_REGISTROS:
DO TRANSACTION ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
    /* Las Ordenes */
    x-procesoOk = "Actualizar la orden como Cierre de Chequeo (PC)". 
    IF chktareas.coddoc = 'HPK' THEN DO:
        x-procesoOk = "Actualizar la orden como Cierre de Chequeo (PC)". 
        FIND FIRST b-vtacdocu WHERE b-vtacdocu.codcia = s-codcia AND 
            b-vtacdocu.codped = chktareas.coddoc AND
            b-vtacdocu.nroped = chktareas.nroped EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE b-vtacdocu THEN DO:
            x-procesoOk = "Error al actualizar la HPK como Cierre de Chequeo".
            UNDO GRABAR_REGISTROS, LEAVE GRABAR_REGISTROS.
        END.
        ASSIGN 
            b-vtacdocu.flgsit = 'PC'
            b-VtaCDocu.Importe[3] = pRollos.    /* OJO */
        /* ******************************************************************************* */
        /* Actualizamos FlgSit de la O/D OTR */
        /* ******************************************************************************* */
        DEF VAR x-CodRef AS CHAR NO-UNDO.
        DEF VAR x-NroRef AS CHAR NO-UNDO.
        x-codref = b-vtacdocu.codref.     /* O/D OTR */
        x-nroref = b-vtacdocu.nroref.
        /* La O/D, OTR */
        FIND FIRST faccpedi WHERE faccpedi.codcia = s-codcia AND 
            faccpedi.coddoc = x-codref AND
            faccpedi.nroped = x-nroref 
            EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE Faccpedi THEN DO:
            x-procesoOk = "Error al actualizar la " + x-codref + " " +
                x-nroref + " como Cierre de Chequeo".
            UNDO GRABAR_REGISTROS, LEAVE GRABAR_REGISTROS.
        END.
        DEF VAR lOrdenLista AS LOG NO-UNDO.
        lOrdenLista = YES.
        FOR EACH b-vtacdocu NO-LOCK WHERE b-vtacdocu.codcia = s-CodCia
            AND b-vtacdocu.codped = chktareas.coddoc      /* HPK */
            AND b-vtacdocu.codref = x-codref      /* O/D OTR */
            AND b-vtacdocu.nroref = x-nroref:
            IF b-vtacdocu.flgsit <> "PC" THEN DO:
                lOrdenLista = NO.
                LEAVE.
            END.
        END.
        IF lOrdenLista = YES THEN ASSIGN Faccpedi.FlgSit = "PC".    /* Cierre de Chequeo */
    END.
    /* Tarea Observacion */
    x-procesoOk = "Ponemos la tarea como Terminado".
    FIND FIRST b-chktareas WHERE b-chktareas.codcia = chktareas.codcia AND
        b-chktareas.coddiv = chktareas.coddiv AND 
        b-chktareas.coddoc = chktareas.coddoc AND
        b-chktareas.nroped = chktareas.nroped AND
        b-chktareas.mesa = chktareas.mesa EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF NOT AVAILABLE b-chktareas THEN DO:
        x-procesoOk = "Error al actualizar la tarea como Terminado".
        UNDO GRABAR_REGISTROS, LEAVE GRABAR_REGISTROS.
    END.
    ASSIGN b-chktareas.flgest = 'T' 
        b-chktareas.fechafin = TODAY
        b-chktareas.horafin = STRING(TIME,"HH:MM:SS")
        b-chktareas.usuariofin = s-user-id NO-ERROR
    .
    x-codigo = "NOT FOUND-CK_EMC".
    x-orden = -999.
    /* Ic - 16Dic2021 - Busco el codigo del track - Max Ramos el codigo es CK-EMC */
    FIND FIRST tabTrkDocs WHERE tabTrkDocs.codcia = s-codcia AND
                                 tabTrkDocs.clave = "TRCKHPK" AND 
                                 tabTrkDocs.codigo = "CK_EMC" NO-LOCK NO-ERROR.
    IF AVAILABLE tabTrkDocs THEN DO:
        x-codigo = "CK_EMC".
        x-orden = tabTrkDocs.orden.
    END.
    CREATE b-LogTrkDocs.
        ASSIGN b-LogTrkDocs.codcia = s-codcia
                b-LogTrkDocs.coddoc = chktareas.coddoc
                b-LogTrkDocs.nrodoc = chktareas.nroped
                b-LogTrkDocs.clave = "TRCKHPK"
                b-LogTrkDocs.orden = x-orden
                b-LogTrkDocs.codigo = x-codigo
                b-LogTrkDocs.fecha = DATETIME(STRING(TODAY,"99/99/9999") + " " + STRING(TIME,"HH:MM:SS"))
                b-LogTrkDocs.usuario = s-user-id
                b-LogTrkDocs.coddiv = chktareas.coddiv NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
        x-procesoOk = ERROR-STATUS:GET-MESSAGE(1).
        UNDO GRABAR_REGISTROS, LEAVE GRABAR_REGISTROS.
    END.
                                
    x-procesoOk = "OK".

END. /* TRANSACTION block */

RELEASE b-faccpedi.
RELEASE b-vtacdocu.
RELEASE b-chktareas.
RELEASE b-LogTrkDocs NO-ERROR.
IF AVAILABLE Di-RutaC THEN RELEASE Di-RutaC.

IF x-procesoOk = "OK" THEN DO:
    {&open-query-browse-14}
END.
ELSE DO:
    MESSAGE x-procesoOk.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-detalle W-Win 
PROCEDURE carga-detalle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


EMPTY TEMP-TABLE tt-w-report.

IF NOT AVAILABLE ChkTareas THEN RETURN.

/**/
RUN recopilar-ordenes(INPUT chktareas.coddoc, INPUT chktareas.nroped).

IF x-codped = 'HPK' THEN DO:
    DEFINE BUFFER x-vtaddocu FOR vtaddocu.
    FOR EACH x-vtaddocu WHERE x-vtaddocu.codcia = s-codcia AND
                                x-vtaddocu.codped = x-codped AND
                                x-vtaddocu.nroped = x-nroped NO-LOCK:
        FIND FIRST almmmatg OF x-vtaddocu NO-LOCK NO-ERROR.
        CREATE tt-w-report.
            ASSIGN tt-w-report.campo-i[1] = x-vtaddocu.nroitm
                    tt-w-report.campo-c[1] = x-vtaddocu.codmat
                    tt-w-report.campo-c[2] = almmmatg.desmat
                    tt-w-report.campo-c[3] = almmmatg.desmar
                tt-w-report.campo-c[4] = x-vtaddocu.undvta
                    tt-w-report.campo-f[1] = x-vtaddocu.canped
        .
    END.
END.
ELSE DO:
    DEFINE BUFFER x-facdpedi FOR facdpedi.
    FOR EACH x-facdpedi WHERE x-facdpedi.codcia = s-codcia AND
                                x-facdpedi.coddoc = x-codped AND
                                x-facdpedi.nroped = x-nroped NO-LOCK:
        FIND FIRST almmmatg OF x-facdpedi NO-LOCK NO-ERROR.
        CREATE tt-w-report.
            ASSIGN tt-w-report.campo-i[1] = x-facdpedi.nroitm
                    tt-w-report.campo-c[1] = x-facdpedi.codmat
                    tt-w-report.campo-c[2] = almmmatg.desmat
                    tt-w-report.campo-c[3] = almmmatg.desmar
                    tt-w-report.campo-f[1] = x-facdpedi.canped
                tt-w-report.campo-c[4] = x-facdpedi.undvta
        .
    END.

END.

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
  ENABLE BROWSE-14 BROWSE-15 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recopilar-ordenes W-Win 
PROCEDURE recopilar-ordenes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER pCodDoc AS CHAR.
DEFINE INPUT PARAMETER pNroDoc AS CHAR.

DEFINE VAR x-llave AS CHAR.

EMPTY TEMP-TABLE ttOrdenes.

IF pCodDoc = 'HPK' THEN DO:
    DEFINE BUFFER x-vtacdocu FOR vtacdocu.
    FIND FIRST x-vtacdocu WHERE x-vtacdocu.codcia = s-codcia AND
        x-vtacdocu.coddiv = s-coddiv AND
        x-vtacdocu.codped = pCodDOc AND
        x-vtacdocu.nroped = pNroDoc NO-LOCK NO-ERROR.
    IF AVAILABLE x-vtacdocu THEN DO:
        IF x-vtacdocu.codter = 'ACUMULATIVO' THEN DO:
            DEFINE BUFFER x-almddocu FOR almddocu.
            x-llave = pCodDoc + "," + pNroDOc.
            FOR EACH x-almddocu WHERE x-almddocu.codcia = s-codcia AND 
                                        x-almddocu.codllave = x-llave NO-LOCK
                                        BREAK BY coddoc BY nrodoc.
                IF FIRST-OF(x-almddocu.coddoc) OR FIRST-OF(x-almddocu.nrodoc) THEN DO:
                    CREATE ttOrdenes.
                        ASSIGN tcoddoc = x-almddocu.coddoc
                                tnrodoc = x-almddocu.nrodoc.
                END.
            END.
        END.
        ELSE DO:
            CREATE ttOrdenes.
                ASSIGN tcoddoc = x-vtacdocu.codref
                        tnrodoc = x-vtacdocu.nroref.
        END.
    END.
END.
ELSE DO:
    CREATE ttOrdenes.
        ASSIGN tcoddoc = pCodDoc
                tnrodoc = pNroDoc.
END.

END PROCEDURE.

/*
DEFINE TEMP-TABLE ttOrdenes
    FIELDS  tcoddoc     AS  CHAR    FORMAT  'x(5)'
    FIELDS  tnrodoc     AS  CHAR    FORMAT  'x(15)'
.
*/

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
  {src/adm/template/snd-list.i "tt-w-report"}
  {src/adm/template/snd-list.i "ChkTareas"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE verifica-hpr-cerrada W-Win 
PROCEDURE verifica-hpr-cerrada :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pcod-hpr AS CHAR.
DEFINE INPUT PARAMETER pnro-hpr AS CHAR.
DEFINE OUTPUT PARAMETER pCerrado AS CHAR.

pCerrado = "OK".
DEFINE BUFFER x-vtacdocu FOR vtacdocu.

FOR EACH x-vtacdocu WHERE x-vtacdocu.codcia = s-codcia AND 
                            x-vtacdocu.codped = 'HPK' AND
                            x-vtacdocu.codori = pcod-hpr AND
                            x-vtacdocu.nroori = pnro-hpr AND
                            x-vtacdocu.flgest <> "A" NO-LOCK:
    IF x-vtacdocu.flgsit <> 'PC' THEN pCerrado = 'NO'.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cantidad-bultos W-Win 
FUNCTION cantidad-bultos RETURNS INTEGER
  ( INPUT pCodDoc AS CHAR, INPUT pNroDoc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
    DEFINE VAR x-retval AS INT INIT 0.

    FIND Vtacdocu WHERE VtaCDocu.CodCia = s-codcia 
        AND VtaCDocu.CodPed = pCodDoc
        AND VtaCDocu.NroPed = pNroDoc
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Vtacdocu THEN RETURN x-RetVal.
    FOR EACH Ccbcbult WHERE CcbCBult.CodCia = VtaCDocu.CodCia 
        AND CcbCBult.CodDoc = VtaCDocu.CodRef 
        AND CcbCBult.NroDoc = VtaCDocu.NroRef
        AND ENTRY(1,CcbCBult.Chr_05) = pCodDoc
        AND ENTRY(2,CcbCBult.Chr_05) = pNroDoc:
        x-RetVal = x-RetVal + CcbCBult.Bultos.
    END.

/*     DEFINE BUFFER x-ControlOD FOR ControlOD.                                        */
/*     FOR EACH ttOrdenes :                                                            */
/*         FOR EACH x-ControlOD WHERE x-ControlOD.codcia = s-codcia AND                */
/*                                     x-ControlOD.coddoc = ttOrdenes.tCodDoc AND      */
/*                                     x-ControlOD.nrodoc = ttOrdenes.tnroDoc NO-LOCK: */
/*             x-retval = x-retval + 1.                                                */
/*         END.                                                                        */
/*     END.                                                                            */

    RETURN x-retval.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cantidad-sku W-Win 
FUNCTION cantidad-sku RETURNS INTEGER
  ( INPUT pCodDoc AS CHAR, INPUT pNrodoc AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  DEFINE VAR x-retval AS INT INIT 0.

  IF pCodDoc = 'HPK' THEN DO:
      DEFINE BUFFER x-vtaddocu FOR vtaddocu.

      FOR EACH x-vtaddocu WHERE x-vtaddocu.codcia = s-codcia AND
                                    x-vtaddocu.codped = pCodDoc AND
                                    x-vtaddocu.nroped = pNrodoc NO-LOCK:
            x-retval = x-retval + 1.

      END.
  END.
  ELSE DO:
      DEFINE BUFFER x-facdpedi FOR facdpedi.

      FOR EACH x-facdpedi WHERE x-facdpedi.codcia = s-codcia AND
                                    x-facdpedi.coddoc = pCodDoc AND
                                    x-facdpedi.nroped = pNrodoc NO-LOCK:
            x-retval = x-retval + 1.

      END.
  END.

  RETURN x-retval.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION chequeador W-Win 
FUNCTION chequeador RETURNS CHARACTER
  ( INPUT pCodDoc AS CHAR, INPUT pNroPed AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

FIND FIRST ttOrdenes NO-ERROR.
DEFINE VAR x-retval AS CHAR INIT "".

DEFINE BUFFER x-faccpedi FOR faccpedi.    

IF NOT AVAILABLE ChkTareas THEN RETURN x-RetVal.

FIND FIRST _User WHERE _UserId = ChkTareas.UsuarioFin NO-LOCK NO-ERROR.
IF AVAILABLE _User THEN x-retval = _User._User-Name.

/*     FIND FIRST x-faccpedi WHERE x-faccpedi.codcia = s-codcia AND                       */
/*         x-faccpedi.coddoc = ttOrdenes.tcoddoc AND                                      */
/*         x-faccpedi.nroped = ttOrdenes.tnrodoc NO-LOCK NO-ERROR.                        */
/*     IF AVAILABLE x-faccpedi THEN DO:                                                   */
/*         /* buscarlo si existe en la maestra de personal */                             */
/*         FIND FIRST pl-pers WHERE  pl-pers.codper = x-faccpedi.usrchq NO-LOCK NO-ERROR. */
/*         IF  AVAILABLE pl-pers THEN DO:                                                 */
/*             x-retval = pl-pers.patper + " " + pl-pers.matper + " " + pl-pers.nomper.   */
/*         END.                                                                           */
/*                                                                                        */
/*     END.                                                                               */
/* END.                                                                                   */

RETURN x-retval.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION cliente W-Win 
FUNCTION cliente RETURNS CHARACTER
  ( INPUT pCodDoc AS CHAR, INPUT pNroDOc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

DEFINE VAR x-retval AS CHAR.

IF pCodDoc = 'HPK' THEN DO:
    DEFINE BUFFER x-vtacdocu FOR vtacdocu.
    FIND FIRST x-vtacdocu WHERE x-vtacdocu.codcia = s-codcia AND
                                    x-vtacdocu.codped = pCodDOc AND
                                    x-vtacdocu.nroped = pNrodoc NO-LOCK NO-ERROR.
    IF AVAILABL x-vtacdocu THEN x-retval = x-vtacdocu.nomcli.
END.
ELSE DO:
    DEFINE BUFFER x-faccpedi FOR faccpedi.
    
    FIND FIRST x-faccpedi WHERE x-faccpedi.codcia = s-codcia AND
                                x-faccpedi.coddoc = pCoddoc AND
                                x-faccpedi.nroped = pNroDoc NO-LOCK NO-ERROR.
    IF AVAILABL x-faccpedi THEN x-retval = x-faccpedi.nomcli.
END.

RETURN x-retval.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION tiempo-observado W-Win 
FUNCTION tiempo-observado RETURNS CHARACTER
  ( INPUT pFecha AS DATE, INPUT pHora AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VAR x-tiempo AS CHAR.

    x-Tiempo = STRING(pFecha,"99/99/9999") + " " + pHora.
    /*
    RUN lib/_time-passed ( DATETIME(STRING(pFecha,"99/99/9999") + ' ' + pHora),
                             DATETIME(STRING(TODAY,"99/99/9999") + ' ' + STRING(TIME,"HH:MM:SS")), OUTPUT x-Tiempo).
    */
    

    RETURN x-tiempo.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

