&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T-CONTROL NO-UNDO LIKE ChkControl
       FIELD Libre_d01 AS DEC.



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

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-coddiv AS CHAR.
DEF SHARED VAR s-user-id AS CHAR.

DEF VAR x-CodPed AS CHAR NO-UNDO.
DEF VAR x-CodDiv AS CHAR NO-UNDO.

&SCOPED-DEFINE Condicion ( T-CONTROL.CodCia = s-codcia ~
AND T-CONTROL.CodDoc = x-CodPed ~
AND ( TRUE <> (x-CodDiv > '') OR T-CONTROL.CodDiv = x-CodDiv ) )

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-7

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES T-CONTROL FacCPedi

/* Definitions for BROWSE BROWSE-7                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-7 T-CONTROL.CodDoc T-CONTROL.NroPed ~
FacCPedi.FchEnt FacCPedi.NomCli T-CONTROL.NroItems T-CONTROL.Peso ~
T-CONTROL.Cantidad[1] T-CONTROL.Cantidad[2] T-CONTROL.Cantidad[3] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-7 
&Scoped-define QUERY-STRING-BROWSE-7 FOR EACH T-CONTROL ~
      WHERE {&Condicion} NO-LOCK, ~
      EACH FacCPedi WHERE FacCPedi.CodCia = T-CONTROL.CodCia ~
  AND FacCPedi.CodDoc = T-CONTROL.CodDoc ~
  AND FacCPedi.NroPed = T-CONTROL.NroPed NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-7 OPEN QUERY BROWSE-7 FOR EACH T-CONTROL ~
      WHERE {&Condicion} NO-LOCK, ~
      EACH FacCPedi WHERE FacCPedi.CodCia = T-CONTROL.CodCia ~
  AND FacCPedi.CodDoc = T-CONTROL.CodDoc ~
  AND FacCPedi.NroPed = T-CONTROL.NroPed NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-7 T-CONTROL FacCPedi
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-7 T-CONTROL
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-7 FacCPedi


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-7}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 BUTTON-Actualizar BUTTON-Asignar ~
SELECT-CodDoc BROWSE-7 
&Scoped-Define DISPLAYED-OBJECTS SELECT-CodDoc FILL-IN-Cantidad-1 ~
FILL-IN-Cantidad-2 FILL-IN-Cantidad-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Actualizar 
     LABEL "ACTUALIZAR" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-Asignar 
     LABEL "ASIGNAR MESA" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE FILL-IN-Cantidad-1 AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Jaba" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-Cantidad-2 AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Paleta" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-Cantidad-3 AS DECIMAL FORMAT ">>>,>>9.99":U INITIAL 0 
     LABEL "Carrito" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 93 BY 1.35
     BGCOLOR 15 FGCOLOR 0 .

DEFINE VARIABLE SELECT-CodDoc AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Todos","Todos" 
     SIZE 48 BY 23.69
     BGCOLOR 14 FGCOLOR 0 FONT 10 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-7 FOR 
      T-CONTROL, 
      FacCPedi SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-7 W-Win _STRUCTURED
  QUERY BROWSE-7 NO-LOCK DISPLAY
      T-CONTROL.CodDoc FORMAT "x(3)":U
      T-CONTROL.NroPed COLUMN-LABEL "Nro. Orden" FORMAT "X(12)":U
      FacCPedi.FchEnt FORMAT "99/99/9999":U WIDTH 11
      FacCPedi.NomCli COLUMN-LABEL "Nombre de Cliente" FORMAT "x(50)":U
            WIDTH 44.43
      T-CONTROL.NroItems COLUMN-LABEL "Nro de Items" FORMAT ">>>,>>9":U
            WIDTH 8.43
      T-CONTROL.Peso COLUMN-LABEL "Peso" FORMAT ">>>,>>9.9999":U
      T-CONTROL.Cantidad[1] COLUMN-LABEL "Jaba" FORMAT ">>>,>>9":U
      T-CONTROL.Cantidad[2] COLUMN-LABEL "Paleta" FORMAT ">>>,>>9":U
      T-CONTROL.Cantidad[3] COLUMN-LABEL "Carrito" FORMAT ">>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93 BY 22.35
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-Actualizar AT ROW 1 COL 2 WIDGET-ID 6
     BUTTON-Asignar AT ROW 1 COL 18 WIDGET-ID 10
     SELECT-CodDoc AT ROW 2.35 COL 2 NO-LABEL WIDGET-ID 8
     BROWSE-7 AT ROW 2.35 COL 51 WIDGET-ID 200
     FILL-IN-Cantidad-1 AT ROW 24.96 COL 59 COLON-ALIGNED WIDGET-ID 14
     FILL-IN-Cantidad-2 AT ROW 24.96 COL 89 COLON-ALIGNED WIDGET-ID 16
     FILL-IN-Cantidad-3 AT ROW 24.96 COL 119 COLON-ALIGNED WIDGET-ID 18
     RECT-1 AT ROW 24.69 COL 51 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 144.29 BY 25.38
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: T-CONTROL T "?" NO-UNDO INTEGRAL ChkControl
      ADDITIONAL-FIELDS:
          FIELD Libre_d01 AS DEC
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "ASIGNACION DE ORDENES"
         HEIGHT             = 25.38
         WIDTH              = 144.29
         MAX-HEIGHT         = 29.54
         MAX-WIDTH          = 144.29
         VIRTUAL-HEIGHT     = 29.54
         VIRTUAL-WIDTH      = 144.29
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-7 SELECT-CodDoc F-Main */
ASSIGN 
       BROWSE-7:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2.

/* SETTINGS FOR FILL-IN FILL-IN-Cantidad-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Cantidad-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Cantidad-3 IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-7
/* Query rebuild information for BROWSE BROWSE-7
     _TblList          = "Temp-Tables.T-CONTROL,INTEGRAL.FacCPedi WHERE Temp-Tables.T-CONTROL ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "{&Condicion}"
     _JoinCode[2]      = "INTEGRAL.FacCPedi.CodCia = Temp-Tables.T-CONTROL.CodCia
  AND INTEGRAL.FacCPedi.CodDoc = Temp-Tables.T-CONTROL.CodDoc
  AND INTEGRAL.FacCPedi.NroPed = Temp-Tables.T-CONTROL.NroPed"
     _FldNameList[1]   = Temp-Tables.T-CONTROL.CodDoc
     _FldNameList[2]   > Temp-Tables.T-CONTROL.NroPed
"T-CONTROL.NroPed" "Nro. Orden" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.FacCPedi.FchEnt
"FacCPedi.FchEnt" ? ? "date" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.FacCPedi.NomCli
"FacCPedi.NomCli" "Nombre de Cliente" ? "character" ? ? ? ? ? ? no ? no no "44.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.T-CONTROL.NroItems
"T-CONTROL.NroItems" "Nro de Items" ? "integer" ? ? ? ? ? ? no ? no no "8.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.T-CONTROL.Peso
"T-CONTROL.Peso" "Peso" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.T-CONTROL.Cantidad[1]
"T-CONTROL.Cantidad[1]" "Jaba" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.T-CONTROL.Cantidad[2]
"T-CONTROL.Cantidad[2]" "Paleta" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.T-CONTROL.Cantidad[3]
"T-CONTROL.Cantidad[3]" "Carrito" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-7 */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME F-Main:HANDLE
       ROW             = 1
       COLUMN          = 69
       HEIGHT          = 3.85
       WIDTH           = 14.29
       WIDGET-ID       = 12
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(BUTTON-Asignar:HANDLE IN FRAME F-Main).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* ASIGNACION DE ORDENES */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* ASIGNACION DE ORDENES */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-7
&Scoped-define SELF-NAME BROWSE-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-7 W-Win
ON ROW-DISPLAY OF BROWSE-7 IN FRAME F-Main
DO:
    IF AVAILABLE T-CONTROL THEN
      DISPLAY 
      T-CONTROL.Cantidad[1] @ FILL-IN-Cantidad-1
      T-CONTROL.Cantidad[2] @ FILL-IN-Cantidad-2
      T-CONTROL.Cantidad[3] @ FILL-IN-Cantidad-3
      WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-7 W-Win
ON VALUE-CHANGED OF BROWSE-7 IN FRAME F-Main
DO:
  IF AVAILABLE T-CONTROL THEN
      DISPLAY 
      T-CONTROL.Cantidad[1] @ FILL-IN-Cantidad-1
      T-CONTROL.Cantidad[2] @ FILL-IN-Cantidad-2
      T-CONTROL.Cantidad[3] @ FILL-IN-Cantidad-3
      WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Actualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Actualizar W-Win
ON CHOOSE OF BUTTON-Actualizar IN FRAME F-Main /* ACTUALIZAR */
DO:
   SESSION:SET-WAIT-STATE('GENERAL').
   RUN Carga-Temporal.
   SESSION:SET-WAIT-STATE('').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Asignar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Asignar W-Win
ON CHOOSE OF BUTTON-Asignar IN FRAME F-Main /* ASIGNAR MESA */
DO:
    RUN Asignar-Mesa.
    IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN NO-APPLY.
    APPLY 'CHOOSE':U TO  BUTTON-Actualizar.
    APPLY 'CHOOSE':U TO  SELECT-CodDoc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame W-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

APPLY 'CHOOSE':U TO BUTTON-Actualizar IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-CodDoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-CodDoc W-Win
ON VALUE-CHANGED OF SELECT-CodDoc IN FRAME F-Main
DO:
  ASSIGN {&SELF-NAME}.
  ASSIGN
      x-CodPed = ''
      x-CodDiv = ''.
  x-CodPed = ENTRY(1, SELECT-CodDoc, '|').
  IF NUM-ENTRIES(SELECT-CodDoc, '|') > 1 THEN x-CodDiv = ENTRY(2, SELECT-CodDoc, '|').
  /*MESSAGE SELF:SCREEN-VALUE SKIP x-codped x-coddiv.*/
  {&OPEN-QUERY-{&BROWSE-NAME}}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar-Mesa W-Win 
PROCEDURE Asignar-Mesa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF NOT AVAILABLE T-CONTROL THEN RETURN 'OK'.

DEF VAR pPrioridad AS CHAR.
DEF VAR pEmbalaje AS LOG.
DEF VAR pMesa AS CHAR.
DEF VAR pOk AS LOG.

RUN dist/d-chk-selecc-mesa (INPUT-OUTPUT pPrioridad,
                            INPUT-OUTPUT pEmbalaje,
                            INPUT-OUTPUT pMesa,
                            OUTPUT pOk).
IF pOk = NO THEN RETURN 'ADM-ERROR'.

/* Bloqueamos registro */
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    {lib/lock-genericov3.i ~
        &Tabla="ChkControl" ~
        &Condicion="ChkControl.CodCia = s-CodCia AND ~
        ChkControl.CodDiv = s-CodDiv AND ~
        ChkControl.CodDoc = T-CONTROL.CodDoc AND ~
        ChkControl.NroPed = T-CONTROL.NroPed" ~
        &Bloqueo="EXCLUSIVE-LOCK" ~
        &Accion="RETRY" ~
        &Mensaje="YES" ~
        &TipoError="UNDO, RETURN 'ADM-ERROR'"}
    IF ChkControl.FlgEst <> "R" THEN DO:
        MESSAGE 'La Orden YA fue asignada a una mesa' VIEW-AS ALERT-BOX ERROR.
        RELEASE ChkControl.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    CREATE ChkTareas.
    ASSIGN
        ChkTareas.CodCia = s-CodCia
        ChkTareas.CodDiv = s-CodDiv
        ChkTareas.CodDoc = ChkControl.CodDoc
        ChkTareas.NroPed = ChkControl.NroPed
        ChkTareas.Embalaje = pEmbalaje
        ChkTareas.FechaInicio = TODAY
        ChkTareas.FlgEst = "P"      /* Valor por Defecto */
        ChkTareas.HoraInicio = STRING(TIME, 'HH:MM:SS')
        ChkTareas.Mesa = pMesa
        ChkTareas.Prioridad = pPrioridad
        ChkTareas.UsuarioInicio = s-User-Id.
    /* ************************************************************************ */
    /* TABLAS RELACIONADAS */
    /* ************************************************************************ */
    /*
    
        La mesa va quedar ocupada desde cuando es asignado al chequeador
    
    FIND ChkMesas WHERE ChkMesas.CodCia = s-CodCia AND 
        ChkMesas.CodDiv = s-CodDiv AND 
        ChkMesas.Mesa = pMesa
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE ChkMesas THEN DO:
        MESSAGE 'NO se pudo actualizar el control de mesas' VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN 'ADM-ERROR'.
    END.    
    ASSIGN
        ChkMesas.FlgEst = "O".      /* Mesa OCUPADA */
    */
    FIND CURRENT Faccpedi EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE Faccpedi THEN DO:
        MESSAGE 'NO se pudo actualizar el estado de la Orden' VIEW-AS ALERT-BOX ERROR.
        UNDO, RETURN 'ADM-ERROR'.
    END.
    ASSIGN
        Faccpedi.FlgSit = "PT".     /* EN COLA DE CHEQUEO */
    /* ************************************************************************ */
    /* ************************************************************************ */
    ASSIGN
        ChkControl.FlgEst = "T".    /* Tarea Asignada */
    /* Tracking de Control */
    FIND Faccpedi WHERE FacCPedi.CodCia = s-CodCia AND 
        FacCPedi.CodDoc = ChkTareas.CodDoc AND 
        FacCPedi.NroPed = ChkTareas.NroPed NO-LOCK NO-ERROR.
    IF AVAILABLE FacCPedi THEN
        RUN vtagn/pTracking-04 (s-CodCia,
                                s-CodDiv,
                                Faccpedi.CodDoc,
                                Faccpedi.NroPed,
                                s-User-Id,
                                'CHKASG',
                                'P',
                                DATETIME(TODAY, MTIME),
                                DATETIME(TODAY, MTIME),
                                Faccpedi.CodDoc,
                                Faccpedi.NroPed,
                                Faccpedi.CodRef,
                                Faccpedi.NroRef).

END.
RELEASE ChkTareas.
RELEASE ChkControl.
RELEASE ChkMesas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal W-Win 
PROCEDURE Carga-Temporal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EMPTY TEMP-TABLE T-CONTROL.                         
FOR EACH ChkControl NO-LOCK WHERE ChkControl.CodCia = s-CodCia AND 
    ChkControl.CodDiv = s-CodDiv AND 
    ChkControl.FlgEst = "R",
    FIRST Faccpedi NO-LOCK WHERE FacCPedi.CodCia = ChkControl.CodCia AND 
    FacCPedi.CodDoc = ChkControl.CodDoc AND 
    FacCPedi.NroPed = ChkControl.NroPed:
    CREATE T-CONTROL.
    BUFFER-COPY ChkControl TO T-CONTROL.
END.
FOR EACH T-CONTROL NO-LOCK,
    FIRST Faccpedi NO-LOCK WHERE FacCPedi.CodCia = T-CONTROL.CodCia AND 
    FacCPedi.CodDoc = T-CONTROL.CodDoc AND 
    FacCPedi.NroPed = T-CONTROL.NroPed:
    T-CONTROL.CodDiv = Faccpedi.CodDiv.     /* OJO: Cambiamos a la divisi�n del O/D OTR */
    /* RHC 27/08/18 si es OTR la agrupamos por destino */
    IF T-CONTROL.CodDoc = "OTR" THEN DO:
        FIND Almacen WHERE Almacen.codcia = Faccpedi.CodCia AND
            Almacen.codalm = Faccpedi.CodCli NO-LOCK NO-ERROR.
        IF AVAILABLE Almacen THEN T-CONTROL.CodDiv = Almacen.CodDiv.
    END.
END.

DEF VAR k AS INT NO-UNDO.
DEF VAR n AS INT NO-UNDO.
DEF VAR m AS INT NO-UNDO.
DEF BUFFER BT-CONTROL FOR T-CONTROL.
DO WITH FRAME {&FRAME-NAME}:
    SELECT-CodDoc:DELETE(SELECT-CodDoc:LIST-ITEM-PAIRS).
    FOR EACH T-CONTROL NO-LOCK
        BREAK BY T-CONTROL.CodDoc BY T-CONTROL.CodDiv:
        IF FIRST-OF(T-CONTROL.CodDoc) THEN DO:
            n = 0.
            FOR EACH BT-CONTROL NO-LOCK WHERE BT-CONTROL.CodDoc = T-CONTROL.CodDoc:
                n = n + 1.
            END.
            SELECT-CodDoc:ADD-LAST( T-CONTROL.CodDoc + ' (' + STRING(n) + ')' , T-CONTROL.CodDoc).
        END.
        IF FIRST-OF(T-CONTROL.CodDoc) OR FIRST-OF(T-CONTROL.CodDiv) THEN DO:
            k = 0.
            m = 0.
        END.
        k = k + 1.
        m = m + 1.
        T-CONTROL.Libre_d01 = k.
        IF LAST-OF(T-CONTROL.CodDiv) THEN DO:
            FIND FIRST GN-DIVI OF T-CONTROL NO-LOCK.
            SELECT-CodDoc:ADD-LAST( T-CONTROL.CodDiv + ' - ' + GN-DIVI.DesDiv + '(' + STRING(m) + ')', T-CONTROL.CodDoc + '|' + T-CONTROL.CodDiv ).
        END.
    END.
    DISPLAY SELECT-CodDoc.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load W-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w-chk-asignacion.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN DISPATCH IN THIS-PROCEDURE("initialize-controls":U) NO-ERROR.
END.
ELSE MESSAGE "w-chk-asignacion.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  DISPLAY SELECT-CodDoc FILL-IN-Cantidad-1 FILL-IN-Cantidad-2 FILL-IN-Cantidad-3 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-1 BUTTON-Actualizar BUTTON-Asignar SELECT-CodDoc BROWSE-7 
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
  {src/adm/template/snd-list.i "T-CONTROL"}
  {src/adm/template/snd-list.i "FacCPedi"}

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

