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

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-coddiv AS CHAR.

DEF VAR x-UsrChq AS CHAR NO-UNDO.
DEF VAR pZonaPickeo AS CHAR NO-UNDO.
DEF SHARED VAR s-user-id AS CHAR.

DEF VAR x-UsrImpOD AS CHAR NO-UNDO.
DEF VAR x-FchImpOD AS DATETIME NO-UNDO.
DEF VAR x-FchPicking AS DATE INIT TODAY NO-UNDO.
DEFINE VAR x-cuales AS INT INIT 1.

&SCOPED-DEFINE Condicion VtaCDocu.CodCia = s-codcia ~
    AND VtaCDocu.DivDes = s-coddiv ~
    AND VtaCDocu.CodPed = "HPK" ~
    AND VtaCDocu.FlgEst = "P" ~
    AND LOOKUP(VtaCDocu.FlgSit, "X,C") > 0 ~
    AND (x-cuales = 1 OR ~
         (x-cuales = 2 AND vtacdocu.libre_c05 = 'FALTANTES') ~
         OR (x-cuales = 3 AND vtacdocu.libre_c05 = 'COMPLETADO')) ~
    AND DATE(Vtacdocu.FchFin) = x-FchPicking

DEFINE VAR pEstado    AS CHAR NO-UNDO.
DEFINE VAR pError     AS CHAR NO-UNDO.
DEFINE VAR pSituacion AS LOG  NO-UNDO.

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
&Scoped-define INTERNAL-TABLES VtaCDocu

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 VtaCDocu.CodPed VtaCDocu.NroPed ~
VtaCDocu.FchPed VtaCDocu.Hora VtaCDocu.NomCli Vtacdocu.usrsac @ x-UsrImpOD ~
(IF NUM-ENTRIES(Vtacdocu.Libre_c03, '|') > 1 THEN DATETIME(ENTRY(2, Vtacdocu.Libre_c03, '|')) ELSE ?) @ x-FchImpOD ~
VtaCDocu.ZonaPickeo VtaCDocu.Libre_c05 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH VtaCDocu ~
      WHERE {&Condicion} NO-LOCK ~
    BY VtaCDocu.FchPed DESCENDING ~
       BY VtaCDocu.Hora ~
        BY VtaCDocu.CodPed ~
         BY VtaCDocu.NroPed INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH VtaCDocu ~
      WHERE {&Condicion} NO-LOCK ~
    BY VtaCDocu.FchPed DESCENDING ~
       BY VtaCDocu.Hora ~
        BY VtaCDocu.CodPed ~
         BY VtaCDocu.NroPed INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 VtaCDocu
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 VtaCDocu


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RADIO-SET-CodDoc FILL-IN-NroPed ~
BUTTON-Refrescar COMBO-BOX-2 rsCuales BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-CodDoc FILL-IN-NroPed ~
COMBO-BOX-2 rsCuales 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-Estado-Orden-ok W-Win 
FUNCTION f-Estado-Orden-ok RETURNS LOGICAL
  ( INPUT pCodDoc AS CHAR , INPUT pNroDoc AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-Refrescar 
     LABEL "REFRESCAR" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE COMBO-BOX-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Hoy" 
     LABEL "Del d�a de" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Hoy","Ayer" 
     DROP-DOWN-LIST
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NroPed AS CHARACTER FORMAT "X(256)":U 
     LABEL "# de Sub-Orden" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE RADIO-SET-CodDoc AS CHARACTER INITIAL "HPK" 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "HOJA DE PICKING", "HPK"
     SIZE 43 BY .81
     BGCOLOR 14 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE rsCuales AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos", 1,
"Faltantes", 2,
"Completados", 3
     SIZE 37 BY .96 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      VtaCDocu SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 W-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      VtaCDocu.CodPed FORMAT "x(3)":U
      VtaCDocu.NroPed COLUMN-LABEL "Numero" FORMAT "X(12)":U WIDTH 11.14
      VtaCDocu.FchPed COLUMN-LABEL "Fecha!Emisi�n" FORMAT "99/99/9999":U
      VtaCDocu.Hora FORMAT "X(5)":U WIDTH 5.57
      VtaCDocu.NomCli COLUMN-LABEL "Cliente" FORMAT "x(80)":U WIDTH 35.43
      Vtacdocu.usrsac @ x-UsrImpOD COLUMN-LABEL "Pickeado por" FORMAT "x(8)":U
      (IF NUM-ENTRIES(Vtacdocu.Libre_c03, '|') > 1 THEN DATETIME(ENTRY(2, Vtacdocu.Libre_c03, '|')) ELSE ?) @ x-FchImpOD COLUMN-LABEL "Fecha y hora de Pickeo" FORMAT "99/99/9999 HH:MM":U
            WIDTH 29.86
      VtaCDocu.ZonaPickeo COLUMN-LABEL "Zona!Pickeo" FORMAT "x(10)":U
            WIDTH 17.29
      VtaCDocu.Libre_c05 COLUMN-LABEL "SITUACION" FORMAT "x(25)":U
            WIDTH 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 143 BY 20.96
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RADIO-SET-CodDoc AT ROW 1.27 COL 13 NO-LABEL WIDGET-ID 6
     FILL-IN-NroPed AT ROW 1.27 COL 69 COLON-ALIGNED WIDGET-ID 2
     BUTTON-Refrescar AT ROW 1.27 COL 111 WIDGET-ID 16
     COMBO-BOX-2 AT ROW 2.23 COL 69 COLON-ALIGNED WIDGET-ID 4
     rsCuales AT ROW 3.15 COL 71 NO-LABEL WIDGET-ID 12
     BROWSE-2 AT ROW 4.65 COL 2 WIDGET-ID 200
     "Documento:" VIEW-AS TEXT
          SIZE 8 BY .5 AT ROW 1.27 COL 4 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 144.29 BY 25.85
         FONT 4 WIDGET-ID 100.


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
         TITLE              = "CIERRE DE SUB-ORDENES"
         HEIGHT             = 25.85
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
/* BROWSE-TAB BROWSE-2 rsCuales F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "INTEGRAL.VtaCDocu"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "INTEGRAL.VtaCDocu.FchPed|no,INTEGRAL.VtaCDocu.Hora|yes,INTEGRAL.VtaCDocu.CodPed|yes,INTEGRAL.VtaCDocu.NroPed|yes"
     _Where[1]         = "{&Condicion}"
     _FldNameList[1]   = INTEGRAL.VtaCDocu.CodPed
     _FldNameList[2]   > INTEGRAL.VtaCDocu.NroPed
"VtaCDocu.NroPed" "Numero" ? "character" ? ? ? ? ? ? no ? no no "11.14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > INTEGRAL.VtaCDocu.FchPed
"VtaCDocu.FchPed" "Fecha!Emisi�n" "99/99/9999" "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > INTEGRAL.VtaCDocu.Hora
"VtaCDocu.Hora" ? ? "character" ? ? ? ? ? ? no ? no no "5.57" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > INTEGRAL.VtaCDocu.NomCli
"VtaCDocu.NomCli" "Cliente" "x(80)" "character" ? ? ? ? ? ? no ? no no "35.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"Vtacdocu.usrsac @ x-UsrImpOD" "Pickeado por" "x(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"(IF NUM-ENTRIES(Vtacdocu.Libre_c03, '|') > 1 THEN DATETIME(ENTRY(2, Vtacdocu.Libre_c03, '|')) ELSE ?) @ x-FchImpOD" "Fecha y hora de Pickeo" "99/99/9999 HH:MM" ? ? ? ? ? ? ? no ? no no "29.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > INTEGRAL.VtaCDocu.ZonaPickeo
"VtaCDocu.ZonaPickeo" "Zona!Pickeo" ? "character" ? ? ? ? ? ? no ? no no "17.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > INTEGRAL.VtaCDocu.Libre_c05
"VtaCDocu.Libre_c05" "SITUACION" "x(25)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* CIERRE DE SUB-ORDENES */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* CIERRE DE SUB-ORDENES */
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
ON ROW-DISPLAY OF BROWSE-2 IN FRAME F-Main
DO:
    CASE vtacdocu.libre_c05:
        WHEN 'COMPLETADO' THEN DO:
            vtacdocu.libre_c05:BGCOLOR IN BROWSE {&BROWSE-NAME} = 2.
        END.
        WHEN 'FALTANTES' THEN DO:
            vtacdocu.libre_c05:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12.
        END.
        OTHERWISE DO:
            /*vtacdocu.libre_c05:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12.*/
        END.
    END CASE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Refrescar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Refrescar W-Win
ON CHOOSE OF BUTTON-Refrescar IN FRAME F-Main /* REFRESCAR */
DO:
   {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-2 W-Win
ON VALUE-CHANGED OF COMBO-BOX-2 IN FRAME F-Main /* Del d�a de */
DO:
  ASSIGN {&SELF-NAME} rsCuales.
  CASE {&SELF-NAME}:
      WHEN 'Hoy' THEN x-FchPicking = TODAY.
      WHEN 'Ayer' THEN x-FchPicking = TODAY - 1.
  END CASE.
  {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-NroPed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NroPed W-Win
ON LEAVE OF FILL-IN-NroPed IN FRAME F-Main /* # de Sub-Orden */
OR RETURN OF FILL-IN-NroPed DO:
    IF SELF:SCREEN-VALUE = '' THEN RETURN.
    SELF:SCREEN-VALUE = REPLACE(SELF:SCREEN-VALUE,"'","-").
    ASSIGN {&SELF-NAME}.
    IF LENGTH(FILL-IN-NroPed) > 12 THEN DO:
        /* Transformamos el n�mero */
        FIND Facdocum WHERE Facdocum.codcia = s-codcia AND Facdocum.codcta[8] = SUBSTRING(FILL-IN-NroPed,1,3)
            NO-LOCK NO-ERROR.
        IF AVAILABLE FacDocum THEN DO:
            RADIO-SET-CodDoc = Facdocum.coddoc.
            FILL-IN-NroPed = SUBSTRING(FILL-IN-NroPed,4).
            DISPLAY FILL-IN-NroPed RADIO-SET-CodDoc WITH FRAME {&FRAME-NAME}.
        END.
    END.
    ASSIGN RADIO-SET-CodDoc FILL-IN-NroPed.
    /* Buscamos Sub-Orden */
    FIND Vtacdocu WHERE Vtacdocu.codcia = s-codcia 
        AND Vtacdocu.divdes = s-CodDiv
        AND Vtacdocu.codped = RADIO-SET-CodDoc
        AND Vtacdocu.nroped = FILL-IN-NroPed
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Vtacdocu THEN DO:
        MESSAGE 'Documento NO registrada' VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = ''.
        RETURN NO-APPLY.
    END.

    /* Ic - verificar si la ORDEN no esta anulada */
    IF f-estado-orden-ok(Vtacdocu.codped, ENTRY(1,Vtacdocu.nroped,"-")) = NO THEN DO:
        SELF:SCREEN-VALUE = ''.
        RETURN NO-APPLY.
    END.

    /*RD01- Verifica si la orden ya ha sido chequeado*/
    IF NOT (Vtacdocu.flgest = 'P' AND Vtacdocu.flgsit = 'P') THEN DO:
        MESSAGE 'El documento NO est� pendiente de cierre' VIEW-AS ALERT-BOX ERROR.
        SELF:SCREEN-VALUE = ''.
        RETURN NO-APPLY.
    END.

    RUN vta2\d-cierre-picking-sub-ordenes ( ROWID(Vtacdocu),
                                            OUTPUT x-UsrChq,
                                            OUTPUT pZonaPickeo,
                                            OUTPUT pSituacion,
                                            OUTPUT pEstado).
    IF pEstado = "ADM-ERROR" THEN RETURN NO-APPLY.

    pError = "".
    RUN Cierre-de-guia.
    ASSIGN
        COMBO-BOX-2 = 'Hoy'
        FILL-IN-NroPed = ''.
    DISPLAY COMBO-BOX-2 FILL-IN-NroPed WITH FRAME {&FRAME-NAME}.
    APPLY 'VALUE-CHANGED':U TO COMBO-BOX-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RADIO-SET-CodDoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-CodDoc W-Win
ON VALUE-CHANGED OF RADIO-SET-CodDoc IN FRAME F-Main
DO:
  ASSIGN {&SELF-NAME}.
  APPLY 'LEAVE':U TO  FILL-IN-NroPed.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsCuales
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsCuales W-Win
ON VALUE-CHANGED OF rsCuales IN FRAME F-Main
DO:
  
    ASSIGN {&SELF-NAME} COMBO-BOX-2.

    x-cuales = rsCuales.

    CASE COMBO-BOX-2:
        WHEN 'Hoy' THEN x-FchPicking = TODAY.
        WHEN 'Ayer' THEN x-FchPicking = TODAY - 1.
    END CASE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cierre-de-guia W-Win 
PROCEDURE Cierre-de-guia :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VAR lOrdenLista AS LOG NO-UNDO.
  DEFINE BUFFER b-vtacdocu FOR vtacdocu.
  DEF VAR x-CodRef AS CHAR NO-UNDO.
  DEF VAR x-NroRef AS CHAR NO-UNDO.

  DEFINE VAR lCodCliente AS CHAR INIT "".
  DEFINE VAR lCodDoc AS CHAR INIT "".

  lOrdenLista = NO.
  pError = "".

  CICLO:
  DO TRANSACTION ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
      /* Bloqueado el comprobante */
      {lib/lock-genericov3.i
          &Tabla="Vtacdocu"
          &Condicion="Vtacdocu.codcia = s-codcia ~
          AND Vtacdocu.divdes = s-CodDiv ~
          AND Vtacdocu.codped = RADIO-SET-CodDoc ~
          AND Vtacdocu.nroped = FILL-IN-NroPed"
          &Bloqueo="EXCLUSIVE-LOCK NO-ERROR"
          &Accion="RETRY"
          &Mensaje="NO"
          &txtMensaje="pError"
          }
      FIND FIRST PikSacadores WHERE PikSacadores.codcia = s-codcia 
          AND PikSacadores.coddiv = s-coddiv 
          AND PikSacadores.codper = Vtacdocu.UsrSac
          AND PikSacadores.flgest = 'A' 
          EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE PikSacadores THEN DO:
          pError = 'Pickeador no esta disponible o no esta activo, c�digo: ' + Vtacdocu.UsrSac.
          UNDO, LEAVE CICLO.
      END.
      ASSIGN
          x-CodRef = Vtacdocu.codped
          x-NroRef = ENTRY(1,Vtacdocu.nroped,'-').
      IF LOOKUP(VtaCDocu.CodPed, 'OTR,O/D,O/M') > 0 THEN DO:
          /* Buscamos la referencia obligatoriamente */
          FIND Faccpedi WHERE Faccpedi.codcia = Vtacdocu.codcia
              AND Faccpedi.coddiv = Vtacdocu.coddiv
              AND Faccpedi.coddoc = x-codref
              AND Faccpedi.nroped = x-nroref
              EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAILABLE Faccpedi THEN DO:
              pError = "Comprobante " + x-codref + " " + x-nroref + " en uso por otro usuario".
              UNDO, LEAVE CICLO.
          END.
      END.
      /* Volvemos a chequear las condiciones */
      IF NOT (Vtacdocu.flgest = 'P' AND Vtacdocu.flgsit = 'P') THEN DO:
          pError =  'ERROR en la Sub-Orden ya NO est� pendiente de Cierre de Pickeo'.
          UNDO, LEAVE CICLO.
      END.
      ASSIGN 
          Vtacdocu.flgsit = 'C'         /* Pickeo Cerrado de la SUB-ORDEN */
          Vtacdocu.Libre_c03 = s-user-id + '|' + STRING(NOW, '99/99/9999 HH:MM:SS') + '|' + x-UsrChq
          Vtacdocu.usrsacrecep = s-user-id
          Vtacdocu.zonapickeo = pZonaPickeo
          Vtacdocu.fchfin = NOW
          Vtacdocu.usuariofin = s-user-id.
      FOR EACH Vtaddocu OF Vtacdocu EXCLUSIVE-LOCK:
          ASSIGN Vtaddocu.CanBase = Vtaddocu.CanPed.
      END.
      /* ********************************************************************************* */
      IF pSituacion = NO THEN Vtacdocu.flgsit = 'X'.         /* Pickeado CON OBSERVACIONES */
      /* ********************************************************************************* */
      /* Marcar al trabajador como Libre */
      ASSIGN  
          Piksacadores.flgtarea = 'L'.    /* LIBRE */
      /* TRACKING */
      CASE Vtacdocu.CodPed:
          WHEN "O/D" OR WHEN "O/M" OR WHEN "ODC" THEN DO:
              RUN vtagn/pTracking-04 (s-CodCia,
                                      s-CodDiv,
                                      Vtacdocu.CodRef,
                                      Vtacdocu.NroRef,
                                      s-User-Id,
                                      'VODP',
                                      'P',
                                      DATETIME(TODAY, MTIME),
                                      DATETIME(TODAY, MTIME),
                                      Vtacdocu.CodPed,
                                      Vtacdocu.NroPed,
                                      Vtacdocu.CodPed,
                                      ENTRY(1,Vtacdocu.NroPed,'-')).
          END.
          WHEN "OTR" THEN DO:
              RUN vtagn/pTracking-04 (s-CodCia,
                                      s-CodDiv,
                                      Vtacdocu.CodPed,
                                      ENTRY(1,Vtacdocu.NroPed,'-'),
                                      s-User-Id,
                                      'VODP',
                                      'P',
                                      DATETIME(TODAY, MTIME),
                                      DATETIME(TODAY, MTIME),
                                      Vtacdocu.CodPed,
                                      Vtacdocu.NroPed,
                                      Vtacdocu.CodPed,
                                      ENTRY(1,Vtacdocu.NroPed,'-')).
          END.
      END CASE.
      /* Verificamos si ya se puede cerrar la orden original */
      IF LOOKUP(VtaCDocu.CodPed, 'OTR,O/D,O/M') > 0 
          AND NOT CAN-FIND(FIRST Vtacdocu WHERE Vtacdocu.codcia = Faccpedi.codcia
                      AND Vtacdocu.coddiv = Faccpedi.coddiv
                      AND Vtacdocu.codped = Faccpedi.coddoc
                      AND ENTRY(1,Vtacdocu.nroped,'-') = Faccpedi.nroped
                      AND Vtacdocu.flgsit <> "C"
                      NO-LOCK)
          THEN DO:
          Faccpedi.FlgSit = "P".    /* Picking OK */
          lOrdenLista = YES.
          lCodDoc = Faccpedi.coddoc.
          lCodCliente = faccpedi.codcli.
      END.
      /* Marco la ORDEN como COMPLETADO o FALTANTES */
      x-NroRef = ENTRY(1,FILL-IN-NroPed,'-').
      FOR EACH b-vtacdocu WHERE b-vtacdocu.codcia = s-codcia 
          AND b-vtacdocu.divdes = s-CodDiv
          AND b-vtacdocu.codped = RADIO-SET-CodDoc 
          AND ENTRY(1,b-vtacdocu.nroped,'-') = x-nroref :
          ASSIGN 
              b-VtacDocu.libre_c05 = IF(lOrdenLista = YES) THEN "COMPLETADO" ELSE "FALTANTES".
      END.
      /* *********************************************************************************** */
      /* CONTROL DE ERRORES */
      /* *********************************************************************************** */
      FINALLY:
        IF NOT AVAILABLE PikSacadores THEN pError = 'Pickeador no esta disponible o no esta activo, c�digo: ' + Vtacdocu.UsrSac.
        IF LOOKUP(VtaCDocu.CodPed, 'OTR,O/D,O/M') > 0 
            AND NOT AVAILABLE Faccpedi THEN pError = "Comprobante " + x-codref + " " + x-nroref + " en uso por otro usuario".
      END FINALLY.
      /* *********************************************************************************** */
      /* *********************************************************************************** */
  END.
  IF AVAILABLE(Vtacdocu) THEN RELEASE Vtacdocu.
  IF AVAILABLE(PikSacadores) THEN RELEASE PikSacadores.
  IF AVAILABLE(Faccpedi) THEN RELEASE Faccpedi.
  IF AVAILABLE(b-vtacdocu) THEN RELEASE b-vtacdocu.
  IF pError > "" THEN MESSAGE pError VIEW-AS ALERT-BOX ERROR.

  IF lOrdenLista = YES THEN DO:
      MESSAGE "Documento" RADIO-SET-CodDoc ENTRY(1,FILL-IN-NroPed,'-') "listo para CIERRE" VIEW-AS ALERT-BOX INFORMATION.
      /* Ic - 24Nov2017, chequear si existe otra ORDEN del mismo cliente que esta PENDIENTE de Pickear*/
      IF LOOKUP(lCoddoc,"O/D,O/M,OTR") > 0 THEN DO:
          DEFINE BUFFER x-vtacdocu FOR vtacdocu.

          FIND FIRST x-vtacdocu USE-INDEX llave04
                                WHERE x-vtacdocu.codcia = s-codcia AND 
                                        x-vtacdocu.codcli = lCodCliente AND
                                        LOOKUP(x-vtacdocu.codped,"O/D,O/M,OTR") > 0 AND
                                        x-vtacdocu.fchped >= TODAY - 30 AND
                                        x-vtacdocu.flgsit <> 'C' NO-LOCK  NO-ERROR.
          IF AVAILABLE x-vtacdocu THEN DO:
              MESSAGE "El cliente aun tiene ORDENES pendientes de Pickear...gracias".
              MESSAGE "Cuidado!! Cliente aun tiene ORDENES pendientes de Pickear...gracias".
          END.                                    
      END.           
  END. 
  RETURN "OK".

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
  DISPLAY RADIO-SET-CodDoc FILL-IN-NroPed COMBO-BOX-2 rsCuales 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RADIO-SET-CodDoc FILL-IN-NroPed BUTTON-Refrescar COMBO-BOX-2 rsCuales 
         BROWSE-2 
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
  {src/adm/template/snd-list.i "VtaCDocu"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-Estado-Orden-ok W-Win 
FUNCTION f-Estado-Orden-ok RETURNS LOGICAL
  ( INPUT pCodDoc AS CHAR , INPUT pNroDoc AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
IF pCodDoc = "ODC" THEN RETURN YES.
IF pCodDoc = "OTC" THEN RETURN YES.

 DEFINE VAR lRetVal AS LOG.

 DEFINE BUFFER x-faccpedi FOR faccpedi.

 lRetval = NO.
 FIND FIRST x-faccpedi WHERE x-faccpedi.codcia = s-codcia 
     AND x-faccpedi.coddoc = pCodDoc 
     AND x-faccpedi.nroped = pNroDoc NO-LOCK NO-ERROR.
 IF NOT AVAILABLE x-faccpedi THEN DO:
     MESSAGE pCodDoc + " - " + pNroDoc + " No existe".
 END.
 ELSE DO:
     IF x-faccpedi.flgest = 'A' THEN DO:
         MESSAGE pCodDoc + " - " + pNroDoc + " esta ANULADO".
     END.
     ELSE DO:
         lRetVal = YES.
     END.
 END.
 
 RETURN lRetVal.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
