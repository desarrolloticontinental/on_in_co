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

DEF SHARED VAR s-codcia AS INTE.

DEF VAR s-tabla AS CHAR INIT 'CORRECCION' NO-UNDO.

DEF VAR x-CodDoc AS CHAR NO-UNDO.
DEF VAR x-NroDoc AS CHAR NO-UNDO.
DEF VAR x-CodMat AS CHAR NO-UNDO.
DEF VAR x-CanOld AS DECI NO-UNDO.
DEF VAR x-CanNew AS DECI NO-UNDO.

DEF VAR pCodDoc AS CHAR NO-UNDO.
DEF VAR pNroDoc AS CHAR NO-UNDO.

&SCOPED-DEFINE Condicion (logtabla.codcia = s-CodCia ~
                AND logtabla.Evento = "CORRECCION" ~
                AND logtabla.Tabla = "FACDPEDI" ~
                AND ( TRUE <> (pCodDoc > "") OR ENTRY(1,logtabla.ValorLlave,"|") = pCodDoc ) ~
                AND ( TRUE <> (pNroDoc > "") OR ENTRY(2,logtabla.ValorLlave,"|") = pNroDoc ) ~
                    )
  /*
&SCOPED-DEFINE Condicion (logtabla.codcia = s-CodCia ~
                AND logtabla.Evento = "CORRECCION" ~
                AND logtabla.Tabla = "FACDPEDI" ~
                AND ( TRUE <> (pCodDoc > "") OR ENTRY(1,logtabla.ValorLlave,"|") = pCodDoc ) ~
                AND ( TRUE <> (pNroDoc > "") OR ENTRY(2,logtabla.ValorLlave,"|") = pNroDoc )
*/

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
&Scoped-define INTERNAL-TABLES logtabla

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 logtabla.Dia logtabla.Hora ~
logtabla.Usuario entry(1,logtabla.ValorLlave,"|") @ x-CodDoc ~
entry(2,logtabla.ValorLlave,"|") @ x-NroDoc ~
entry(3,logtabla.ValorLlave,"|") @ x-CodMat ~
DECIMAL(entry(4,logtabla.ValorLlave,"|")) @ x-CanOld ~
DECIMAL(entry(5,logtabla.ValorLlave,"|")) @ x-CanNew 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH logtabla ~
      WHERE {&Condicion} NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH logtabla ~
      WHERE {&Condicion} NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 logtabla
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 logtabla


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS COMBO-BOX-CodDoc BUTTON-3 BUTTON-4 ~
FILL-IN-NroDoc BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-CodDoc FILL-IN-NroDoc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-3 
     LABEL "FILTRAR" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-4 
     LABEL "LIMPIAR FILTROS" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE COMBO-BOX-CodDoc AS CHARACTER FORMAT "X(256)":U 
     LABEL "C�digo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","O/D","OTR" 
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NroDoc AS CHARACTER FORMAT "X(12)":U 
     LABEL "N�mero" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      logtabla SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 W-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      logtabla.Dia FORMAT "99/99/9999":U
      logtabla.Hora FORMAT "X(8)":U WIDTH 8.72
      logtabla.Usuario FORMAT "x(12)":U
      entry(1,logtabla.ValorLlave,"|") @ x-CodDoc COLUMN-LABEL "Doc" FORMAT "x(5)":U
      entry(2,logtabla.ValorLlave,"|") @ x-NroDoc COLUMN-LABEL "Numero" FORMAT "x(12)":U
      entry(3,logtabla.ValorLlave,"|") @ x-CodMat COLUMN-LABEL "Articulo" FORMAT "x(8)":U
      DECIMAL(entry(4,logtabla.ValorLlave,"|")) @ x-CanOld COLUMN-LABEL "Antes"
            WIDTH 11.43
      DECIMAL(entry(5,logtabla.ValorLlave,"|")) @ x-CanNew COLUMN-LABEL "Despues"
            WIDTH 8.72
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 74 BY 21
         FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     COMBO-BOX-CodDoc AT ROW 1.27 COL 20 COLON-ALIGNED WIDGET-ID 2
     BUTTON-3 AT ROW 1.27 COL 41 WIDGET-ID 6
     BUTTON-4 AT ROW 1.27 COL 56 WIDGET-ID 8
     FILL-IN-NroDoc AT ROW 2.08 COL 20 COLON-ALIGNED WIDGET-ID 4
     BROWSE-2 AT ROW 3.42 COL 5 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 83.29 BY 24.35
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
         TITLE              = "CONSULTA LOG DE CIERRE PICKING CON OBSERVACIONES"
         HEIGHT             = 24.35
         WIDTH              = 83.29
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
/* BROWSE-TAB BROWSE-2 FILL-IN-NroDoc F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "INTEGRAL.logtabla"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "{&Condicion}"
     _FldNameList[1]   = INTEGRAL.logtabla.Dia
     _FldNameList[2]   > INTEGRAL.logtabla.Hora
"logtabla.Hora" ? ? "character" ? ? ? ? ? ? no ? no no "8.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = INTEGRAL.logtabla.Usuario
     _FldNameList[4]   > "_<CALC>"
"entry(1,logtabla.ValorLlave,""|"") @ x-CodDoc" "Doc" "x(5)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"entry(2,logtabla.ValorLlave,""|"") @ x-NroDoc" "Numero" "x(12)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > "_<CALC>"
"entry(3,logtabla.ValorLlave,""|"") @ x-CodMat" "Articulo" "x(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"DECIMAL(entry(4,logtabla.ValorLlave,""|"")) @ x-CanOld" "Antes" ? ? ? ? ? ? ? ? no ? no no "11.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"DECIMAL(entry(5,logtabla.ValorLlave,""|"")) @ x-CanNew" "Despues" ? ? ? ? ? ? ? ? no ? no no "8.72" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* CONSULTA LOG DE CIERRE PICKING CON OBSERVACIONES */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* CONSULTA LOG DE CIERRE PICKING CON OBSERVACIONES */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* FILTRAR */
DO:
  ASSIGN
      pCodDoc = COMBO-BOX-CodDoc:SCREEN-VALUE
      pNroDoc = FILL-IN-NroDoc:SCREEN-VALUE.
  IF pCodDoc > '' THEN DO:        
      {&OPEN-QUERY-{&BROWSE-NAME}}
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 W-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* LIMPIAR FILTROS */
DO:
  ASSIGN
      COMBO-BOX-CodDoc = ''
      FILL-IN-NroDoc = ''
      pCodDoc = ''
      pNroDoc = ''.
  COMBO-BOX-CodDoc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ' '.
  DISPLAY COMBO-BOX-CodDoc FILL-IN-NroDoc WITH FRAME {&FRAME-NAME}.
  {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
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
  DISPLAY COMBO-BOX-CodDoc FILL-IN-NroDoc 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE COMBO-BOX-CodDoc BUTTON-3 BUTTON-4 FILL-IN-NroDoc BROWSE-2 
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
  {src/adm/template/snd-list.i "logtabla"}

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

