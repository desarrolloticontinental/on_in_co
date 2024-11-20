&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
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
DEF INPUT PARAMETER pParametro AS CHAR.
/* Sintaxis:
    pParametro: lista de par�metros separados por '|'
                <CodDoc<|<FlgEst>[|CE]
                
                Ej. run vta2/waprobaciongeneral.r(PED|X)
                    run vta2/waprobaciongeneral.r(PED|X|CE)
                    
    NOTA: CE significa SOLO 001 y 002 para FlgEst = "X"
                
*/

DEF NEW SHARED VAR s-coddoc AS CHAR INIT "PED".
DEF NEW SHARED VAR s-flgest AS CHAR INIT "X".
DEF NEW SHARED VAR S-CNDVTA AS CHAR.
DEF NEW SHARED VAR s-DiasVtoPed LIKE GN-DIVI.DiasVtoPed.

DEFINE NEW SHARED VAR lh_handle AS HANDLE.
DEFINE SHARED VAR s-codcia AS INT.


s-CodDoc = ENTRY(1,pParametro,'|').
IF NUM-ENTRIES(pParametro,'|') >= 2 THEN DO:
    s-FlgEst = ENTRY(2,pParametro,'|').
END.
IF s-FlgEst = "X" THEN DO:  /* Solo para aprobaci�n de Cr�ditos y Cobranzas */
    s-CndVta = "NCE".   /* NO Contraentrega */
    IF NUM-ENTRIES(pParametro,'|') >= 3 AND ENTRY(3,pParametro,'|') <> ''
        THEN s-CndVta = ENTRY(3,pParametro,'|').
    /* Nuevo par�metro: s-CndVta = '900' */
END.

/* Consistencia */
IF LOOKUP(s-CodDoc, 'PED') = 0 THEN DO:
    MESSAGE 'Documento no v�lido:' s-coddoc SKIP 'Proceso abortado' VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
IF LOOKUP(s-FlgEst, 'T,X,W,WL,WC') = 0 THEN DO:
    MESSAGE 'Estado no v�lido:' s-flgest SKIP 'Proceso abortado' VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
/* RHC 18/01/2016 Control adicional para cambiar la divisi�n */
DEFINE NEW SHARED VAR s-acceso-total  AS LOG INIT NO NO-UNDO.
IF NUM-ENTRIES(pParametro,'|') >= 4 THEN DO:
    IF LOOKUP(ENTRY(4,pParametro,'|'), 'YES,NO') > 0 THEN DO:
        IF ENTRY(4,pParametro,'|') = 'YES' THEN s-acceso-total = YES.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 chbx-como-dni BUTTON-14 BUTTON-15 ~
BUTTON-11 BUTTON-12 BUTTON-13 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-param chbx-como-dni txtStkLetras 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-pedido-1 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-sdoxcli AS HANDLE NO-UNDO.
DEFINE VARIABLE h_bcaprobaciongeneral AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-11 
     LABEL "APROBAR" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-12 
     LABEL "RECHAZAR" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-13 
     LABEL "REFRESCAR" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-14 
     LABEL "Sentinel" 
     SIZE 27 BY 1.12.

DEFINE BUTTON BUTTON-15 
     LABEL "Cuenta Corriente" 
     SIZE 18 BY 1.12.

DEFINE VARIABLE FILL-IN-param AS CHARACTER FORMAT "X(25)":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY .77
     BGCOLOR 15 FGCOLOR 12 FONT 4 NO-UNDO.

DEFINE VARIABLE txtStkLetras AS INTEGER FORMAT "->>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY .77
     BGCOLOR 15 FGCOLOR 4 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 7 BY 1.77.

DEFINE VARIABLE chbx-como-dni AS LOGICAL INITIAL yes 
     LABEL "RUC inicia con 10 buscar como DNI" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .77
     FGCOLOR 9  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN-param AT ROW 20.42 COL 123 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     chbx-como-dni AT ROW 21.77 COL 80 WIDGET-ID 10
     txtStkLetras AT ROW 22.35 COL 67.72 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     BUTTON-14 AT ROW 22.54 COL 80 WIDGET-ID 8
     BUTTON-15 AT ROW 22.54 COL 114.43 WIDGET-ID 12
     BUTTON-11 AT ROW 24.65 COL 98 WIDGET-ID 2
     BUTTON-12 AT ROW 24.65 COL 113 WIDGET-ID 4
     BUTTON-13 AT ROW 24.65 COL 128 WIDGET-ID 6
     "Letras" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 21.69 COL 69.43 WIDGET-ID 14
          FGCOLOR 9 FONT 6
     RECT-1 AT ROW 21.5 COL 68.86 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 150.86 BY 25.27 WIDGET-ID 100.


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
         TITLE              = "PEDIDOS POR APROBAR POR CREDITOS - MULTIPLE SELECCION"
         HEIGHT             = 25.27
         WIDTH              = 143.72
         MAX-HEIGHT         = 26.92
         MAX-WIDTH          = 187.57
         VIRTUAL-HEIGHT     = 26.92
         VIRTUAL-WIDTH      = 187.57
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
/* SETTINGS FOR FILL-IN FILL-IN-param IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN txtStkLetras IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* PEDIDOS POR APROBAR POR CREDITOS - MULTIPLE SELECCION */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* PEDIDOS POR APROBAR POR CREDITOS - MULTIPLE SELECCION */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-11 W-Win
ON CHOOSE OF BUTTON-11 IN FRAME F-Main /* APROBAR */
DO:
   RUN Aprobar IN h_bcaprobaciongeneral.
   APPLY 'CHOOSE':U TO BUTTON-13.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-12 W-Win
ON CHOOSE OF BUTTON-12 IN FRAME F-Main /* RECHAZAR */
DO:
   RUN Rechazar IN h_bcaprobaciongeneral.
   APPLY 'CHOOSE':U TO BUTTON-13.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-13
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-13 W-Win
ON CHOOSE OF BUTTON-13 IN FRAME F-Main /* REFRESCAR */
DO:
  RUN dispatch IN h_bcaprobaciongeneral ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 W-Win
ON CHOOSE OF BUTTON-14 IN FRAME F-Main /* Sentinel */
DO:
  IF s-flgest = 'X' THEN DO:
      ASSIGN chbx-como-dni.
     RUN riesgo-crediticio IN h_bcaprobaciongeneral(INPUT chbx-como-dni).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 W-Win
ON CHOOSE OF BUTTON-15 IN FRAME F-Main /* Cuenta Corriente */
DO:
  RUN cuenta-corriente IN h_bcaprobaciongeneral.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

lh_Handle = THIS-PROCEDURE.

CASE s-CodDoc:
    WHEN 'PED' THEN {&WINDOW-NAME}:TITLE = 'PEDIDOS POR APROBAR'.
    WHEN 'O/D' THEN {&WINDOW-NAME}:TITLE = 'ORDENES DE DESPACHO POR APROBAR'.
END CASE.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

DO WITH FRAME {&FRAME-NAME}:
     {&WINDOW-NAME}:TITLE = "APROBACION/RECHAZO DE ".
     IF s-FlgEst = "W" THEN {&WINDOW-NAME}:TITLE = "PRE APROBACION/RECHAZO DE ".
     CASE s-CodDoc:
         WHEN 'PED' THEN {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + "PEDIDOS".
     END CASE.
     IF s-FlgEst = "W" THEN {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - CON BAJO MARGEN".
     IF s-FlgEst = "WL" THEN {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - LOGISTICA".
     IF s-FlgEst = "WC" THEN {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - COMERCIAL".
END.

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
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vta2/bcaprobaciongeneral.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_bcaprobaciongeneral ).
       RUN set-position IN h_bcaprobaciongeneral ( 1.00 , 2.00 ) NO-ERROR.
       RUN set-size IN h_bcaprobaciongeneral ( 8.12 , 142.14 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vta2/b-pedido-1.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-pedido-1 ).
       RUN set-position IN h_b-pedido-1 ( 9.35 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-pedido-1 ( 10.96 , 142.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta2/b-sdoxcli.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-sdoxcli ).
       RUN set-position IN h_b-sdoxcli ( 20.38 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-sdoxcli ( 5.65 , 65.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-pedido-1. */
       RUN add-link IN adm-broker-hdl ( h_bcaprobaciongeneral , 'Record':U , h_b-pedido-1 ).

       /* Links to SmartBrowser h_b-sdoxcli. */
       RUN add-link IN adm-broker-hdl ( h_b-pedido-1 , 'Record':U , h_b-sdoxcli ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_bcaprobaciongeneral ,
             FILL-IN-param:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-pedido-1 ,
             h_bcaprobaciongeneral , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-sdoxcli ,
             h_b-pedido-1 , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

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
  DISPLAY FILL-IN-param chbx-como-dni txtStkLetras 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-1 chbx-como-dni BUTTON-14 BUTTON-15 BUTTON-11 BUTTON-12 BUTTON-13 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  fill-in-param:SCREEN-VALUE IN FRAME {&FRAME-NAME} = pParametro.

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-stk-letras W-Win 
PROCEDURE show-stk-letras :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER pCodCli AS CHAR.

DEFINE BUFFER x-gn-clie FOR gn-clie.

DO WITH FRAME {&FRAME-NAME}:
    txtStkLetras:SCREEN-VALUE = "0".

    FIND FIRST ccbstklet WHERE ccbstklet.codcia = s-codcia AND 
                                    ccbstklet.codclie = pCodCli NO-LOCK NO-ERROR.

    IF AVAILABLE ccbstklet THEN txtStkLetras:SCREEN-VALUE = STRING(ccbstklet.qstklet,"->>,>>99").
END.

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

