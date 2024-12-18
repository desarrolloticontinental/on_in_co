&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
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
DEF INPUT PARAMETER pCodCia AS INT.
DEF INPUT PARAMETER pCodDiv AS CHAR.
DEF INPUT PARAMETER pCodDoc AS CHAR.
DEF INPUT PARAMETER pNroDoc AS CHAR.

/* Local Variable Definitions ---                                       */
DEFINE SHARED VARIABLE PV-CODCIA  AS INTEGER.
DEFINE SHARED VARIABLE S-USER-ID  AS CHAR.

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
&Scoped-Define ENABLED-OBJECTS RECT-21 RECT-22 RECT-65 f-Placa f-CodAge ~
f-Traslado f-Chofer f-NroLicencia F-CodTran F-Direccion F-Lugar F-Contacto ~
F-horario F-Fecha F-Observ Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS f-Placa f-Marca f-CodAge f-NomTra f-RucAge ~
f-Traslado f-Chofer f-NroLicencia F-CodTran F-Nombre F-ruc F-Direccion ~
F-Lugar F-Contacto F-horario F-Fecha F-Observ 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "img/b-cancel.bmp":U
     LABEL "Cancel" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "img/b-ok.bmp":U
     LABEL "OK" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE f-Chofer AS CHARACTER FORMAT "X(50)":U 
     LABEL "Chofer" 
     VIEW-AS FILL-IN 
     SIZE 62 BY .81 NO-UNDO.

DEFINE VARIABLE f-CodAge AS CHARACTER FORMAT "X(11)":U 
     LABEL "Transportista" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE F-CodTran AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencia Transporte" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .81
     FONT 4 NO-UNDO.

DEFINE VARIABLE F-Contacto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Contacto" 
     VIEW-AS FILL-IN 
     SIZE 63 BY .81
     FONT 4 NO-UNDO.

DEFINE VARIABLE F-Direccion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Direccion" 
     VIEW-AS FILL-IN 
     SIZE 63 BY .81
     FONT 4 NO-UNDO.

DEFINE VARIABLE F-Fecha AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha de Entrega" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE F-horario AS CHARACTER FORMAT "X(256)":U 
     LABEL "Horario de Atencion" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     FONT 4 NO-UNDO.

DEFINE VARIABLE F-Lugar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lugar de Entrega" 
     VIEW-AS FILL-IN 
     SIZE 63 BY .81
     FONT 4 NO-UNDO.

DEFINE VARIABLE f-Marca AS CHARACTER FORMAT "X(20)":U 
     LABEL "Veh�culo Marca" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE F-Nombre AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY .81 NO-UNDO.

DEFINE VARIABLE f-NomTra AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 49 BY .81 NO-UNDO.

DEFINE VARIABLE f-NroLicencia AS CHARACTER FORMAT "X(10)":U 
     LABEL "N� de Licencia" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE F-Observ AS CHARACTER FORMAT "X(256)":U 
     LABEL "Observaciones" 
     VIEW-AS FILL-IN 
     SIZE 62 BY .81 NO-UNDO.

DEFINE VARIABLE f-Placa AS CHARACTER FORMAT "X(10)":U 
     LABEL "N� Placa" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE F-ruc AS CHARACTER FORMAT "X(256)":U 
     LABEL "RUC" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     FONT 4 NO-UNDO.

DEFINE VARIABLE f-RucAge AS CHARACTER FORMAT "x(11)":U 
     LABEL "RUC" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE f-Traslado AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha Traslado" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 4.04.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 3.23.

DEFINE RECTANGLE RECT-65
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 6.73.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     f-Placa AT ROW 2.08 COL 15 COLON-ALIGNED WIDGET-ID 46
     f-Marca AT ROW 2.88 COL 15 COLON-ALIGNED WIDGET-ID 48
     f-CodAge AT ROW 3.69 COL 15 COLON-ALIGNED WIDGET-ID 40
     f-NomTra AT ROW 3.69 COL 29 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     f-RucAge AT ROW 4.5 COL 15 COLON-ALIGNED WIDGET-ID 42
     f-Traslado AT ROW 5.31 COL 15 COLON-ALIGNED WIDGET-ID 50
     f-Chofer AT ROW 6.12 COL 15 COLON-ALIGNED WIDGET-ID 56
     f-NroLicencia AT ROW 6.92 COL 15 COLON-ALIGNED WIDGET-ID 54
     F-CodTran AT ROW 8.54 COL 15 COLON-ALIGNED WIDGET-ID 2
     F-Nombre AT ROW 8.54 COL 24 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     F-ruc AT ROW 9.35 COL 15 COLON-ALIGNED WIDGET-ID 14
     F-Direccion AT ROW 10.15 COL 15 COLON-ALIGNED WIDGET-ID 4
     F-Lugar AT ROW 11.77 COL 15 COLON-ALIGNED WIDGET-ID 6
     F-Contacto AT ROW 12.58 COL 15 COLON-ALIGNED WIDGET-ID 8
     F-horario AT ROW 13.38 COL 15 COLON-ALIGNED WIDGET-ID 10
     F-Fecha AT ROW 13.38 COL 47 COLON-ALIGNED WIDGET-ID 34
     F-Observ AT ROW 14.19 COL 15 COLON-ALIGNED WIDGET-ID 36
     Btn_OK AT ROW 15.54 COL 2 WIDGET-ID 18
     Btn_Cancel AT ROW 15.54 COL 13 WIDGET-ID 16
     "DATOS DEL TRANSPORTISTA / CONDUCTOR" VIEW-AS TEXT
          SIZE 35 BY .5 AT ROW 1.27 COL 3 WIDGET-ID 38
     "SEGUNDO TRAMO:" VIEW-AS TEXT
          SIZE 14 BY .5 AT ROW 11.23 COL 3 WIDGET-ID 24
     "PRIMER TRAMO:" VIEW-AS TEXT
          SIZE 13 BY .5 AT ROW 8 COL 3 WIDGET-ID 26
     RECT-21 AT ROW 11.5 COL 2 WIDGET-ID 20
     RECT-22 AT ROW 8.27 COL 2 WIDGET-ID 22
     RECT-65 AT ROW 1.54 COL 2 WIDGET-ID 52
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82 BY 16.35
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
         TITLE              = "AGENCIA DE TRANSPORTES"
         HEIGHT             = 16.42
         WIDTH              = 82
         MAX-HEIGHT         = 31.31
         MAX-WIDTH          = 164.57
         VIRTUAL-HEIGHT     = 31.31
         VIRTUAL-WIDTH      = 164.57
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
   FRAME-NAME L-To-R                                                    */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:PRIVATE-DATA     = 
                "sdfsdfsdfsdfsdf".

/* SETTINGS FOR FILL-IN f-Marca IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Nombre IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-NomTra IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-ruc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-RucAge IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* AGENCIA DE TRANSPORTES */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* AGENCIA DE TRANSPORTES */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK W-Win
ON CHOOSE OF Btn_OK IN FRAME F-Main /* OK */
DO:
   RUN Graba-datos.
   IF RETURN-VALUE = "ADM-ERROR" THEN RETURN NO-APPLY.
   
   &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-CodAge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-CodAge W-Win
ON LEAVE OF f-CodAge IN FRAME F-Main /* Transportista */
DO:
    DEF VAR dist AS CHARACTER.
    IF SELF:SCREEN-VALUE = "" THEN RETURN "ADM-ERROR".
    FIND FIRST gn-prov WHERE 
         gn-prov.CodCia  = PV-CODCIA AND
         gn-prov.CodPro  = F-CodAge:SCREEN-VALUE .
    IF AVAILABLE gn-prov THEN DO:
       DISPLAY gn-prov.NomPro      @ F-NomTra
               gn-prov.Ruc         @ F-RucAge
               WITH FRAME {&FRAME-NAME}.
    END.
    ELSE DO:
       MESSAGE "Transportista no Registrado" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
    END.
    IF gn-prov.flgsit = 'C' THEN DO:
       MESSAGE 'Transportista CESADO' VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-CodTran
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CodTran W-Win
ON LEAVE OF F-CodTran IN FRAME F-Main /* Agencia Transporte */
DO:
     DEF VAR dist AS CHARACTER.
     IF SELF:SCREEN-VALUE = "" THEN RETURN "ADM-ERROR".
     FIND FIRST gn-prov WHERE gn-prov.CodCia  = PV-CODCIA 
         AND gn-prov.CodPro  = F-CodTran:SCREEN-VALUE
         NO-LOCK NO-ERROR.
     IF AVAILABLE gn-prov THEN DO:
         FIND FIRST Tabdistr WHERE  Tabdistr.CodDept  BEGINS gn-prov.CodDept 
             AND Tabdistr.CodProv  BEGINS gn-prov.CodProv 
             AND Tabdistr.Coddistr BEGINS gn-prov.CodDist
             NO-LOCK NO-ERROR.
         IF AVAILABLE Tabdistr THEN DO:
             dist = Tabdistr.Nomdistr.
             IF gn-prov.CodDept = ""  THEN dist = "". 
             DISPLAY 
                 gn-prov.NomPro      @ F-Nombre 
                 gn-prov.Ruc         @ F-ruc 
                 gn-prov.DirPro 
                 + " - " + dist      @ F-Direccion 
                 WITH FRAME {&FRAME-NAME}.
         END.
     END.
     ELSE DO:
        MESSAGE "Transportista no Registrado" VIEW-AS ALERT-BOX.
        RETURN NO-APPLY.
     END.
     IF gn-prov.flgsit = 'C' THEN DO:
        MESSAGE 'Transportista CESADO' VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
     END.
           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CodTran W-Win
ON LEFT-MOUSE-DBLCLICK OF F-CodTran IN FRAME F-Main /* Agencia Transporte */
OR F8 OF f-CodTran
DO:
    ASSIGN
        input-var-1 = ''
        input-var-2 = ''
        input-var-3 = ''
        output-var-1 = ?.
    RUN lkup/c-provee ('Transportista').
    IF output-var-1 <> ? THEN SELF:SCREEN-VALUE = output-var-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-Placa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-Placa W-Win
ON LEAVE OF f-Placa IN FRAME F-Main /* N� Placa */
DO:
  IF SELF:SCREEN-VALUE = '' THEN RETURN.
  FIND gn-vehic WHERE gn-vehic.codcia = pcodcia
      AND gn-vehic.placa = SELF:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE gn-vehic THEN DO:
      MESSAGE 'Placa NO registrada' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.
  ASSIGN
      f-CodAge:SCREEN-VALUE = gn-vehic.codpro
      f-Marca:SCREEN-VALUE = gn-vehic.marca.
  APPLY 'LEAVE':U TO f-CodAge.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-Placa W-Win
ON LEFT-MOUSE-DBLCLICK OF f-Placa IN FRAME F-Main /* N� Placa */
OR F8 OF f-Placa
DO:
    ASSIGN
        input-var-1 = ''
        input-var-2 = ''
        input-var-3 = ''
        output-var-1 = ?.
    RUN lkup/c-vehic ('Vehiculo').
    IF output-var-1 <> ? THEN SELF:SCREEN-VALUE = output-var-2.
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
  DISPLAY f-Placa f-Marca f-CodAge f-NomTra f-RucAge f-Traslado f-Chofer 
          f-NroLicencia F-CodTran F-Nombre F-ruc F-Direccion F-Lugar F-Contacto 
          F-horario F-Fecha F-Observ 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-21 RECT-22 RECT-65 f-Placa f-CodAge f-Traslado f-Chofer 
         f-NroLicencia F-CodTran F-Direccion F-Lugar F-Contacto F-horario 
         F-Fecha F-Observ Btn_OK Btn_Cancel 
      WITH FRAME F-Main IN WINDOW W-Win.
  VIEW FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Fecha-Entrega W-Win 
PROCEDURE Fecha-Entrega :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pPedido    AS CHAR.
DEF INPUT PARAMETER pFechaBase AS DATE.
DEF INPUT PARAMETER pHoraBase  AS CHAR.
DEF INPUT PARAMETER pAlmDes    AS CHAR.
DEF INPUT PARAMETER pCodPos    AS CHAR.
DEF INPUT PARAMETER pNroSKU    AS INT.
DEF INPUT PARAMETER pPeso      AS DEC.
DEF INPUT PARAMETER pCodCli    AS CHAR.
DEF INPUT PARAMETER pCodDiv    AS CHAR.
DEF INPUT PARAMETER pDocum     AS CHAR.     /* Ej, COT,999123456 */
DEF INPUT-OUTPUT PARAMETER pFchEnt    AS DATE.
DEF OUTPUT PARAMETER       pMensaje   AS CHAR.

/* OJO con la hora */
RUN gn/p-fchent-v2 (pPedido,            /* Pedido Base */
                    pFechaBase,        /* Fecha Base */
                    pHoraBase,         /* Hora Base */
                    pAlmDes,           /* Almac�n Despacho */
                    pCodPos,           /* Ubigeo (C�digo Postal o DepProDistr */
                    pNroSKU,
                    pPeso,
                    pCodCli,
                    pCodDiv,           /* Divisi�n donde se origina el documento */
                    pDocum,
                    INPUT-OUTPUT pFchEnt,
                    OUTPUT pMensaje
                    ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-datos W-Win 
PROCEDURE Graba-datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION WITH FRAME {&FRAME-NAME} ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      FIND Faccpedi WHERE Faccpedi.codcia = pcodcia
          AND Faccpedi.coddiv = pcoddiv
          AND Faccpedi.coddoc = pcoddoc
          AND Faccpedi.nroped = pnrodoc
          EXCLUSIVE-LOCK NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
          RUN dispatch IN THIS-PROCEDURE ('show-errors':U).
          UNDO, RETURN 'ADM-ERROR'.
      END.
      FIND Ccbadocu WHERE Ccbadocu.codcia = pcodcia
          AND Ccbadocu.coddiv = pcoddiv
          AND Ccbadocu.coddoc = pcoddoc
          AND Ccbadocu.nrodoc = pnrodoc
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Ccbadocu THEN CREATE Ccbadocu.
      ELSE DO:
          FIND CURRENT Ccbadocu EXCLUSIVE-LOCK NO-ERROR.
          IF ERROR-STATUS:ERROR THEN DO:
              RUN dispatch IN THIS-PROCEDURE ('show-errors':U).
              UNDO, RETURN 'ADM-ERROR'.
          END.
      END.
      ASSIGN
          Ccbadocu.codcia = pcodcia
          Ccbadocu.coddoc = pcoddoc
          Ccbadocu.nrodoc = pnrodoc
          Ccbadocu.coddiv = pcoddiv.
      ASSIGN
          CcbADocu.Libre_C[1] = f-Placa:SCREEN-VALUE 
          CcbADocu.Libre_C[2] = f-Marca:SCREEN-VALUE 
          CcbADocu.Libre_C[3] = f-CodAge:SCREEN-VALUE 
          CcbADocu.Libre_C[4] = f-NomTra:SCREEN-VALUE 
          CcbADocu.Libre_C[5] = f-RucAge:SCREEN-VALUE 
          CcbADocu.Libre_C[6] = f-NroLicencia:SCREEN-VALUE 
          CcbADocu.Libre_C[7] = f-Chofer:SCREEN-VALUE 
          CcbADocu.Libre_C[8] = f-NroLicencia:SCREEN-VALUE 
          CcbADocu.Libre_C[9] = f-CodTran:SCREEN-VALUE 
          CcbADocu.Libre_C[10] = f-Nombre:SCREEN-VALUE 
          CcbADocu.Libre_C[11] = f-Ruc:SCREEN-VALUE 
          CcbADocu.Libre_C[12] = f-Direccion:SCREEN-VALUE 
          CcbADocu.Libre_C[13] = f-Lugar:SCREEN-VALUE 
          CcbADocu.Libre_C[14] = f-Contacto:SCREEN-VALUE 
          CcbADocu.Libre_C[15] = f-Horario:SCREEN-VALUE 
          CcbADocu.Libre_C[16] = f-Observ:SCREEN-VALUE 
          CcbADocu.Libre_F[1] = INPUT f-Traslado
          CcbADocu.Libre_F[2] = INPUT f-Fecha
          NO-ERROR.
      IF ERROR-STATUS:ERROR THEN DO:
          RUN dispatch IN THIS-PROCEDURE ('show-errors':U).
          UNDO, RETURN 'ADM-ERROR'.
      END.
      /* Reactualizamos la Fecha de Entrega */
      DEF VAR pFchEnt AS DATE NO-UNDO.
      DEF VAR pMensaje AS CHAR NO-UNDO.
      IF CcbADocu.Libre_C[9] > '' THEN DO:  /* PRIMER TRAMO */
          FIND gn-prov WHERE gn-prov.CodCia = pv-codcia AND gn-prov.CodPro = CcbADocu.Libre_C[9] NO-LOCK NO-ERROR.
          IF AVAILABLE gn-prov THEN DO:
              /* Buscamos C�digo Postal (si est� configurado) */
              FIND TabDistr WHERE TabDistr.CodDepto = gn-prov.CodDept 
                  AND TabDistr.CodProvi = gn-prov.CodProv 
                  AND TabDistr.CodDistr = gn-prov.CodDist
                  NO-LOCK NO-ERROR.
              IF AVAILABLE TabDistr AND TabDistr.CodPos > '' THEN DO:
                  ASSIGN
                      FacCPedi.Ubigeo[1] = TabDistr.CodDepto 
                      FacCPedi.Ubigeo[2] = TabDistr.CodProvi 
                      FacCPedi.Ubigeo[3] = TabDistr.CodDistr
                      FacCPedi.CodPos    = TabDistr.CodPos
                      pFchEnt            = FacCPedi.FchEnt
                      pMensaje           = ''.
                  RUN Fecha-Entrega ( FacCPedi.CodDoc + "," + FacCPedi.NroRef,
                                      TODAY,
                                      STRING(TIME,'HH:MM:SS'),
                                      FacCPedi.CodAlm,
                                      FacCPedi.CodPos,
                                      0,
                                      0,
                                      FacCPedi.CodCli,
                                      FacCPedi.CodDiv,                          
                                      "COT," + FacCPedi.NroRef,
                                      INPUT-OUTPUT pFchEnt,
                                      OUTPUT pMensaje
                                      ).
                  IF pMensaje > '' THEN DO:
                      MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.
                      UNDO, RETURN 'ADM-ERROR'.
                  END.
                  ASSIGN
                      FacCPedi.FchEnt = pFchEnt.
                  MESSAGE 'Se ha actualizado el pedido con los datos del PRIMER TRAMO:' SKIP
                      'C�digo Postal:' Faccpedi.codpos SKIP
                      'Fecha de Entrega:' Faccpedi.FchEnt
                      VIEW-AS ALERT-BOX INFORMATION.
              END.
          END.

      END.

      RELEASE Faccpedi.
      RELEASE Ccbadocu.
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
  FIND Ccbadocu WHERE Ccbadocu.codcia = pcodcia
      AND Ccbadocu.coddiv = pcoddiv
      AND Ccbadocu.coddoc = pcoddoc
      AND Ccbadocu.nrodoc = pnrodoc
      NO-LOCK NO-ERROR.
  IF AVAILABLE Ccbadocu THEN DO:
      ASSIGN
          f-Placa = CcbADocu.Libre_C[1] 
          f-Marca = CcbADocu.Libre_C[2] 
          f-CodAge = CcbADocu.Libre_C[3] 
          f-NomTra = CcbADocu.Libre_C[4] 
          f-RucAge = CcbADocu.Libre_C[5] 
          f-NroLicencia = CcbADocu.Libre_C[6] 
          f-Chofer = CcbADocu.Libre_C[7] 
          f-NroLicencia = CcbADocu.Libre_C[8] 
          f-CodTran = CcbADocu.Libre_C[9] 
          f-Nombre = CcbADocu.Libre_C[10] 
          f-Ruc = CcbADocu.Libre_C[11] 
          f-Direccion = CcbADocu.Libre_C[12] 
          f-Lugar = CcbADocu.Libre_C[13] 
          f-Contacto = CcbADocu.Libre_C[14] 
          f-Horario = CcbADocu.Libre_C[15] 
          f-Observ = CcbADocu.Libre_C[16] 
          f-Traslado = CcbADocu.Libre_F[1] 
          f-Fecha = CcbADocu.Libre_F[2].
  END.

  FIND gn-prov WHERE 
       gn-prov.CodCia = PV-CODCIA AND
       gn-prov.CodPro = f-CodTran NO-LOCK NO-ERROR.
  IF AVAILABLE gn-prov THEN                                     
     ASSIGN 
            F-Nombre = gn-prov.NomPro
            F-ruc    = gn-prov.Ruc.
  FIND gn-prov WHERE 
       gn-prov.CodCia = PV-CODCIA AND
       gn-prov.CodPro = f-CodAge NO-LOCK NO-ERROR.
  IF AVAILABLE gn-prov THEN                                     
     ASSIGN 
            F-NomTra = gn-prov.NomPro
            F-RucAge = gn-prov.Ruc.

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
    input-var-1 = "".
    CASE HANDLE-CAMPO:name:
        WHEN "CndCmp" THEN ASSIGN input-var-1 = "" /*"20"*/.
        

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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

