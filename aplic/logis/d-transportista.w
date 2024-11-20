&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T-CcbADocu NO-UNDO LIKE CcbADocu.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
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

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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

DEF INPUT-OUTPUT PARAMETER TABLE FOR T-Ccbadocu.
DEF INPUT PARAMETER pCodCia AS INT NO-UNDO.
DEF INPUT PARAMETER pCodDiv AS CHAR NO-UNDO.
DEF INPUT PARAMETER pCodDoc AS CHAR NO-UNDO.
DEF INPUT PARAMETER pNroDoc AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE SHARED VARIABLE s-CodCia   AS INTEGER.
DEFINE SHARED VARIABLE cl-codcia  AS INTEGER.
DEFINE SHARED VARIABLE PV-CODCIA  AS INTEGER.
DEFINE SHARED VARIABLE S-USER-ID  AS CHAR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-66 RECT-67 RECT-21 f-Placa f-Traslado ~
f-Chofer f-NroLicencia F-CodTran f-SedeTran F-Contacto F-horario F-Observ ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS f-Placa f-Marca f-CodAge f-NomTra f-RucAge ~
f-Traslado f-Chofer f-NroLicencia f-Certificado F-CodTran F-Nombre ~
f-SedeTran F-Direccion F-ruc F-Lugar F-Contacto F-horario F-Observ 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     IMAGE-UP FILE "img/error.ico":U
     LABEL "Cancel" 
     SIZE 15 BY 1.54
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     IMAGE-UP FILE "img/ok.ico":U
     LABEL "OK" 
     SIZE 15 BY 1.54
     BGCOLOR 8 .

DEFINE VARIABLE f-Certificado AS CHARACTER FORMAT "X(30)":U 
     LABEL "N� Certif. Inscrip." 
     VIEW-AS FILL-IN 
     SIZE 30 BY .81 NO-UNDO.

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
     SIZE 12 BY .81
     FONT 4 NO-UNDO.

DEFINE VARIABLE F-Contacto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Contacto" 
     VIEW-AS FILL-IN 
     SIZE 63 BY .81
     FONT 4 NO-UNDO.

DEFINE VARIABLE F-Direccion AS CHARACTER FORMAT "X(256)":U 
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
     SIZE 63 BY .81 NO-UNDO.

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

DEFINE VARIABLE f-SedeTran AS CHARACTER FORMAT "X(8)":U 
     LABEL "Sede" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE f-Traslado AS DATE FORMAT "99/99/99":U 
     LABEL "Fecha Traslado" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY 4.04.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY 7.54.

DEFINE RECTANGLE RECT-67
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 104 BY 3.23.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     f-Placa AT ROW 2.08 COL 20 COLON-ALIGNED WIDGET-ID 46
     f-Marca AT ROW 2.88 COL 20 COLON-ALIGNED WIDGET-ID 48
     f-CodAge AT ROW 3.69 COL 20 COLON-ALIGNED WIDGET-ID 40
     f-NomTra AT ROW 3.69 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 44
     f-RucAge AT ROW 4.5 COL 20 COLON-ALIGNED WIDGET-ID 42
     f-Traslado AT ROW 5.31 COL 20 COLON-ALIGNED WIDGET-ID 50
     f-Chofer AT ROW 6.12 COL 20 COLON-ALIGNED WIDGET-ID 56
     f-NroLicencia AT ROW 6.92 COL 20 COLON-ALIGNED WIDGET-ID 54
     f-Certificado AT ROW 7.73 COL 20 COLON-ALIGNED WIDGET-ID 60
     F-CodTran AT ROW 9.35 COL 20 COLON-ALIGNED WIDGET-ID 2
     F-Nombre AT ROW 9.35 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     f-SedeTran AT ROW 10.15 COL 20 COLON-ALIGNED WIDGET-ID 106
     F-Direccion AT ROW 10.15 COL 32 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     F-ruc AT ROW 10.96 COL 20 COLON-ALIGNED WIDGET-ID 14
     F-Lugar AT ROW 12.58 COL 20 COLON-ALIGNED WIDGET-ID 6
     F-Contacto AT ROW 13.38 COL 20 COLON-ALIGNED WIDGET-ID 8
     F-horario AT ROW 14.19 COL 20 COLON-ALIGNED WIDGET-ID 10
     F-Fecha AT ROW 14.19 COL 52 COLON-ALIGNED WIDGET-ID 34
     F-Observ AT ROW 15 COL 20 COLON-ALIGNED WIDGET-ID 36
     Btn_OK AT ROW 16.62 COL 2
     Btn_Cancel AT ROW 16.62 COL 17
     "PRIMER TRAMO:" VIEW-AS TEXT
          SIZE 13 BY .5 AT ROW 8.81 COL 8 WIDGET-ID 26
          BGCOLOR 9 FGCOLOR 15 
     "DATOS DEL TRANSPORTISTA / CONDUCTOR" VIEW-AS TEXT
          SIZE 35 BY .5 AT ROW 1.27 COL 8 WIDGET-ID 38
          BGCOLOR 9 FGCOLOR 15 
     "SEGUNDO TRAMO:" VIEW-AS TEXT
          SIZE 14 BY .5 AT ROW 12.04 COL 8 WIDGET-ID 24
          BGCOLOR 9 FGCOLOR 15 
     RECT-66 AT ROW 1.54 COL 2 WIDGET-ID 108
     RECT-67 AT ROW 9.08 COL 2 WIDGET-ID 110
     RECT-21 AT ROW 12.31 COL 2 WIDGET-ID 20
     SPACE(2.13) SKIP(2.29)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "AGENCIA DE TRANSPORTES"
         CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: T-CcbADocu T "?" NO-UNDO INTEGRAL CcbADocu
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm-vm/method/vmviewer.i}
{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE
       FRAME D-Dialog:PRIVATE-DATA     = 
                "sdfsdfsdfsdfsdf".

/* SETTINGS FOR FILL-IN f-Certificado IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-CodAge IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Direccion IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Fecha IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       F-Fecha:HIDDEN IN FRAME D-Dialog           = TRUE.

/* SETTINGS FOR FILL-IN F-Lugar IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-Marca IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-Nombre IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-NomTra IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-ruc IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN f-RucAge IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* AGENCIA DE TRANSPORTES */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO:
    /* Validaciones */
    IF F-CodTran:SCREEN-VALUE > '' THEN DO:
        FIND gn-provd WHERE gn-provd.CodCia = pv-codcia AND
            gn-provd.CodPro = F-CodTran:SCREEN-VALUE AND 
            gn-provd.Sede = f-SedeTran:SCREEN-VALUE
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE gn-provd THEN DO:
            MESSAGE 'Debe registrar la SEDE' VIEW-AS ALERT-BOX ERROR.
            APPLY 'ENTRY':U TO f-SedeTran.
            RETURN NO-APPLY.
        END.
    END.

    RUN Graba-datos.
    IF RETURN-VALUE = "ADM-ERROR" THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME f-CodAge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-CodAge D-Dialog
ON LEAVE OF f-CodAge IN FRAME D-Dialog /* Transportista */
DO:
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CodTran D-Dialog
ON LEAVE OF F-CodTran IN FRAME D-Dialog /* Agencia Transporte */
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
                 (gn-prov.DirPro + " - " + dist) @ F-Direccion 
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
     APPLY 'LEAVE':U TO f-SedeTran.
           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CodTran D-Dialog
ON LEFT-MOUSE-DBLCLICK OF F-CodTran IN FRAME D-Dialog /* Agencia Transporte */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-Placa D-Dialog
ON LEAVE OF f-Placa IN FRAME D-Dialog /* N� Placa */
DO:
  IF SELF:SCREEN-VALUE = '' THEN RETURN.
  FIND gn-vehic WHERE gn-vehic.codcia = pcodcia
      AND gn-vehic.placa = f-Placa:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF NOT AVAILABLE gn-vehic THEN DO:
      MESSAGE 'Placa NO registrada' VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
  END.
  ASSIGN
      f-Marca:SCREEN-VALUE = gn-vehic.marca
      f-CodAge:SCREEN-VALUE = gn-vehic.codpro.
  APPLY 'LEAVE':U TO f-CodAge.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-Placa D-Dialog
ON LEFT-MOUSE-DBLCLICK OF f-Placa IN FRAME D-Dialog /* N� Placa */
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


&Scoped-define SELF-NAME f-SedeTran
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL f-SedeTran D-Dialog
ON LEAVE OF f-SedeTran IN FRAME D-Dialog /* Sede */
DO:
  F-Direccion:SCREEN-VALUE = ''.
  FIND gn-provd WHERE gn-provd.CodCia = pv-codcia AND
      gn-provd.CodPro = F-CodTran:SCREEN-VALUE AND 
      gn-provd.Sede = f-SedeTran:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAILABLE gn-provd THEN F-Direccion:SCREEN-VALUE = gn-provd.DirPro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
          f-NroLicencia f-Certificado F-CodTran F-Nombre f-SedeTran F-Direccion 
          F-ruc F-Lugar F-Contacto F-horario F-Observ 
      WITH FRAME D-Dialog.
  ENABLE RECT-66 RECT-67 RECT-21 f-Placa f-Traslado f-Chofer f-NroLicencia 
         F-CodTran f-SedeTran F-Contacto F-horario F-Observ Btn_OK Btn_Cancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Datos D-Dialog 
PROCEDURE Graba-Datos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION WITH FRAME {&FRAME-NAME} ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
      EMPTY TEMP-TABLE T-CcbADocu.
      CREATE T-CcbADocu.
      ASSIGN
          T-Ccbadocu.codcia = pcodcia
          T-Ccbadocu.coddoc = pcoddoc
          T-Ccbadocu.nrodoc = pnrodoc
          T-Ccbadocu.coddiv = pcoddiv.
      ASSIGN
          T-CcbADocu.Libre_C[1] = f-Placa:SCREEN-VALUE 
          T-CcbADocu.Libre_C[2] = f-Marca:SCREEN-VALUE 
          T-CcbADocu.Libre_C[3] = f-CodAge:SCREEN-VALUE 
          T-CcbADocu.Libre_C[4] = f-NomTra:SCREEN-VALUE 
          T-CcbADocu.Libre_C[5] = f-RucAge:SCREEN-VALUE 
          T-CcbADocu.Libre_C[6] = f-NroLicencia:SCREEN-VALUE 
          T-CcbADocu.Libre_C[7] = f-Chofer:SCREEN-VALUE 
          T-CcbADocu.Libre_C[8] = f-NroLicencia:SCREEN-VALUE 
          T-CcbADocu.Libre_C[9] = f-CodTran:SCREEN-VALUE 
          T-CcbADocu.Libre_C[20] = f-SedeTran:SCREEN-VALUE 
          T-CcbADocu.Libre_C[10] = f-Nombre:SCREEN-VALUE 
          T-CcbADocu.Libre_C[11] = f-Ruc:SCREEN-VALUE 
          T-CcbADocu.Libre_C[12] = f-Direccion:SCREEN-VALUE 
          T-CcbADocu.Libre_C[13] = f-Lugar:SCREEN-VALUE 
          T-CcbADocu.Libre_C[14] = f-Contacto:SCREEN-VALUE 
          T-CcbADocu.Libre_C[15] = f-Horario:SCREEN-VALUE 
          T-CcbADocu.Libre_C[16] = f-Observ:SCREEN-VALUE 
          T-CcbADocu.Libre_C[17] = f-Certificado:SCREEN-VALUE 
          /*T-CcbADocu.Libre_F[1] = INPUT f-Traslado*/
          T-CcbADocu.Libre_F[2] = INPUT f-Fecha.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND FIRST T-CcbADocu NO-LOCK NO-ERROR.
  IF AVAILABLE T-CcbADocu THEN DO:
      /* DATOS DEL TRANSPORTISTA / CONDUCTOR */
      ASSIGN
          f-Placa = T-CcbADocu.Libre_C[1] 
          f-Marca = T-CcbADocu.Libre_C[2] 
          f-CodAge = T-CcbADocu.Libre_C[3] 
          f-NroLicencia = T-CcbADocu.Libre_C[6] 
          f-Chofer = T-CcbADocu.Libre_C[7] 
          f-NroLicencia = T-CcbADocu.Libre_C[8]
          f-Traslado = T-CcbADocu.Libre_F[1]
          f-SedeTran = T-CcbADocu.Libre_C[20].
      /* PRIMER TRAMO: */
      ASSIGN
          f-CodTran = T-CcbADocu.Libre_C[9]
          f-Nombre = T-CcbADocu.Libre_C[10] 
          f-Ruc = T-CcbADocu.Libre_C[11] 
          f-Direccion = T-CcbADocu.Libre_C[12] .
      /* SEGUNDO TRAMO: */
      ASSIGN
          f-Lugar = T-CcbADocu.Libre_C[13] 
          f-Contacto = T-CcbADocu.Libre_C[14] 
          f-Horario = T-CcbADocu.Libre_C[15] 
          f-Observ = T-CcbADocu.Libre_C[16] 
          f-Fecha = T-CcbADocu.Libre_F[2].
      /* TRANSPORTISTA */
      IF f-CodAge > '' THEN DO:
          FIND gn-prov WHERE gn-prov.CodCia = PV-CODCIA AND
              gn-prov.CodPro = f-CodAge NO-LOCK NO-ERROR.
          IF AVAILABLE gn-prov THEN DO:                                    
              ASSIGN 
                F-NomTra = gn-prov.NomPro
                F-RucAge = gn-prov.Ruc.
          END.
      END.
      /* AGENCIA */
      IF f-CodTran > '' THEN DO:
          FIND gn-prov WHERE gn-prov.CodCia = pv-CodCia AND
              gn-prov.CodPro = f-CodTran NO-LOCK NO-ERROR.
          IF AVAILABLE gn-prov THEN DO:
              ASSIGN
                  f-Nombre = gn-prov.NomPro 
                  f-Ruc = gn-prov.Ruc 
                  f-Direccion = gn-prov.DirPro.
          END.
          FIND gn-provd WHERE gn-provd.CodCia = pv-codcia AND
              gn-provd.CodPro = f-CodTran AND
              gn-provd.Sede  = f-SedeTran
              NO-LOCK NO-ERROR.
          IF AVAILABLE gn-provd THEN DO:
              ASSIGN
                  f-Direccion = gn-provd.DirPro.
          END.
      END.

  END.
  /* Cargamos el SEGUNDO TRAMO */
  FIND Faccpedi WHERE Faccpedi.codcia = pcodcia
      AND Faccpedi.coddiv = pcoddiv
      AND Faccpedi.coddoc = pcoddoc
      AND Faccpedi.nroped = pnrodoc
      NO-LOCK NO-ERROR.
  FIND Gn-ClieD WHERE Gn-ClieD.CodCia = cl-codcia AND
      Gn-ClieD.CodCli = Faccpedi.codcli AND
      Gn-ClieD.Sede = Faccpedi.sede
      NO-LOCK NO-ERROR.
  IF AVAILABLE Gn-ClieD THEN f-Lugar = Gn-ClieD.DirCli.     /* Valor por defecto */
  IF TRUE <> (f-Observ > '') THEN f-Observ = FacCPedi.Glosa.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      APPLY 'LEAVE':U TO f-Placa.
      APPLY 'LEAVE':U TO f-CodAge.
      APPLY 'LEAVE':U TO f-NroLicencia.
      APPLY 'LEAVE':U TO f-CodTran.
      APPLY 'LEAVE':U TO f-SedeTran.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procesa-parametros D-Dialog 
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
        WHEN "" THEN .
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE recoge-parametros D-Dialog 
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
        WHEN "f-SedeTran" THEN 
            ASSIGN 
                input-var-1 = f-CodTran:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                input-var-2 = ''
                input-var-3 = ''.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
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

