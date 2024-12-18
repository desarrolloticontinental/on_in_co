&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE T-DI-RutaC NO-UNDO LIKE DI-RutaC
       FIELD CuentaODS AS INT
       FIELD CuentaHPK AS INT
       FIELD CuentaClientes AS INT
       FIELD SumaPeso AS DEC
       FIELD SumaVolumen AS DEC
       FIELD SumaImporte AS DEC
       FIELD CuentaItems AS INT
       FIELD Bultos AS INT.
DEFINE TEMP-TABLE T-VtaCDocu NO-UNDO LIKE VtaCDocu
       FIELD SumaPeso AS DEC
       FIELD SUmaVolumen AS DEC
       FIELD CuentaItems as INT
       FIELD SumaImporte AS DEC
       FIELD Estado AS CHAR
       FIELD Bultos AS INT
       .



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
DEF SHARED VAR s-codcia AS INTE.

/* Local Variable Definitions ---                                       */

DEF TEMP-TABLE DETALLE
    FIELD NroDoc LIKE DI-RutaC.NroDoc LABEL 'PreHoja'
    FIELD FchDoc AS DATE FORMAT '99/99/9999' LABEL 'Fecha'
    FIELD Observ AS CHAR FORMAT 'x(60)' LABEL 'Descripcion'
    FIELD CuentaODS AS INT LABEL 'O/D'
    FIELD CuentaClientes AS INT LABEL 'Nro Clientes'
    FIELD Bultos AS INT LABEL 'Bultos'
    FIELD NroPed AS CHAR FORMAT 'x(12)' LABEL 'HPK'
    FIELD FchPed AS DATE FORMAT '99/99/9999' LABEL 'Fecha'
    FIELD Hora   AS CHAR FORMAT 'x(8)' LABEL 'Hora'
    FIELD CodTer AS CHAR FORMAT 'x(20)' LABEL 'Tipo'
    FIELD CodRef AS CHAR FORMAT 'x(3)' LABEL 'Codigo'
    FIELD NroRef AS CHAR FORMAT 'x(15)' LABEL 'N�mero'
    FIELD FchRef AS DATE FORMAT '99/99/9999' LABEL 'Fecha'
    FIELD HorRef AS CHAR FORMAT 'x(10)' LABEL 'Hora'
    FIELD Departamento AS CHAR FORMAT 'x(30)' LABEL 'Departamento'
    FIELD Distrito AS CHAR FORMAT 'x(30)' LABEL 'Distrito'
    FIELD CuentaItems AS INT LABEL 'Items'
    FIELD SumaPeso AS DEC LABEL 'Peso'
    FIELD SumaVolumen AS DEC LABEL 'Volumen'
    FIELD Estado AS CHAR FORMAT 'x(20)' LABEL 'Estado'
    FIELD BultosDetalle AS INT LABEL 'Bultos'
    FIELD NomCli AS CHAR FORMAT 'x(60)' LABEL 'Nombre'
    FIELD FchEnt AS DATE FORMAT '99/99/9999' LABEL 'Fecha Entrega'
    FIELD UsrSac AS CHAR FORMAT 'x(15)' LABEL 'Picador'
    FIELD NomSac AS CHAR FORMAT 'x(60)' LABEL 'Nombre'
    FIELD ZonaPickeo AS CHAR FORMAT 'x(8)' LABEL 'Zona Pickeo'
    FIELD CodAlm AS CHAR FORMAT 'x(8)' LABEL 'Almacen'
    FIELD fFinPicking AS CHAR FORMAT 'x(10)' LABEL 'Fecha Picking Completo'
    FIELD hFinPicking AS CHAR FORMAT 'x(10)' LABEL 'Hora Picking Completo'
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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-Aplicar COMBO-BOX-CodDoc ~
FILL-IN-FchDoc-1 FILL-IN-FchDoc-2 FILL-IN-NomCli FILL-IN-NroPed ~
FILL-IN-NroHPK FILL-IN-FchEnt-1 FILL-IN-FchEnt-2 BUTTON-Limpiar BUTTON-15 ~
BUTTON-16 BUTTON-TEXTO BUTTON-18 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-CodDoc FILL-IN-FchDoc-1 ~
FILL-IN-FchDoc-2 FILL-IN-NomCli FILL-IN-NroPed FILL-IN-NroHPK ~
FILL-IN-FchEnt-1 FILL-IN-FchEnt-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDepartamento W-Win 
FUNCTION fDepartamento RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDistrito W-Win 
FUNCTION fDistrito RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-hpk-consulta-cab AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-hpk-consulta-det AS HANDLE NO-UNDO.
DEFINE VARIABLE h_tab95 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-15 
     IMAGE-UP FILE "img/print.ico":U
     LABEL "Button 15" 
     SIZE 7 BY 1.88 TOOLTIP "Impresi�n Selectiva".

DEFINE BUTTON BUTTON-16 
     IMAGE-UP FILE "img/print.ico":U
     LABEL "Button 16" 
     SIZE 7 BY 1.88 TOOLTIP "Impresi�n Total".

DEFINE BUTTON BUTTON-18 
     IMAGE-UP FILE "img/print.ico":U
     LABEL "Button 18" 
     SIZE 7 BY 1.88 TOOLTIP "Impresi�n Total (Almac�n)".

DEFINE BUTTON BUTTON-Aplicar 
     LABEL "Aplicar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-Limpiar 
     LABEL "Limpiar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-TEXTO 
     LABEL "EXPORTAR A TEXTO" 
     SIZE 19 BY 1.12.

DEFINE VARIABLE COMBO-BOX-CodDoc AS CHARACTER FORMAT "X(256)":U INITIAL "PHR" 
     LABEL "Ruta" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "PHR" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FchDoc-1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-FchDoc-2 AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-FchEnt-1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Entrega Desde" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-FchEnt-2 AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-NomCli AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nombre Cliente" 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-NroHPK AS CHARACTER FORMAT "X(15)":U 
     LABEL "# de HPK" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-NroPed AS CHARACTER FORMAT "X(15)":U 
     LABEL "# de O/D" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-HR AS LOGICAL INITIAL yes 
     LABEL "Sin H/R" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.29 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-Aplicar AT ROW 1 COL 112 WIDGET-ID 6
     COMBO-BOX-CodDoc AT ROW 1.27 COL 9 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-FchDoc-1 AT ROW 1.27 COL 29 COLON-ALIGNED WIDGET-ID 12
     FILL-IN-FchDoc-2 AT ROW 1.27 COL 49 COLON-ALIGNED WIDGET-ID 14
     FILL-IN-NomCli AT ROW 1.27 COL 76 COLON-ALIGNED WIDGET-ID 20
     TOGGLE-HR AT ROW 2.08 COL 11 WIDGET-ID 4
     FILL-IN-NroPed AT ROW 2.08 COL 29 COLON-ALIGNED WIDGET-ID 16
     FILL-IN-NroHPK AT ROW 2.08 COL 49 COLON-ALIGNED WIDGET-ID 18
     FILL-IN-FchEnt-1 AT ROW 2.08 COL 77 COLON-ALIGNED WIDGET-ID 26
     FILL-IN-FchEnt-2 AT ROW 2.08 COL 95 COLON-ALIGNED WIDGET-ID 28
     BUTTON-Limpiar AT ROW 2.08 COL 112 WIDGET-ID 8
     BUTTON-15 AT ROW 3.15 COL 81 WIDGET-ID 10
     BUTTON-16 AT ROW 5.31 COL 81 WIDGET-ID 22
     BUTTON-TEXTO AT ROW 5.46 COL 92 WIDGET-ID 30
     BUTTON-18 AT ROW 7.46 COL 81 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 127.14 BY 24.96
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: T-DI-RutaC T "?" NO-UNDO INTEGRAL DI-RutaC
      ADDITIONAL-FIELDS:
          FIELD CuentaODS AS INT
          FIELD CuentaHPK AS INT
          FIELD CuentaClientes AS INT
          FIELD SumaPeso AS DEC
          FIELD SumaVolumen AS DEC
          FIELD SumaImporte AS DEC
          FIELD CuentaItems AS INT
          FIELD Bultos AS INT
      END-FIELDS.
      TABLE: T-VtaCDocu T "?" NO-UNDO INTEGRAL VtaCDocu
      ADDITIONAL-FIELDS:
          FIELD SumaPeso AS DEC
          FIELD SUmaVolumen AS DEC
          FIELD CuentaItems as INT
          FIELD SumaImporte AS DEC
          FIELD Estado AS CHAR
          FIELD Bultos AS INT
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "CONSULTA GENERAL DE PEDIDOS"
         HEIGHT             = 24.96
         WIDTH              = 127.14
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
/* SETTINGS FOR TOGGLE-BOX TOGGLE-HR IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOGGLE-HR:HIDDEN IN FRAME F-Main           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* CONSULTA GENERAL DE PEDIDOS */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* CONSULTA GENERAL DE PEDIDOS */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 W-Win
ON CHOOSE OF BUTTON-15 IN FRAME F-Main /* Button 15 */
DO:
  MESSAGE 'Procedemos con la impresi�n?' VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO UPDATE rpta AS LOG.
  IF rpta = NO THEN RETURN.
  DEF VAR pTipo AS INT NO-UNDO.
  RUN GET-ATTRIBUTE('CURRENT-PAGE').
  pTipo = INTEGER(RETURN-VALUE).
  RUN Imprimir IN h_b-hpk-consulta-cab ( INPUT pTipo /* INTEGER */).
  /*RUN dispatch IN h_b-hpk-consulta-cab ('imprime':U).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-16 W-Win
ON CHOOSE OF BUTTON-16 IN FRAME F-Main /* Button 16 */
DO:
  MESSAGE 'Procedemos con la impresi�n?' VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO UPDATE rpta AS LOG.
  IF rpta = NO THEN RETURN.
  DEF VAR pTipo AS INT NO-UNDO.
  RUN GET-ATTRIBUTE('CURRENT-PAGE').
  pTipo = INTEGER(RETURN-VALUE).
  RUN Imprimir-Todo IN h_b-hpk-consulta-cab ( INPUT pTipo /* INTEGER */).
  /*RUN dispatch IN h_b-hpk-consulta-cab ('imprime':U).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-18 W-Win
ON CHOOSE OF BUTTON-18 IN FRAME F-Main /* Button 18 */
DO:
  MESSAGE 'Procedemos con la impresi�n?' VIEW-AS ALERT-BOX QUESTION
      BUTTONS YES-NO UPDATE rpta AS LOG.
  IF rpta = NO THEN RETURN.
  DEF VAR pTipo AS INT NO-UNDO.
  RUN GET-ATTRIBUTE('CURRENT-PAGE').
  pTipo = INTEGER(RETURN-VALUE).
  RUN Imprimir-Todo-Almacen IN h_b-hpk-consulta-cab ( INPUT pTipo /* INTEGER */).
  /*RUN dispatch IN h_b-hpk-consulta-cab ('imprime':U).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Aplicar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Aplicar W-Win
ON CHOOSE OF BUTTON-Aplicar IN FRAME F-Main /* Aplicar */
DO:
  ASSIGN COMBO-BOX-CodDoc TOGGLE-HR.
  ASSIGN FILL-IN-FchDoc-1 FILL-IN-FchDoc-2 FILL-IN-NroPed FILL-IN-NroHPK FILL-IN-NomCli.
  ASSIGN FILL-IN-FchEnt-1 FILL-IN-FchEnt-2.

  IF FILL-IN-FchEnt-1 <> ? AND FILL-IN-FchEnt-2 = ? THEN DO:
    MESSAGE "Ingrese fecha de entrega HASTA"
        VIEW-AS ALERT-BOX INFORMATION.
    RETURN NO-APPLY.
  END.
  IF FILL-IN-FchEnt-1 = ? AND FILL-IN-FchEnt-2 <> ? THEN DO:
        MESSAGE "Ingrese fecha de entrega DESDE"
        VIEW-AS ALERT-BOX INFORMATION.
        RETURN NO-APPLY.
  END.


  DEF VAR pFlgEst AS CHAR NO-UNDO.

  IF TOGGLE-HR = YES THEN pFlgEst = "P,PK,PF,PX".
  ELSE pFlgEst = "P,C".
  SESSION:SET-WAIT-STATE('GENERAL').
  RUN Carga-Filtros IN h_b-hpk-consulta-cab
    ( INPUT pFlgEst /* CHARACTER */,
      INPUT FILL-IN-FchDoc-1,
      INPUT FILL-IN-FchDoc-2,
      INPUT FILL-IN-NroPed,
      INPUT FILL-IN-NroHPK,
      INPUT FILL-IN-NomCli,
      INPUT FILL-IN-FchEnt-1,
      INPUT FILL-IN-FchEnt-2
      ).
  RUN Carga-Filtros IN h_b-hpk-consulta-det
    ( INPUT FILL-IN-FchEnt-1 /* DATE */,
      INPUT FILL-IN-FchEnt-2 /* DATE */).
  SESSION:SET-WAIT-STATE('').
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Limpiar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Limpiar W-Win
ON CHOOSE OF BUTTON-Limpiar IN FRAME F-Main /* Limpiar */
DO:
  ASSIGN
      FILL-IN-FchDoc-1 = ?
      FILL-IN-FchDoc-2 = ?
      FILL-IN-FchEnt-1 = ?
      FILL-IN-FchEnt-2 = ?
      FILL-IN-NomCli = ''
      FILL-IN-NroHPK = ''
      FILL-IN-NroPed = ''.
  DISPLAY FILL-IN-FchDoc-1 FILL-IN-FchDoc-2 FILL-IN-FchEnt-1 FILL-IN-FchEnt-2 
      FILL-IN-NomCli FILL-IN-NroHPK FILL-IN-NroPed WITH FRAME {&FRAME-NAME}.
  APPLY 'CHOOSE':U TO BUTTON-Aplicar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-TEXTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-TEXTO W-Win
ON CHOOSE OF BUTTON-TEXTO IN FRAME F-Main /* EXPORTAR A TEXTO */
DO:
    DEF VAR pOptions AS CHAR NO-UNDO.
    DEF VAR pArchivo AS CHAR NO-UNDO.

    DEF VAR OKpressed AS LOG.
    DEF VAR pNombre AS CHAR NO-UNDO.
    DEF VAR porigen AS CHAR NO-UNDO.

    SYSTEM-DIALOG GET-FILE pArchivo
        FILTERS "Archivo txt" "*.txt"
        ASK-OVERWRITE 
        CREATE-TEST-FILE
        DEFAULT-EXTENSION ".txt"
        SAVE-AS
        USE-FILENAME
        UPDATE OKpressed.
    IF OKpressed = FALSE THEN RETURN NO-APPLY.
    
    ASSIGN
        pOptions = "FileType:TXT" + CHR(1) + ~
              "Grid:ver" + CHR(1) + ~ 
              "ExcelAlert:false" + CHR(1) + ~
              "ExcelVisible:false" + CHR(1) + ~
              "Labels:yes".

    SESSION:SET-WAIT-STATE('GENERAL').

    /* Capturamos informaci�n de la cabecera y el detalle */
    EMPTY TEMP-TABLE DETALLE.
    RUN Captura-Temporal IN h_b-hpk-consulta-cab ( OUTPUT TABLE T-DI-RutaC).
    FOR EACH T-DI-RutaC NO-LOCK, FIRST DI-RutaC OF T-DI-RutaC NO-LOCK:
        EMPTY TEMP-TABLE T-VtaCDocu.
        RUN Captura-Temporal IN h_b-hpk-consulta-det ( INPUT ROWID(DI-RutaC),
                                                       INPUT-OUTPUT TABLE T-VtaCDocu).
        IF NOT CAN-FIND(FIRST T-VtaCDocu WHERE T-VtaCDocu.CodCia = DI-RutaC.CodCia
                        AND T-VtaCDocu.CodOri = DI-RutaC.CodDoc
                        AND T-VtaCDocu.NroOri = DI-RutaC.NroDoc
                        AND T-VtaCDocu.CodDiv = DI-RutaC.CodDiv
                        AND T-VtaCDocu.CodPed = "HPK" NO-LOCK)
            THEN DO:
            CREATE DETALLE.
            BUFFER-COPY T-DI-RutaC TO DETALLE.
            NEXT.
        END.
        FOR EACH T-VtaCDocu WHERE T-VtaCDocu.CodCia = DI-RutaC.CodCia
            AND T-VtaCDocu.CodOri = DI-RutaC.CodDoc
            AND T-VtaCDocu.NroOri = DI-RutaC.NroDoc
            AND T-VtaCDocu.CodDiv = DI-RutaC.CodDiv
            AND T-VtaCDocu.CodPed = "HPK" NO-LOCK,
            FIRST VtaCDocu WHERE VtaCDocu.CodCia = T-VtaCDocu.CodCia
            AND VtaCDocu.CodDiv = T-VtaCDocu.CodDiv
            AND VtaCDocu.CodPed = T-VtaCDocu.CodPed
            AND VtaCDocu.NroPed = T-VtaCDocu.NroPed NO-LOCK:
            CREATE DETALLE.
            BUFFER-COPY T-DI-RutaC TO DETALLE.
            BUFFER-COPY T-VtaCDocu EXCEPT T-VtaCDocu.Bultos
                TO DETALLE 
                ASSIGN 
                DETALLE.BultosDetalle = T-VtaCDocu.Bultos
                DETALLE.CodAlm = T-VtaCDocu.CodAlm
                DETALLE.ZonaPickeo = T-VtaCDocu.ZonaPickeo
                .
            /* Cargamos Departamento y Distrito */
            FIND FIRST Faccpedi WHERE Faccpedi.codcia = T-VtaCDocu.codcia
                AND Faccpedi.coddoc = T-VtaCDocu.codref
                AND Faccpedi.nroped = T-VtaCDocu.nroref
                NO-LOCK NO-ERROR.
            IF AVAILABLE Faccpedi THEN DO:
                DETALLE.Departamento = fDepartamento().
                DETALLE.Distrito = fDistrito().
            END.
            /* 16/03/2022 Datos MR */
            IF AVAILABLE Faccpedi THEN DO:
                ASSIGN
                    DETALLE.FchRef = Faccpedi.FchPed
                    DETALLE.HorRef = Faccpedi.Hora
                    .
            END.
            ASSIGN
                DETALLE.UsrSac = Vtacdocu.UsrSac.
            RUN logis/p-busca-por-dni (Vtacdocu.UsrSac, OUTPUT pNombre, OUTPUT pOrigen).
            ASSIGN
                DETALLE.NomSac = pNombre.
        END.
    END.
    SESSION:SET-WAIT-STATE('').

    FIND FIRST DETALLE NO-LOCK NO-ERROR.
    IF NOT AVAILABLE DETALLE THEN DO:
        MESSAGE 'No hay datos que imprimir' VIEW-AS ALERT-BOX WARNING.
        RETURN NO-APPLY.
    END.

    /* 07/02/2023: M.Ramos fecha de picking completado */
    FOR EACH DETALLE EXCLUSIVE-LOCK:
        FOR EACH LogTrkDocs NO-LOCK WHERE LogTrkDocs.CodCia = s-codcia AND
            LogTrkDocs.CodDoc = "HPK" AND 
            LogTrkDocs.NroDoc = DETALLE.nroped AND 
            LogTrkDocs.Clave = "TRCKHPK" AND 
            LogTrkDocs.Codigo = "PK_COM":
            DETALLE.fFinPicking = ENTRY(1,STRING(LogTrkDocs.Fecha),' ').
            DETALLE.hFinPicking = SUBSTRING(ENTRY(2,STRING(LogTrkDocs.Fecha),' '),1,8).
        END.
    END.
    
    DEF VAR cArchivo AS CHAR NO-UNDO.
    /* El archivo se va a generar en un archivo temporal de trabajo antes 
    de enviarlo a su directorio destino */
    cArchivo = LC(pArchivo).
    SESSION:SET-WAIT-STATE('GENERAL').
    RUN lib/tt-filev2 (TEMP-TABLE DETALLE:HANDLE, cArchivo, pOptions).
    SESSION:SET-WAIT-STATE('').
    /* ******************************************************* */
    MESSAGE 'Proceso terminado' VIEW-AS ALERT-BOX INFORMATION.
  
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
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/logis/b-hpk-consulta-cab.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-hpk-consulta-cab ).
       RUN set-position IN h_b-hpk-consulta-cab ( 3.15 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-hpk-consulta-cab ( 6.73 , 78.14 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-free/objects/tab95.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'LABEL-FONT = 4,
                     LABEL-FGCOLOR = 0,
                     FOLDER-BGCOLOR = 8,
                     FOLDER-PARENT-BGCOLOR = 8,
                     LABELS = Picking y Checking':U ,
             OUTPUT h_tab95 ).
       RUN set-position IN h_tab95 ( 9.88 , 2.00 ) NO-ERROR.
       RUN set-size IN h_tab95 ( 15.88 , 126.00 ) NO-ERROR.

       /* Links to SmartTab95 h_tab95. */
       RUN add-link IN adm-broker-hdl ( h_tab95 , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-hpk-consulta-cab ,
             BUTTON-Limpiar:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_tab95 ,
             BUTTON-18:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/logis/b-hpk-consulta-det.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-hpk-consulta-det ).
       RUN set-position IN h_b-hpk-consulta-det ( 10.96 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-hpk-consulta-det ( 14.58 , 122.14 ) NO-ERROR.

       /* Links to SmartBrowser h_b-hpk-consulta-det. */
       RUN add-link IN adm-broker-hdl ( h_b-hpk-consulta-cab , 'Record':U , h_b-hpk-consulta-det ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-hpk-consulta-det ,
             h_tab95 , 'AFTER':U ).
    END. /* Page 1 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

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
  DISPLAY COMBO-BOX-CodDoc FILL-IN-FchDoc-1 FILL-IN-FchDoc-2 FILL-IN-NomCli 
          FILL-IN-NroPed FILL-IN-NroHPK FILL-IN-FchEnt-1 FILL-IN-FchEnt-2 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BUTTON-Aplicar COMBO-BOX-CodDoc FILL-IN-FchDoc-1 FILL-IN-FchDoc-2 
         FILL-IN-NomCli FILL-IN-NroPed FILL-IN-NroHPK FILL-IN-FchEnt-1 
         FILL-IN-FchEnt-2 BUTTON-Limpiar BUTTON-15 BUTTON-16 BUTTON-TEXTO 
         BUTTON-18 
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
  FILL-IN-FchDoc-1 = ADD-INTERVAL (TODAY, -15, 'days').
  FILL-IN-FchDoc-2 = TODAY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDepartamento W-Win 
FUNCTION fDepartamento RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR pUbigeo AS CHAR NO-UNDO.
  DEF VAR pLongitud AS DEC NO-UNDO.
  DEF VAR pLatitud AS DEC NO-UNDO.
  DEF VAR pCuadrante AS CHAR NO-UNDO.

  RUN logis/p-datos-sede-auxiliar.r (
      FacCPedi.Ubigeo[2],   /* ClfAux @CL @PV */
      FacCPedi.Ubigeo[3],   /* Auxiliar */
      FacCPedi.Ubigeo[1],   /* Sede */
      OUTPUT pUbigeo,
      OUTPUT pLongitud,
      OUTPUT pLatitud
      ).
  FIND TabDepto WHERE TabDepto.CodDepto = SUBSTRING(pUbigeo,1,2) NO-LOCK NO-ERROR.
  IF AVAILABLE TabDepto THEN RETURN TabDepto.NomDepto.
  ELSE RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDistrito W-Win 
FUNCTION fDistrito RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  DEF VAR pUbigeo AS CHAR NO-UNDO.
  DEF VAR pLongitud AS DEC NO-UNDO.
  DEF VAR pLatitud AS DEC NO-UNDO.
  DEF VAR pCuadrante AS CHAR NO-UNDO.

  RUN logis/p-datos-sede-auxiliar.r (
      FacCPedi.Ubigeo[2],   /* ClfAux @CL @PV */
      FacCPedi.Ubigeo[3],   /* Auxiliar */
      FacCPedi.Ubigeo[1],   /* Sede */
      OUTPUT pUbigeo,
      OUTPUT pLongitud,
      OUTPUT pLatitud
      ).
  FIND TabDistr WHERE TabDistr.CodDepto = SUBSTRING(pUbigeo,1,2)
      AND TabDistr.CodProvi = SUBSTRING(pUbigeo,3,2)
      AND TabDistr.CodDistr = SUBSTRING(pUbigeo,5,2)
      NO-LOCK NO-ERROR.
    IF AVAILABLE TabDistr THEN RETURN TabDistr.NomDistr.
    ELSE RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

