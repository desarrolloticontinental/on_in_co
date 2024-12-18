&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE BUFFER b-ccbcdocu FOR CcbCDocu.
DEFINE BUFFER CREDITO FOR CcbCDocu.
DEFINE TEMP-TABLE T-CCOTI NO-UNDO LIKE FacCPedi.
DEFINE TEMP-TABLE T-CDOCU NO-UNDO LIKE CcbCDocu.
DEFINE TEMP-TABLE T-DCOTI NO-UNDO LIKE FacDPedi.
DEFINE TEMP-TABLE T-DDOCU NO-UNDO LIKE CcbDDocu
       INDEX Idx00 AS PRIMARY CodMat.
DEFINE TEMP-TABLE T-FELogErrores NO-UNDO LIKE FELogErrores.
DEFINE TEMP-TABLE ttCcbcdocu NO-UNDO LIKE CcbCDocu.
DEFINE TEMP-TABLE ttCcbddocu NO-UNDO LIKE CcbDDocu.
DEFINE TEMP-TABLE txCcbCDocu NO-UNDO LIKE CcbCDocu.
DEFINE TEMP-TABLE txCcbDDocu NO-UNDO LIKE CcbDDocu.



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

&SCOPED-DEFINE precio-venta-general pri/PrecioVentaMayorCredito.p

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER pParam AS CHAR.

IF TRUE <> (pParam > '') THEN RETURN ERROR.
IF NUM-ENTRIES(pParam) <> 2 THEN RETURN ERROR.

/* Local Variable Definitions ---                                       */
DEF SHARED VAR s-codcia AS INTE.
DEF SHARED VAR cl-codcia AS INTE.
DEF SHARED VAR s-coddiv AS CHAR.
DEF SHARED VAR s-user-id AS CHAR.

DEFINE VAR x-nueva-arimetica-sunat-2021 AS LOG.

x-nueva-arimetica-sunat-2021 = YES.

/* DESCUENTO POR VOLUMEN/ ESCALA */
DEF VAR pCodCtaNC AS CHAR INIT '00002' NO-UNDO.
DEF VAR pCodCtaND AS CHAR INIT '00002' NO-UNDO.

pCodCtaNC = ENTRY(1,pParam).
pCodCtaND = ENTRY(2,pParam).
IF NOT CAN-FIND(FIRST CcbTabla WHERE CcbTabla.CodCia = s-codcia
                AND CcbTabla.Tabla = "N/C" 
                AND CcbTabla.Codigo = pCodCtaNC
                NO-LOCK)
    THEN DO:
    MESSAGE 'NO configurado el concepto' pCodCtaND 'para la N/C' VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
IF NOT CAN-FIND(FIRST CcbTabla WHERE CcbTabla.CodCia = s-codcia
                AND CcbTabla.Tabla = "N/D" 
                AND CcbTabla.Codigo = pCodCtaND
                NO-LOCK)
    THEN DO:
    MESSAGE 'NO configurado el concepto' pCodCtaND 'para la N/D' VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

IF NOT CAN-FIND(FIRST FacCorre WHERE FacCorre.CodCia = s-codcia AND
                FacCorre.CodDiv = s-coddiv AND
                FacCorre.CodDoc = 'N/C' AND
                FacCorre.FlgEst = YES NO-LOCK)
    THEN DO:
    MESSAGE 'NO hay ning�n correlativo definido para la N/C' SKIP
        'en la divisi�n' s-coddiv VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
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
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 FILL-IN_CodCli ~
COMBO-BOX_CodDiv BUTTON_Calcular COMBO-BOX_NroSerNC COMBO-BOX_NroSerND 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_CodCli FILL-IN_NomCli ~
COMBO-BOX_CodDiv COMBO-BOX_NroSerNC FILL-IN_CodCtaNC FILL-IN_DescripcionNC ~
COMBO-BOX_NroSerND FILL-IN_CodCtaND FILL-IN_DescripcionND 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-nc-dcto-por-vol-doc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-nc-dcto-por-vol-mat AS HANDLE NO-UNDO.
DEFINE VARIABLE h_tab95 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON_Borrar 
     LABEL "BORRAR DATOS" 
     SIZE 19 BY 1.12
     FONT 6.

DEFINE BUTTON BUTTON_Calcular 
     LABEL "CALCULAR" 
     SIZE 15 BY 1.12
     FONT 6.

DEFINE BUTTON BUTTON_Genera 
     LABEL "GENERAR NOTA DE CREDITO Y DEBITO" 
     SIZE 38 BY 1.12
     FONT 6.

DEFINE VARIABLE COMBO-BOX_CodDiv AS CHARACTER FORMAT "X(256)":U 
     LABEL "Seleccione Configuraci�n" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 90 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE COMBO-BOX_NroSerNC AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Nro. de Serie N/C" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX_NroSerND AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Nro. de Serie N/D" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_CodCli AS CHARACTER FORMAT "X(15)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN_CodCtaNC AS CHARACTER FORMAT "X(256)":U 
     LABEL "Concepto" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_CodCtaND AS CHARACTER FORMAT "X(256)":U 
     LABEL "Concepto" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_DescripcionNC AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_DescripcionND AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 71 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_NomCli AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 75 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 128 BY 2.42
     BGCOLOR 15 FGCOLOR 0 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 128 BY 6.19
     BGCOLOR 11 FGCOLOR 0 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN_CodCli AT ROW 1.54 COL 29 COLON-ALIGNED WIDGET-ID 2
     FILL-IN_NomCli AT ROW 1.54 COL 44 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     COMBO-BOX_CodDiv AT ROW 2.62 COL 29 COLON-ALIGNED WIDGET-ID 8
     BUTTON_Calcular AT ROW 3.96 COL 7 WIDGET-ID 6
     BUTTON_Borrar AT ROW 3.96 COL 23 WIDGET-ID 14
     BUTTON_Genera AT ROW 3.96 COL 70 WIDGET-ID 24
     COMBO-BOX_NroSerNC AT ROW 5.31 COL 21 COLON-ALIGNED WIDGET-ID 16
     FILL-IN_CodCtaNC AT ROW 6.38 COL 21 COLON-ALIGNED WIDGET-ID 28
     FILL-IN_DescripcionNC AT ROW 6.38 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 30
     COMBO-BOX_NroSerND AT ROW 7.46 COL 21 COLON-ALIGNED WIDGET-ID 32
     FILL-IN_CodCtaND AT ROW 8.54 COL 21 COLON-ALIGNED WIDGET-ID 34
     FILL-IN_DescripcionND AT ROW 8.54 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     "Datos:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1 COL 3 WIDGET-ID 12
          BGCOLOR 9 FGCOLOR 15 FONT 6
     RECT-1 AT ROW 1.27 COL 2 WIDGET-ID 10
     RECT-2 AT ROW 3.69 COL 2 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 130.72 BY 25.58 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: b-ccbcdocu B "?" ? INTEGRAL CcbCDocu
      TABLE: CREDITO B "?" ? INTEGRAL CcbCDocu
      TABLE: T-CCOTI T "?" NO-UNDO INTEGRAL FacCPedi
      TABLE: T-CDOCU T "?" NO-UNDO INTEGRAL CcbCDocu
      TABLE: T-DCOTI T "?" NO-UNDO INTEGRAL FacDPedi
      TABLE: T-DDOCU T "?" NO-UNDO INTEGRAL CcbDDocu
      ADDITIONAL-FIELDS:
          INDEX Idx00 AS PRIMARY CodMat
      END-FIELDS.
      TABLE: T-FELogErrores T "?" NO-UNDO INTEGRAL FELogErrores
      TABLE: ttCcbcdocu T "?" NO-UNDO INTEGRAL CcbCDocu
      TABLE: ttCcbddocu T "?" NO-UNDO INTEGRAL CcbDDocu
      TABLE: txCcbCDocu T "?" NO-UNDO INTEGRAL CcbCDocu
      TABLE: txCcbDDocu T "?" NO-UNDO INTEGRAL CcbDDocu
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "LIQUIDACION NOTAS DE CREDITO POR DESCUENTO POR VOLUMEN"
         HEIGHT             = 25.58
         WIDTH              = 130.72
         MAX-HEIGHT         = 25.58
         MAX-WIDTH          = 178.72
         VIRTUAL-HEIGHT     = 25.58
         VIRTUAL-WIDTH      = 178.72
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
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON BUTTON_Borrar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON_Genera IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_CodCtaNC IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_CodCtaND IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DescripcionNC IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_DescripcionND IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_NomCli IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* LIQUIDACION NOTAS DE CREDITO POR DESCUENTO POR VOLUMEN */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* LIQUIDACION NOTAS DE CREDITO POR DESCUENTO POR VOLUMEN */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON_Borrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON_Borrar W-Win
ON CHOOSE OF BUTTON_Borrar IN FRAME F-Main /* BORRAR DATOS */
DO:
  DO WITH WITH FRAME {&FRAME-NAME}:
      COMBO-BOX_CodDiv = ''.
      FILL-IN_CodCli = ''.
      FILL-IN_NomCli = ''.

      DISPLAY COMBO-BOX_CodDiv FILL-IN_CodCli FILL-IN_NomCli .
      ENABLE FILL-IN_CodCli COMBO-BOX_CodDiv BUTTON_Calcular.
      DISABLE BUTTON_Borrar BUTTON_Genera.
      EMPTY TEMP-TABLE T-DDOCU.
      EMPTY TEMP-TABLE T-CDOCU.
      IF h_b-nc-dcto-por-vol-mat <> ? THEN RUN Captura-Temporal IN h_b-nc-dcto-por-vol-mat
        ( INPUT TABLE T-DDOCU).
      IF h_b-nc-dcto-por-vol-doc <> ? THEN RUN Captura-Temporal IN h_b-nc-dcto-por-vol-doc
        ( INPUT TABLE T-CDOCU).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON_Calcular
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON_Calcular W-Win
ON CHOOSE OF BUTTON_Calcular IN FRAME F-Main /* CALCULAR */
DO:
  ASSIGN FILL-IN_CodCli COMBO-BOX_CodDiv.

  /* Validaci�n */
  IF NOT CAN-FIND(FIRST gn-clie WHERE gn-clie.codcia = cl-codcia
                  AND gn-clie.codcli = FILL-IN_CodCli NO-LOCK)
      THEN DO:
      MESSAGE 'Cliente NO registrado' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO FILL-IN_CodCli.
      RETURN NO-APPLY.
  END.
  IF TRUE <> (COMBO-BOX_CodDiv > '') THEN DO:
      MESSAGE 'Seleccione una configuraci�n' VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO COMBO-BOX_CodDiv.
      RETURN NO-APPLY.
  END.

  DISABLE FILL-IN_CodCli COMBO-BOX_CodDiv BUTTON_Calcular WITH FRAME {&FRAME-NAME}.

  SESSION:SET-WAIT-STATE('GENERAL').
  RUN Calcular (INPUT FILL-IN_CodCli, INPUT COMBO-BOX_CodDiv).
  SESSION:SET-WAIT-STATE('').


  /* Pasamos informaci�n a los browses */
  IF h_b-nc-dcto-por-vol-mat <> ? THEN RUN Captura-Temporal IN h_b-nc-dcto-por-vol-mat
    ( INPUT TABLE T-DDOCU).
  IF h_b-nc-dcto-por-vol-doc <> ? THEN RUN Captura-Temporal IN h_b-nc-dcto-por-vol-doc
    ( INPUT TABLE T-CDOCU).

  ENABLE BUTTON_Borrar BUTTON_Genera WITH FRAME {&FRAME-NAME}.

  MESSAGE 'Proceso Terminado' VIEW-AS ALERT-BOX INFORMATION.

  IF NOT CAN-FIND(FIRST T-DDOCU NO-LOCK) THEN DO:
      MESSAGE 'NO hay registros que procesar' VIEW-AS ALERT-BOX INFORMATION.
      APPLY 'CHOOSE':U TO BUTTON_Borrar.
      RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON_Genera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON_Genera W-Win
ON CHOOSE OF BUTTON_Genera IN FRAME F-Main /* GENERAR NOTA DE CREDITO Y DEBITO */
DO:
  ASSIGN
       COMBO-BOX_CodDiv COMBO-BOX_NroSerNC COMBO-BOX_NroSerND FILL-IN_CodCli.
  DEF VAR pMensaje AS CHAR NO-UNDO.
  DEF VAR pMsgSUnat AS CHAR NO-UNDO.

  RUN MASTER-TRANSACTION (OUTPUT pMensaje, OUTPUT pMsgSunat).
  IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
      IF TRUE <> (pMensaje > '') THEN pMensaje = "Hubo problemas para genrar las N/C y/o N/D".
      IF pMensaje > '' THEN MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.
  END.
  IF pMsgSunat > '' THEN MESSAGE pMsgSunat VIEW-AS ALERT-BOX WARNING.

/*   RUN Genera-NC-MASTER (OUTPUT pMensaje, OUTPUT pMsgSunat).             */
/*   IF RETURN-VALUE = 'ADM-ERROR' THEN DO:                                */
/*       IF pMensaje > '' THEN MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.   */
/*       IF pMsgSUnat > '' THEN MESSAGE pMsgSunat VIEW-AS ALERT-BOX ERROR. */
/*       RETURN NO-APPLY.                                                  */
/*   END.                                                                  */
/*   RUN Genera-ND-MASTER (OUTPUT pMensaje, OUTPUT pMsgSunat).             */
/*   IF RETURN-VALUE = 'ADM-ERROR' THEN DO:                                */
/*       IF pMensaje > '' THEN MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.   */
/*       IF pMsgSUnat > '' THEN MESSAGE pMsgSunat VIEW-AS ALERT-BOX ERROR. */
/*       RETURN NO-APPLY.                                                  */
/*   END.                                                                  */

  APPLY "CHOOSE":U TO BUTTON_Borrar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_CodCli
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_CodCli W-Win
ON LEAVE OF FILL-IN_CodCli IN FRAME F-Main /* Cliente */
DO:
  FILL-IN_NomCli:SCREEN-VALUE = ''.
  FIND gn-clie WHERE gn-clie.codcia = cl-codcia
      AND gn-clie.codcli = SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE gn-clie THEN FILL-IN_NomCli:SCREEN-VALUE = gn-clie.nomcli.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_CodCli W-Win
ON LEFT-MOUSE-DBLCLICK OF FILL-IN_CodCli IN FRAME F-Main /* Cliente */
OR F8 OF FILL-IN_CodCli
DO:
    ASSIGN
        input-var-1 = ''
        input-var-2 = ''
        input-var-3 = ''
        output-var-1 = ?.

    RUN lkup/c-client.w ('CLIENTES').
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
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-free/objects/tab95.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'LABEL-FONT = 4,
                     LABEL-FGCOLOR = 0,
                     FOLDER-BGCOLOR = 8,
                     FOLDER-PARENT-BGCOLOR = 8,
                     LABELS = Art�culos|Comprobantes':U ,
             OUTPUT h_tab95 ).
       RUN set-position IN h_tab95 ( 9.88 , 2.00 ) NO-ERROR.
       RUN set-size IN h_tab95 ( 16.15 , 128.00 ) NO-ERROR.

       /* Links to SmartTab95 h_tab95. */
       RUN add-link IN adm-broker-hdl ( h_tab95 , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_tab95 ,
             FILL-IN_DescripcionND:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/ccb/b-nc-dcto-por-vol-mat.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-nc-dcto-por-vol-mat ).
       RUN set-position IN h_b-nc-dcto-por-vol-mat ( 10.96 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-nc-dcto-por-vol-mat ( 14.73 , 122.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-nc-dcto-por-vol-mat ,
             h_tab95 , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/ccb/b-nc-dcto-por-vol-doc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-nc-dcto-por-vol-doc ).
       RUN set-position IN h_b-nc-dcto-por-vol-doc ( 10.96 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-nc-dcto-por-vol-doc ( 14.54 , 105.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-nc-dcto-por-vol-doc ,
             h_tab95 , 'AFTER':U ).
    END. /* Page 2 */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calcular W-Win 
PROCEDURE Calcular :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER pCodCli AS CHAR.
DEF INPUT PARAMETER pInternalId AS CHAR.

DEF VAR pInicio AS DATE NO-UNDO.
DEF VAR pFin AS DATE NO-UNDO.

DEF VAR fFactor AS DECI NO-UNDO.

DEF BUFFER COTIZACION FOR Faccpedi.         
DEF BUFFER PEDIDO FOR Faccpedi.    
DEF BUFFER CREDITO FOR Ccbcdocu.

EMPTY TEMP-TABLE T-CCOTI.
EMPTY TEMP-TABLE T-DCOTI.
EMPTY TEMP-TABLE T-DDOCU.
EMPTY TEMP-TABLE T-CDOCU.

DEF VAR pOk AS LOG NO-UNDO.

/* ******************************************************************************* */
/* Barremos toda la tabla de configuraci�n */
/* ******************************************************************************* */
DEF VAR x-Clientes AS CHAR NO-UNDO.
DEF VAR x-Agrupador AS CHAR NO-UNDO.
DEF VAR k AS INTE NO-UNDO.
DEF VAR x-CodCli AS CHAR NO-UNDO.

x-Clientes = pCodCli.       /* Valor por defecto */

/* Rastreamos el cliente agrupador */
FIND FIRST VtaCTabla WHERE VtaCTabla.CodCia = s-codcia AND
    VtaCTabla.Tabla = "CLGRP" AND
    VtaCTabla.Llave = pCodCli NO-LOCK NO-ERROR.
IF AVAILABLE VtaCTabla THEN x-Agrupador = VtaCTabla.Llave.
ELSE DO:
    FOR EACH Vtadtabla NO-LOCK WHERE VtaDTabla.CodCia = s-codcia AND
        VtaDTabla.Tabla = "CLGRP" AND
        VtaDTabla.Tipo = pCodCli,
        FIRST VtaCTabla OF VtaDTabla NO-LOCK:
        x-Agrupador = VtaCTabla.Llave.
        LEAVE.
    END.
END.
IF x-Agrupador > '' THEN DO:
    IF LOOKUP(x-Agrupador, x-Clientes) = 0 THEN x-Clientes = x-Clientes + ',' + x-Agrupador.
    FOR EACH VtaDTabla NO-LOCK WHERE VtaDTabla.CodCia = s-codcia AND
        VtaDTabla.Tabla = "CLGRP" AND
        VtaDTabla.Llave = x-Agrupador:
        IF LOOKUP(VtaDTabla.Tipo, x-Clientes) = 0 THEN x-Clientes = x-Clientes + ',' + VtaDTabla.Tipo.
    END.
END.
IF TRUE <> (x-Agrupador > '') THEN x-Agrupador = pCodCli.

DEF VAR x-Nro-Control AS CHAR NO-UNDO.
FOR EACH PriDtoVolAcuCab NO-LOCK WHERE PriDtoVolAcuCab.CodCia = s-codcia AND PriDtoVolAcuCab.Internal_Id = pInternalId:
    pInicio = PriDtoVolAcuCab.Inicio.
    pFin = PriDtoVolAcuCab.Fin.
    /* Barremos las cotizaciones del cliente */
    /* Barremos por cada cliente */
    DO k = 1 TO NUM-ENTRIES(x-Clientes):
        x-CodCli = ENTRY(k, x-Clientes).
        FOR EACH COTIZACION NO-LOCK WHERE COTIZACION.codcia = s-codcia
            AND COTIZACION.codcli = x-CodCli
            AND COTIZACION.coddoc = "COT"
            AND COTIZACION.FchPed >= pInicio
            AND COTIZACION.FchPed <= pFin
            AND COTIZACION.Lista_de_Precios = PriDtoVolAcuCab.CodDiv:
            /* ****************************************************************** */
            /* Definimos un # de control �nico de acuerdo a la condici�n de venta */
            /*x-Nro-Control = COTIZACION.CodCli + "-" + COTIZACION.FmaPgo.*/
            x-Nro-Control = x-Agrupador + "-" + COTIZACION.FmaPgo.
            /* ****************************************************************** */
            IF NOT CAN-FIND(FIRST T-CCOTI WHERE T-CCOTI.nroped = x-Nro-Control NO-LOCK) THEN DO:
                CREATE T-CCOTI.
                BUFFER-COPY COTIZACION TO T-CCOTI
                    ASSIGN T-CCOTI.nroped = x-Nro-Control.
            END.
            /* Por cada cotizaci�n barremos sus pedidos log�sticos */
            FOR EACH PEDIDO NO-LOCK WHERE PEDIDO.codcia = s-codcia
                AND PEDIDO.codref = COTIZACION.coddoc
                AND PEDIDO.nroref = COTIZACION.nroped
                AND PEDIDO.coddoc = "PED":
                /* Por cada Pedido Log�stico buscamos sus facturas */
                FOR EACH Ccbcdocu NO-LOCK WHERE Ccbcdocu.codcia = s-codcia
                    AND Ccbcdocu.codped = PEDIDO.coddoc
                    AND Ccbcdocu.nroped = PEDIDO.nroped 
                    AND LOOKUP(Ccbcdocu.coddoc, 'FAC,BOL') > 0
                    AND Ccbcdocu.flgest <> "A"
                    AND Ccbcdocu.flgcie <> "C":     /* OJO */
                    fFactor = 1.
                    pOk = NO.
                    FOR EACH Ccbddocu OF Ccbcdocu NO-LOCK,
                        FIRST Almmmatg OF ccbddocu NO-LOCK,
                        FIRST Facdpedi OF COTIZACION NO-LOCK WHERE Facdpedi.codmat = Ccbddocu.codmat,
                        FIRST PriDtoVolAcuDet OF PriDtoVolAcuCab NO-LOCK WHERE PriDtoVolAcuDet.CodMat = Ccbddocu.codmat:
                        /* Detalle por Cotizacion */
                        FIND T-DCOTI WHERE T-DCOTI.CodCia = FacDPedi.CodCia 
                            AND T-DCOTI.CodDiv = FacDPedi.CodDiv 
                            AND T-DCOTI.CodDoc = FacDPedi.CodDoc 
                            AND T-DCOTI.NroPed = x-Nro-Control
                            AND T-DCOTI.codmat = Facdpedi.codmat NO-ERROR.
                        IF NOT AVAILABLE T-DCOTI THEN DO:
                            CREATE T-DCOTI.
                            ASSIGN
                                T-DCOTI.CodCia = FacDPedi.CodCia 
                                T-DCOTI.CodDiv = FacDPedi.CodDiv 
                                T-DCOTI.CodDoc = FacDPedi.CodDoc 
                                T-DCOTI.NroPed = x-Nro-Control
                                T-DCOTI.CodMat = FacDPedi.codmat
                                T-DCOTI.UndVta = Almmmatg.CHR__01
                                T-DCOTI.AftIgv = FacDPedi.AftIgv
                                T-DCOTI.Factor = 1.
                        END.
                        ASSIGN
                            T-DCOTI.CanPed = T-DCOTI.CanPed + (Facdpedi.canped * Facdpedi.factor) * fFactor.
                        /* Resumen por Comprobante */
                        FIND FIRST T-DDOCU WHERE T-DDOCU.codmat = Ccbddocu.codmat NO-ERROR.
                        IF NOT AVAILABLE T-DDOCU THEN CREATE T-DDOCU.
                        ASSIGN
                            T-DDOCU.codcia = FacDPedi.CodCia 
                            T-DDOCU.codmat = FacDPedi.codmat
                            T-DDOCU.UndVta = Almmmatg.CHR__01
                            T-DDOCU.AftIgv = FacDPedi.AftIgv
                            T-DDOCU.Factor = 1
                            T-DDOCU.candes = T-DDOCU.candes + (Ccbddocu.candes * Ccbddocu.factor) * fFactor
                            T-DDOCU.implin = T-DDOCU.implin + ( (Ccbddocu.implin - Ccbddocu.impdto2) * fFactor).
                        pOk = YES.
                    END.
                    IF pOk THEN DO:
                        IF NOT CAN-FIND(FIRST T-CDOCU OF Ccbcdocu NO-LOCK) THEN DO:
                            CREATE T-CDOCU.
                            BUFFER-COPY Ccbcdocu TO T-CDOCU.
                        END.
                    END.
                    /* Buscamos si la Factur tiene N/C por devoluci�n de mercader�a */
                    FOR EACH CREDITO NO-LOCK WHERE CREDITO.codcia = s-codcia
                        AND CREDITO.coddoc = "N/C"
                        AND CREDITO.codref = Ccbcdocu.coddoc
                        AND CREDITO.nroref = Ccbcdocu.nrodoc
                        AND CREDITO.CndCre = "D"
                        AND CREDITO.flgest <> "A"
                        AND CREDITO.flgcie <> "C":
                        fFactor = -1.
                        pOk = NO.
                        FOR EACH Ccbddocu OF CREDITO NO-LOCK,
                            FIRST Almmmatg OF Ccbddocu NO-LOCK,
                            FIRST Facdpedi OF COTIZACION NO-LOCK WHERE Facdpedi.codmat = Ccbddocu.codmat:
                            /* Detalle por Cotizacion */
                            FIND T-DCOTI WHERE T-DCOTI.CodCia = FacDPedi.CodCia 
                                AND T-DCOTI.CodDiv = FacDPedi.CodDiv 
                                AND T-DCOTI.CodDoc = FacDPedi.CodDoc 
                                AND T-DCOTI.NroPed = x-Nro-Control
                                AND T-DCOTI.codmat = Facdpedi.codmat NO-ERROR.
                            IF NOT AVAILABLE T-DCOTI THEN DO:
                                CREATE T-DCOTI.
                                ASSIGN
                                    T-DCOTI.CodCia = FacDPedi.CodCia 
                                    T-DCOTI.CodDiv = FacDPedi.CodDiv 
                                    T-DCOTI.CodDoc = FacDPedi.CodDoc 
                                    T-DCOTI.NroPed = x-Nro-Control
                                    T-DCOTI.CodMat = FacDPedi.codmat
                                    T-DCOTI.UndVta = Almmmatg.CHR__01
                                    T-DCOTI.AftIgv = FacDPedi.AftIgv
                                    T-DCOTI.Factor = 1.
                            END.
                            ASSIGN
                                T-DCOTI.CanPed = T-DCOTI.CanPed + (Facdpedi.canped * Facdpedi.factor) * fFactor.
                            /* Resumen */
                            FIND FIRST T-DDOCU WHERE T-DDOCU.codmat = Ccbddocu.codmat NO-ERROR.
                            IF NOT AVAILABLE T-DDOCU THEN CREATE T-DDOCU.
                            ASSIGN
                                T-DDOCU.codcia = FacDPedi.CodCia 
                                T-DDOCU.codmat = FacDPedi.codmat
                                T-DDOCU.UndVta = Almmmatg.CHR__01
                                T-DDOCU.AftIgv = FacDPedi.AftIgv
                                T-DDOCU.Factor = 1
                                T-DDOCU.candes = T-DDOCU.candes + (Ccbddocu.candes * Ccbddocu.factor) * fFactor
                                T-DDOCU.implin = T-DDOCU.implin + ( (Ccbddocu.implin - Ccbddocu.impdto2) * fFactor).
                            pOk = YES.
                        END.
                        IF pOk THEN DO:
                            IF NOT CAN-FIND(FIRST T-CDOCU OF Ccbcdocu NO-LOCK) THEN DO:
                                CREATE T-CDOCU.
                                BUFFER-COPY Ccbcdocu TO T-CDOCU.
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.
END.

/* ******************************************************************************* */
/* Calculamos el importe */
/* ******************************************************************************* */
DEF VAR s-UndVta AS CHAR NO-UNDO.
DEF VAR f-Factor AS DECI NO-UNDO.
DEF VAR F-PREBAS AS DECI NO-UNDO.
DEF VAR F-PREVTA AS DECI NO-UNDO.
DEF VAR F-DSCTOS AS DECI NO-UNDO.
DEF VAR Y-DSCTOS AS DECI NO-UNDO.
DEF VAR Z-DSCTOS AS DECI NO-UNDO.
DEF VAR X-TIPDTO AS CHAR NO-UNDO.
DEF VAR f-FleteUnitario AS DECI NO-UNDO.
DEF VAR s-NroDec AS INTE NO-UNDO.
DEF VAR s-PorIgv AS DECI NO-UNDO.
DEF VAR s-CodCli AS CHAR NO-UNDO.
DEF VAR s-Cmpbnte AS CHAR NO-UNDO.

FOR EACH T-CCOTI NO-LOCK:
    FOR EACH T-DCOTI OF T-CCOTI, 
        FIRST Almmmatg OF T-DCOTI NO-LOCK, FIRST Almtfami OF Almmmatg NO-LOCK, FIRST Almsfami OF Almmmatg NO-LOCK:
        s-NroDec = T-CCOTI.Libre_d01.
        s-PorIgv = T-CCOTI.PorIgv.
        s-CodCli = T-CCOTI.CodCli.
        s-Cmpbnte = T-CCOTI.Cmpbnte.
        s-UndVta = T-DCOTI.UndVta.
        F-FACTOR = T-DCOTI.Factor.
        RUN {&precio-venta-general} (T-CCOTI.TpoPed,
                                     T-CCOTI.Lista_de_Precio,
                                     T-CCOTI.CodCli,
                                     T-CCOTI.CodMon,
                                     INPUT-OUTPUT s-UndVta,
                                     OUTPUT f-Factor,
                                     T-DCOTI.CodMat,
                                     T-CCOTI.FmaPgo,
                                     T-DCOTI.CanPed,
                                     s-NroDec,
                                     OUTPUT F-PREBAS,
                                     OUTPUT F-PREVTA,
                                     OUTPUT F-DSCTOS,
                                     OUTPUT Y-DSCTOS,
                                     OUTPUT Z-DSCTOS,
                                     OUTPUT X-TIPDTO,
                                     "",     /* ClfCli: lo ingresamos solo si se quiere forzar la clasificacion */
                                     OUTPUT f-FleteUnitario,
                                     "",
                                     NO
                                     ).
        IF X-TIPDTO <> "VOL" THEN DO:
            /* NO afecto a descuento por volumen por escala */
            DELETE T-DCOTI.
            NEXT.
        END.
        /* Grabamos los importes aplicando el descuento por Volumen por Escalas */
        ASSIGN 
            T-DCOTI.Factor = f-Factor
            T-DCOTI.UndVta = s-UndVta
            T-DCOTI.PreUni = F-PREVTA
            T-DCOTI.Libre_d02 = f-FleteUnitario    /* Flete Unitario */
            T-DCOTI.PreBas = F-PreBas 
            T-DCOTI.PreVta[1] = F-PreVta   /* CONTROL DE PRECIO DE LISTA */
            T-DCOTI.PorDto = F-DSCTOS      /* Ambos descuentos afectan */
            T-DCOTI.PorDto2 = 0            /* el precio unitario */
            T-DCOTI.Por_Dsctos[2] = z-Dsctos
            T-DCOTI.Por_Dsctos[3] = Y-DSCTOS 
            T-DCOTI.AftIgv = Almmmatg.AftIgv
            T-DCOTI.AftIsc = Almmmatg.AftIsc
            T-DCOTI.ImpIsc = 0
            T-DCOTI.ImpIgv = 0
            T-DCOTI.Libre_c04 = x-TipDto.
        /* ***************************************************************** */
        {vtagn/CalculoDetalleMayorCredito.i &Tabla="T-DCOTI" }
        /* ***************************************************************** */
        IF t-dcoti.codmat = '059091' THEN DO:
            MESSAGE t-dcoti.nroped t-dcoti.codmat t-dcoti.canped t-dcoti.undvta f-prevta SKIP
                t-dcoti.canped t-dcoti.preuni t-dcoti.por_dsctos[1] t-dcoti.por_dsctos[2] t-dcoti.por_dsctos[3] t-dcoti.implin.
        END.
    END.
END.
/* ******************************************************************************* */
/* Acumulamos los descuentos te�ricos y lo grabamos al comparativo */
/* ******************************************************************************* */
DEF VAR x-ImpDto AS DECI INIT 0 NO-UNDO.
FOR EACH T-DDOCU:
    IF NOT CAN-FIND(FIRST T-DCOTI WHERE T-DCOTI.codmat = T-DDOCU.codmat NO-LOCK) THEN DO:
        /* Solo los que est�n afectos a descuentos por volumen por escala */
        DELETE T-DDOCU.
        NEXT.
    END.
    T-DDOCU.ImpPro = 0.
    FOR EACH T-DCOTI NO-LOCK WHERE T-DCOTI.codmat = T-DDOCU.codmat:
        T-DDOCU.ImpPro = T-DDOCU.ImpPro + T-DCOTI.ImpLin.
    END.
    T-DDOCU.ImpDto = T-DDOCU.ImpLin - T-DDOCU.ImpPro.
    IF T-DDOCU.ImpDto = 0 THEN DO:
        DELETE T-DDOCU.
        NEXT.
    END.
    x-ImpDto = x-ImpDto + T-DDOCU.ImpDto.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Items W-Win 
PROCEDURE Carga-Items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*
DEFINE VAR x-estado-item AS CHAR.
DEFINE VAR x-item AS INT INIT 0.
DEFINE VAR x-ImpBrt AS DEC.
DEFINE VAR x-ImpExo AS DEC.
DEFINE VAR x-ImpIgv AS DEC.
DEFINE VAR x-ImpTot AS DEC.

EMPTY TEMP-TABLE ttCcbddocu.

/* Todos los valores en positivo */
FOR EACH T-DDOCU EXCLUSIVE-LOCK:
    T-DDOCU.impdto = ABSOLUTE(T-DDOCU.impdto).
END.

FOR EACH T-DDOCU NO-LOCK:
    x-item = x-item + 1.
    CREATE ttCcbddocu.
    ASSIGN
        ttCcbddocu.codcia = s-codcia
        ttCcbddocu.coddiv = s-coddiv
        ttCcbddocu.coddoc = x-coddoc
        ttCcbddocu.nroitm = x-item
        ttCcbddocu.codmat = T-DDOCU.codmat
        ttCcbddocu.aftigv = T-DDOCU.aftigv
        ttCcbddocu.candes = T-DDOCU.candes
        ttCcbddocu.preuni = T-DDOCU.impdto / T-DDOCU.candes
        ttCcbddocu.implin = T-DDOCU.impdto
        ttCcbddocu.undvta = T-DDOCU.undvta
        ttCcbddocu.factor = T-DDOCU.factor
        ttCcbddocu.impigv = (IF T-DDOCU.AftIgv THEN (T-DDOCU.impdto / (1 + ttCcbcdocu.PorIgv)) ELSE 0)
        .
    IF T-DDOCU.AftIgv = YES THEN x-ImpBrt = x-ImpBrt + ttCcbddocu.implin.
    IF T-DDOCU.AftIgv = NO THEN x-ImpExo = x-ImpExo + ttCcbddocu.implin.
    x-ImpIgv = x-ImpIgv + ttCcbddocu.ImpIgv.
    x-ImpTot = x-ImpTot + ttCcbddocu.ImpLin.
END.
ASSIGN  
    ttCcbcdocu.ImpBrt = x-impbrt
    ttCcbcdocu.ImpExo = x-impexo
    ttCcbcdocu.ImpIgv = x-ImpIgv
    ttCcbcdocu.ImpTot = x-ImpTot
    ttCcbcdocu.ImpVta = ttCcbcdocu.ImpBrt - ttCcbcdocu.ImpIgv
    ttCcbcdocu.ImpBrt = ttCcbcdocu.ImpBrt - ttCcbcdocu.ImpIgv
    ttCcbcdocu.SdoAct = ttCcbcdocu.ImpTot.
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal-NC W-Win 
PROCEDURE Carga-Temporal-NC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Limpiamos tablas receptoras */
EMPTY TEMP-TABLE txCcbcdocu.
EMPTY TEMP-TABLE txCcbddocu.
/* ******************************************************************************************* */
/* Por cada comprobante repartimos los art�culos */
/* Comprobante de mayor importe al de menor importe */
/* ******************************************************************************************* */
FOR EACH T-CDOCU BY T-CDOCU.ImpTot DESC:
    /* Por cada comprobante barremos el acumulado con T-DDOCU.ImpDto < 0 */
    FOR EACH T-DDOCU WHERE T-DDOCU.ImpDto > 0:
        /* Buscamos si est� registrado en el comprobante */
        FIND FIRST Ccbddocu OF T-CDOCU WHERE Ccbddocu.codmat = T-DDOCU.codmat NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Ccbddocu OR ABS(T-DDOCU.ImpDto) > Ccbddocu.ImpLin THEN NEXT.
        /* Creamos un registro */
        CREATE txCcbddocu.
        BUFFER-COPY T-DDOCU TO txCcbddocu
            ASSIGN 
            txCcbddocu.CodDiv = s-CodDiv
            txCcbddocu.CodDoc = T-CDOCU.CodDoc     /* Referenciamos la FAC */
            txCcbddocu.NroDoc = T-CDOCU.NroDoc.
        /* Borramos lo ya asignado */
        DELETE T-DDOCU.
    END.
END.
FOR EACH txCcbddocu, 
    FIRST T-CDOCU WHERE T-CDOCU.coddoc = txCcbddocu.coddoc AND T-CDOCU.nrodoc = txCcbddocu.nrodoc
    BREAK BY txCcbddocu.NroDoc:
    IF FIRST-OF(txCcbddocu.NroDoc) THEN DO:
        CREATE txCcbcdocu.
        BUFFER-COPY T-CDOCU TO txCcbcdocu
            ASSIGN txCcbcdocu.coddiv = s-CodDiv.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Temporal-ND W-Win 
PROCEDURE Carga-Temporal-ND :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Limpiamos tablas receptoras */
EMPTY TEMP-TABLE txCcbcdocu.
EMPTY TEMP-TABLE txCcbddocu.
/* ******************************************************************************************* */
/* Por cada comprobante repartimos los art�culos */
/* Comprobante de mayor importe al de menor importe */
/* ******************************************************************************************* */
FOR EACH T-CDOCU BY T-CDOCU.ImpTot DESC:
    /* Por cada comprobante barremos el acumulado con T-DDOCU.ImpDto > 0 */
    FOR EACH T-DDOCU WHERE T-DDOCU.ImpDto < 0:
        /* Buscamos si est� registrado en el comprobante */
        FIND FIRST Ccbddocu OF T-CDOCU WHERE Ccbddocu.codmat = T-DDOCU.codmat NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Ccbddocu OR ABS(T-DDOCU.ImpDto) > Ccbddocu.ImpLin THEN NEXT.
        /* Creamos un registro */
        CREATE txCcbddocu.
        BUFFER-COPY T-DDOCU TO txCcbddocu
            ASSIGN 
            txCcbddocu.CodDiv = s-CodDiv
            txCcbddocu.CodDoc = T-CDOCU.CodDoc     /* Referenciamos la FAC */
            txCcbddocu.NroDoc = T-CDOCU.NroDoc.
        /* Borramos lo ya asignado */
        DELETE T-DDOCU.
    END.
END.
FOR EACH txCcbddocu, 
    FIRST T-CDOCU WHERE T-CDOCU.coddoc = txCcbddocu.coddoc AND T-CDOCU.nrodoc = txCcbddocu.nrodoc
    BREAK BY txCcbddocu.NroDoc:
    IF FIRST-OF(txCcbddocu.NroDoc) THEN DO:
        CREATE txCcbcdocu.
        BUFFER-COPY T-CDOCU TO txCcbcdocu
            ASSIGN txCcbcdocu.coddiv = s-CodDiv.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cierra-Comprobante-Origen W-Win 
PROCEDURE Cierra-Comprobante-Origen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    /* Actualizamos el estado de todas las facturas relacionadas */
    FOR EACH T-CDOCU NO-LOCK:
        FIND FIRST Ccbcdocu OF T-CDOCU EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF ERROR-STATUS:ERROR = YES THEN DO:
            pMensaje = "ERROR al actualizar el comprobante " + T-CDOCU.coddoc + " " + T-CDOCU.nrodoc + " (" + ERROR-STATUS:GET-MESSAGE(1) + ")".
            UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
        END.
        ASSIGN
            CcbCDocu.FchCie = TODAY
            CcbCDocu.FlgCie = "C".      /* OJO */
        FOR EACH CREDITO WHERE CREDITO.codcia = s-codcia
            AND CREDITO.coddoc = "N/C"
            AND CREDITO.codref = Ccbcdocu.coddoc
            AND CREDITO.nroref = Ccbcdocu.nrodoc
            AND CREDITO.CndCre = "D"
            AND CREDITO.flgest <> "A"
            AND CREDITO.flgcie <> "C" EXCLUSIVE-LOCK ON ERROR UNDO, THROW:
            ASSIGN
                CREDITO.FchCie = TODAY
                CREDITO.FlgCie = "C".      /* OJO */
        END.
    END.
END.
IF AVAILABLE(Ccbcdocu) THEN RELEASE Ccbcdocu.
IF AVAILABLE(CREDITO)  THEN RELEASE CREDITO.
RETURN 'OK'.

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
  DISPLAY FILL-IN_CodCli FILL-IN_NomCli COMBO-BOX_CodDiv COMBO-BOX_NroSerNC 
          FILL-IN_CodCtaNC FILL-IN_DescripcionNC COMBO-BOX_NroSerND 
          FILL-IN_CodCtaND FILL-IN_DescripcionND 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-1 RECT-2 FILL-IN_CodCli COMBO-BOX_CodDiv BUTTON_Calcular 
         COMBO-BOX_NroSerNC COMBO-BOX_NroSerND 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FIRST-TRANSACTION W-Win 
PROCEDURE FIRST-TRANSACTION :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER x-CodDoc AS CHAR.       /* N/C */
DEF INPUT PARAMETER pNroSer AS INTE.
DEF INPUT PARAMETER pCodCta AS CHAR.    
DEF INPUT PARAMETER pCodRef AS CHAR.        /* FAC */
DEF INPUT PARAMETER pNroRef AS CHAR.

DEF OUTPUT PARAMETER pNroDoc AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEFINE VAR x-Serie  AS INT NO-UNDO.
DEFINE VAR x-Numero AS INT NO-UNDO.
DEFINE VAR x-NroDoc AS CHAR NO-UNDO.
DEFINE VAR x-Item   AS INTE NO-UNDO.
DEFINE VAR x-ImpBrt AS DECI NO-UNDO.
DEFINE VAR x-ImpExo AS DECI NO-UNDO.
DEFINE VAR x-ImpIgv AS DECI NO-UNDO.
DEFINE VAR x-ImpTot AS DECI NO-UNDO.

DEFINE BUFFER b-ccbcdocu FOR ccbcdocu.
DEFINE BUFFER b-ccbddocu FOR ccbddocu.
DEFINE BUFFER CREDITO FOR ccbcdocu.

GRABAR_DOCUMENTO:
DO TRANSACTION ON ERROR UNDO GRABAR_DOCUMENTO, LEAVE GRABAR_DOCUMENTO ON STOP UNDO GRABAR_DOCUMENTO, LEAVE GRABAR_DOCUMENTO:
    /* Nos posicionamos en la FAC */
    FIND FIRST Ccbcdocu WHERE Ccbcdocu.codcia = s-codcia AND
        Ccbcdocu.coddoc = pCodRef AND
        Ccbcdocu.nrodoc = pNroRef NO-LOCK.
    /* El n�mero de serie */ 
    FIND FIRST Faccorre WHERE  Faccorre.CodCia = S-CODCIA 
        AND Faccorre.CodDoc = x-CodDoc
        AND Faccorre.CodDiv = S-CODDIV 
        AND Faccorre.NroSer = pNroSer
        AND Faccorre.FlgEst = YES EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF ERROR-STATUS:ERROR = YES THEN DO:
        pMensaje = "ERROR en el correlativo del documento " + x-CodDoc + " (" + ERROR-STATUS:GET-MESSAGE(1) + ")".
        UNDO, RETURN 'ADM-ERROR'.
    END.
    x-Serie = FacCorre.nroser.
    x-Numero = Faccorre.correlativo.
    Faccorre.correlativo = Faccorre.correlativo + 1.
    /* Ultimos ajustes a los temporales */
    x-NroDoc = STRING(x-Serie,"999") + STRING(x-Numero,"99999999").
    CREATE ttCcbcdocu.
    BUFFER-COPY Ccbcdocu TO ttCcbcdocu
    ASSIGN 
        ttCcbcdocu.CodCia = s-codcia
        ttCcbcdocu.coddiv = s-coddiv
        ttCcbcdocu.coddoc = x-coddoc
        ttccbcdocu.nrodoc = x-NroDoc
        ttccbcdocu.cndcre = "N"
        ttccbcdocu.tpofac = "OTROS"
        ttccbcdocu.codcta = pCodCta
        ttccbcdocu.codref = pCodRef
        ttccbcdocu.nroref = pNroRef
        ttccbcdocu.porigv = Ccbcdocu.porigv
        ttccbcdocu.codmon = Ccbcdocu.codmon
        ttCcbcdocu.fchdoc = TODAY
        ttCcbcdocu.fchvto = TODAY + 365
        ttCcbcdocu.usuario = s-user-id
        ttCcbcdocu.flgest = 'P'
        ttCcbcdocu.ImpBrt = 0
        ttCcbcdocu.ImpExo = 0
        ttCcbcdocu.ImpDto = 0
        ttCcbcdocu.ImpIgv = 0
        ttCcbcdocu.ImpTot = 0
        ttCcbcdocu.horcie = STRING(TIME,"HH:MM:SS")
        NO-ERROR.
    IF ERROR-STATUS:ERROR = YES THEN DO:
        pMensaje = "ERROR - Ccbcdocu :(" + ERROR-STATUS:GET-MESSAGE(1) + ")".
        UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
    END.
    ASSIGN
        pNroDoc = ttccbcdocu.nrodoc.    /* # de Control */
    /* *************************************************************************** */
    /* Cargamos los items */
    /* *************************************************************************** */
    x-Item = 0.
    x-ImpBrt = 0.
    x-ImpExo = 0.
    x-ImpIgv = 0.
    x-ImpTot = 0.
    EMPTY TEMP-TABLE ttCcbddocu.
    FOR EACH txCcbddocu OF txCcbcdocu:
        x-item = x-item + 1.
        CREATE ttCcbddocu.
        ASSIGN
            ttCcbddocu.codcia = s-codcia
            ttCcbddocu.coddiv = s-coddiv
            ttCcbddocu.coddoc = x-coddoc
            ttCcbddocu.nrodoc = x-NroDoc 
            ttCcbddocu.nroitm = x-item
            ttCcbddocu.codcli = ttCcbcdocu.codcli
            ttCcbddocu.codmat = txCcbddocu.codmat
            ttCcbddocu.aftigv = txCcbddocu.aftigv
            ttCcbddocu.candes = txCcbddocu.candes
            ttCcbddocu.preuni = ABS(txCcbddocu.impdto) / txCcbddocu.candes
            ttCcbddocu.implin = ABS(txCcbddocu.impdto)
            ttCcbddocu.undvta = txCcbddocu.undvta
            ttCcbddocu.factor = txCcbddocu.factor
            ttCcbddocu.impigv = (IF txCcbddocu.AftIgv THEN (ABS(txCcbddocu.impdto) / (1 + ttCcbcdocu.PorIgv)) ELSE 0)
            .
        IF txCcbddocu.AftIgv = YES THEN x-ImpBrt = x-ImpBrt + ttCcbddocu.implin.
        IF txCcbddocu.AftIgv = NO THEN x-ImpExo = x-ImpExo + ttCcbddocu.implin.
        x-ImpIgv = x-ImpIgv + ttCcbddocu.ImpIgv.
        x-ImpTot = x-ImpTot + ttCcbddocu.ImpLin.
    END.
    ASSIGN  
        ttCcbcdocu.ImpBrt = x-impbrt
        ttCcbcdocu.ImpExo = x-impexo
        ttCcbcdocu.ImpIgv = x-ImpIgv
        ttCcbcdocu.ImpTot = x-ImpTot
        ttCcbcdocu.ImpVta = ttCcbcdocu.ImpBrt - ttCcbcdocu.ImpIgv
        ttCcbcdocu.ImpBrt = ttCcbcdocu.ImpBrt - ttCcbcdocu.ImpIgv
        ttCcbcdocu.SdoAct = ttCcbcdocu.ImpTot.
    IF NOT CAN-FIND(FIRST ttCcbddocu NO-LOCK) THEN DO:
        pMensaje = 'NO hay items'.
        RETURN 'ADM-ERROR'.
    END.
    /* *************************************************************************** */
    /* La cabecera del documento */
    /* *************************************************************************** */
    CREATE b-ccbcdocu.
    BUFFER-COPY ttCcbcdocu TO b-ccbcdocu NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        pMensaje = "ERROR al crear registro en CCBCDOCU (" + ERROR-STATUS:GET-MESSAGE(1) + ")".
        UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
    END.
    FOR EACH ttCcbddocu ON ERROR UNDO, THROW:
        /* Detalle update block */
        CREATE b-ccbddocu.
        BUFFER-COPY ttCcbddocu TO b-ccbddocu NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            pMensaje = "ERROR al crear registro en CCBDDOCU (" + ERROR-STATUS:GET-MESSAGE(1) + ")".
            UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
        END.
    END.
    /* ****************************** */
    /* Ic - 16Nov2021 - Importes Aritm�tica de SUNAT */
    /* ****************************** */
    IF x-nueva-arimetica-sunat-2021 = YES THEN DO:
        DEF VAR hProc AS HANDLE NO-UNDO.
        RUN sunat/sunat-calculo-importes.r PERSISTENT SET hProc.
        RUN tabla-ccbcdocu IN hProc (INPUT s-coddiv,
                                     INPUT x-coddoc,
                                     INPUT x-nrodoc,
                                     OUTPUT pMensaje).
        DELETE PROCEDURE hProc.
        /* ****************************** */
        IF RETURN-VALUE = "ADM-ERROR" THEN DO:
            UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
        END.
    END.
END. /* TRANSACTION block */
IF AVAILABLE FacCorre THEN RELEASE faccorre.
IF AVAILABLE b-ccbcdocu THEN RELEASE b-ccbcdocu.
IF AVAILABLE b-ccbddocu THEN RELEASE b-ccbddocu.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FIRST-TRANSACTION-OLD W-Win 
PROCEDURE FIRST-TRANSACTION-OLD :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF INPUT PARAMETER pNroSer AS INTE.
DEF INPUT PARAMETER pCodCta AS CHAR.
DEF INPUT PARAMETER pCodRef AS CHAR.
DEF INPUT PARAMETER pNroRef AS CHAR.
DEF OUTPUT PARAMETER pNroDoc AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

DEFINE VAR x-serie AS INT.
DEFINE VAR x-numero AS INT.
DEFINE VAR x-CodDoc AS CHAR INIT 'N/C' NO-UNDO.
DEFINE VAR x-nrodoc AS CHAR.

DEFINE BUFFER b-ccbcdocu FOR ccbcdocu.
DEFINE BUFFER b-ccbddocu FOR ccbddocu.
DEFINE BUFFER CREDITO FOR ccbcdocu.

EMPTY TEMP-TABLE T-FELogErrores.
EMPTY TEMP-TABLE ttCcbcdocu.

GRABAR_DOCUMENTO:
DO TRANSACTION ON ERROR UNDO GRABAR_DOCUMENTO, LEAVE GRABAR_DOCUMENTO ON STOP UNDO GRABAR_DOCUMENTO, LEAVE GRABAR_DOCUMENTO:
    /* Nos posicionamos en la FAC */
    FIND Ccbcdocu WHERE Ccbcdocu.codcia = s-codcia
        AND Ccbcdocu.coddoc = pCodRef 
        AND Ccbcdocu.nrodoc = pNroRef
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Ccbcdocu THEN DO:
        pMensaje = 'Comprobante ' + pCodRef + ' ' + pNroRef + ' no registrado'.
        UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
    END.
    /* El numero de serie */ 
    FIND FIRST Faccorre WHERE  Faccorre.CodCia = S-CODCIA 
        AND Faccorre.CodDoc = x-CodDoc
        AND Faccorre.CodDiv = S-CODDIV 
        AND Faccorre.NroSer = pNroSer
        AND Faccorre.FlgEst = YES EXCLUSIVE-LOCK NO-ERROR.
    IF LOCKED faccorre THEN DO:
        pMensaje = "La tabla FACCORRE esta bloqueada por otro usuario".
        UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
    END.                                  
    ELSE DO:
        IF NOT AVAILABLE faccorre THEN DO:
            pMensaje = 'Correlativo no asignado a la divisi�n : ' + s-coddiv .
            UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
        END.
    END.

    x-serie = FacCorre.nroser.
    x-numero = Faccorre.correlativo.
    ASSIGN
        Faccorre.correlativo = Faccorre.correlativo + 1 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        pMensaje = "Al actualizar el correlativo (" + ERROR-STATUS:GET-MESSAGE(1) + ")".
        UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
    END.                  

    /* Ultimos ajustes a los temporales */
    x-nrodoc = STRING(x-serie,"999") + STRING(x-numero,"99999999").

    CREATE ttCcbcdocu.
    BUFFER-COPY Ccbcdocu TO ttCcbcdocu
    ASSIGN 
        ttCcbcdocu.CodCia = s-codcia
        ttCcbcdocu.coddiv = s-coddiv
        ttCcbcdocu.coddoc = x-coddoc
        ttccbcdocu.nrodoc = x-nrodoc
        ttccbcdocu.cndcre = "N"
        ttccbcdocu.tpofac = "OTROS"
        ttccbcdocu.codcta = pCodCta
        ttccbcdocu.codref = pCodRef
        ttccbcdocu.nroref = pNroRef
        ttccbcdocu.porigv = Ccbcdocu.porigv
        ttccbcdocu.codmon = Ccbcdocu.codmon
/*         ttCcbcdocu.codped = Ccbcdocu.coddoc          /* PNC */ */
/*         ttCcbcdocu.nroped = Ccbcdocu.nrodoc                    */
        ttCcbcdocu.fchdoc = TODAY
        ttCcbcdocu.fchvto = TODAY + 365
        ttCcbcdocu.usuario = s-user-id
        ttCcbcdocu.flgest = 'P'
        ttCcbcdocu.ImpBrt = 0
        ttCcbcdocu.ImpExo = 0
        ttCcbcdocu.ImpDto = 0
        ttCcbcdocu.ImpIgv = 0
        ttCcbcdocu.ImpTot = 0
        ttCcbcdocu.horcie = STRING(TIME,"HH:MM:SS")
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        pMensaje = "ERROR - ttCcbcdocu :(" + ERROR-STATUS:GET-MESSAGE(1) + ")".
        UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
    END.
    ASSIGN
        pNroDoc = ttccbcdocu.nrodoc.    /* # de Control */
    /* ****************** */
    /* Cargamos los items */
    /* ****************** */
    RUN Carga-Items.
    IF NOT CAN-FIND(FIRST ttCcbddocu NO-LOCK) THEN DO:
        pMensaje = 'NO hay items'.
        RETURN 'ADM-ERROR'.
    END.
    /* ****************** */

    /**/
    FOR EACH ttCcbddocu :
        ASSIGN 
            ttCcbddocu.nrodoc = x-nrodoc NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            pMensaje = "ERROR - ttCcbddocu (" + ERROR-STATUS:GET-MESSAGE(1) + ")".
            UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
        END.                  
    END.

    /* La cabecera del documento */
    CREATE b-ccbcdocu.
    BUFFER-COPY ttCcbcdocu TO b-ccbcdocu NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        pMensaje = "ERROR al crear registro en CCBCDOCU (" + ERROR-STATUS:GET-MESSAGE(1) + ")".
        UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
    END.
    FOR EACH ttCcbddocu ON ERROR UNDO, THROW:
        /* Detalle update block */
        CREATE b-ccbddocu.
        BUFFER-COPY ttCcbddocu TO b-ccbddocu NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            pMensaje = "ERROR al crear registro en CCBDDOCU (" + ERROR-STATUS:GET-MESSAGE(1) + ")".
            UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
        END.
    END.
    /* Actualizamos el estado de todas las facturas relacionadas */
    FOR EACH T-CDOCU NO-LOCK:
        FIND FIRST Ccbcdocu OF T-CDOCU EXCLUSIVE-LOCK NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            pMensaje = "ERROR al actualizar el comprobante " + T-CDOCU.coddoc + " " + T-CDOCU.nrodoc + " (" + ERROR-STATUS:GET-MESSAGE(1) + ")".
            UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
        END.
        ASSIGN
            CcbCDocu.FchCie = TODAY
            CcbCDocu.FlgCie = "C".      /* OJO */
        FOR EACH CREDITO EXCLUSIVE-LOCK WHERE CREDITO.codcia = s-codcia
            AND CREDITO.coddoc = "N/C"
            AND CREDITO.codref = Ccbcdocu.coddoc
            AND CREDITO.nroref = Ccbcdocu.nrodoc
            AND CREDITO.CndCre = "D"
            AND CREDITO.flgest <> "A"
            AND CREDITO.flgcie <> "C":
            ASSIGN
                CREDITO.FchCie = TODAY
                CREDITO.FlgCie = "C".      /* OJO */
        END.
    END.
    /* ****************************** */
    /* Ic - 16Nov2021 - Importes Arimetica de SUNAT */
    /* ****************************** */
    IF x-nueva-arimetica-sunat-2021 = YES THEN DO:
        DEF VAR hProc AS HANDLE NO-UNDO.
        RUN sunat/sunat-calculo-importes.r PERSISTENT SET hProc.
        RUN tabla-ccbcdocu IN hProc (INPUT s-coddiv,
                                     INPUT x-coddoc,
                                     INPUT x-nrodoc,
                                     OUTPUT pMensaje).
        DELETE PROCEDURE hProc.
        /* ****************************** */
        IF RETURN-VALUE = "ADM-ERROR" THEN DO:
            UNDO GRABAR_DOCUMENTO, RETURN 'ADM-ERROR'.
        END.
    END.
END. /* TRANSACTION block */
RELEASE faccorre.
RELEASE b-ccbcdocu.
RELEASE b-ccbddocu.
RELEASE Ccbcdocu.
RELEASE CREDITO.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FIRST-TRANSACTION-SAVE-ITEMS W-Win 
PROCEDURE FIRST-TRANSACTION-SAVE-ITEMS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
DEFINE VAR x-estado-item AS CHAR.
DEFINE VAR x-item AS INT INIT 0.
DEFINE VAR x-ImpBrt AS DEC.
DEFINE VAR x-ImpExo AS DEC.
DEFINE VAR x-ImpIgv AS DEC.
DEFINE VAR x-ImpTot AS DEC.

EMPTY TEMP-TABLE ttCcbddocu.

/* Todos los valores en positivo */
FOR EACH T-DDOCU EXCLUSIVE-LOCK:
    T-DDOCU.impdto = ABSOLUTE(T-DDOCU.impdto).
END.

FOR EACH T-DDOCU NO-LOCK:
    x-item = x-item + 1.
    CREATE ttCcbddocu.
    ASSIGN
        ttCcbddocu.codcia = s-codcia
        ttCcbddocu.coddiv = s-coddiv
        ttCcbddocu.coddoc = x-coddoc
        ttCcbddocu.nroitm = x-item
        ttCcbddocu.codmat = T-DDOCU.codmat
        ttCcbddocu.aftigv = T-DDOCU.aftigv
        ttCcbddocu.candes = T-DDOCU.candes
        ttCcbddocu.preuni = T-DDOCU.impdto / T-DDOCU.candes
        ttCcbddocu.implin = T-DDOCU.impdto
        ttCcbddocu.undvta = T-DDOCU.undvta
        ttCcbddocu.factor = T-DDOCU.factor
        ttCcbddocu.impigv = (IF T-DDOCU.AftIgv THEN (T-DDOCU.impdto / (1 + ttCcbcdocu.PorIgv)) ELSE 0)
        .
    IF T-DDOCU.AftIgv = YES THEN x-ImpBrt = x-ImpBrt + ttCcbddocu.implin.
    IF T-DDOCU.AftIgv = NO THEN x-ImpExo = x-ImpExo + ttCcbddocu.implin.
    x-ImpIgv = x-ImpIgv + ttCcbddocu.ImpIgv.
    x-ImpTot = x-ImpTot + ttCcbddocu.ImpLin.
END.
ASSIGN  
    ttCcbcdocu.ImpBrt = x-impbrt
    ttCcbcdocu.ImpExo = x-impexo
    ttCcbcdocu.ImpIgv = x-ImpIgv
    ttCcbcdocu.ImpTot = x-ImpTot
    ttCcbcdocu.ImpVta = ttCcbcdocu.ImpBrt - ttCcbcdocu.ImpIgv
    ttCcbcdocu.ImpBrt = ttCcbcdocu.ImpBrt - ttCcbcdocu.ImpIgv
    ttCcbcdocu.SdoAct = ttCcbcdocu.ImpTot.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-NC W-Win 
PROCEDURE Genera-NC :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER x-CodDoc AS CHAR NO-UNDO.

    DEF INPUT PARAMETER pNroSer AS INTE.
    DEF INPUT PARAMETER pCodRef AS CHAR.        /* FAC */
    DEF INPUT PARAMETER pNroRef AS CHAR.

    /* Controles */
    DEFINE VAR hxProc AS HANDLE NO-UNDO.                /* Handle Libreria */
    DEFINE VAR x-retval AS CHAR NO-UNDO.
    DEFINE VAR pMensaje AS CHAR NO-UNDO.
    DEFINE VAR pMsgSunat AS CHAR NO-UNDO.
    DEFINE VAR pNroDoc AS CHAR NO-UNDO.

    /* Casos:
    Solo N/C
    Solo N/D
    N/C y N/D 
    */


    IF x-CodDoc = "N/C" THEN DO:
        RUN ccb\libreria-ccb.r PERSISTENT SET hxProc.
        RUN notas-creditos-supera-comprobante IN hxProc (INPUT pCodRef, 
                                                         INPUT pNroRef,
                                                         OUTPUT x-retval).
        DELETE PROCEDURE hxProc.                    /* Release Libreria */
        /* 
            pRetVal : NO (importes de N/C NO supera al comprobante)
        */
        IF x-retval <> "NO" THEN DO:
            MESSAGE "El comprobante " + pCodRef + " " + pNroRef SKIP
                "tiene emitida varias N/Cs que la suma de sus importes" SKIP
                "superan al importe total del comprobante"
                VIEW-AS ALERT-BOX INFORMATION.
            RETURN 'ADM-ERROR'.
        END.
    END.

    MESSAGE 'Seguro de generar la ' x-CodDoc 'con Serie Nro : ' + STRING(pNroSer)
         VIEW-AS ALERT-BOX QUESTION
        BUTTONS YES-NO UPDATE rpta AS LOG.
    IF rpta = NO THEN RETURN 'ADM-ERROR'.

    DO TRANSACTION ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
        RUN FIRST-TRANSACTION (INPUT pNroSer,
                               INPUT (IF x-CodDoc = "N/C" THEN pCodCtaNC ELSE pCodCtaND),
                               OUTPUT pNroDoc,
                               OUTPUT pMensaje).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            IF TRUE <> (pMensaje > '') THEN pMensaje = 'No se pudo generar la N/C'.
            UNDO, LEAVE.
        END.
        RUN SECOND-TRANSACTION (INPUT pNroDoc, OUTPUT pMsgSunat).
    END.
    IF pMensaje > '' THEN DO:
        MESSAGE pMensaje VIEW-AS ALERT-BOX ERROR.
        RETURN 'ADM-ERROR'.
    END.
    IF pMsgSunat > '' THEN DO:
        MESSAGE pMsgSunat VIEW-AS ALERT-BOX WARNING.
    END.
    MESSAGE 'Proceso Terminado' SKIP
        x-CodDoc + ":" pNroDoc VIEW-AS ALERT-BOX INFORMATION.
    RETURN 'OK'.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-NC-MASTER W-Win 
PROCEDURE Genera-NC-MASTER :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

/* Varias formas de generar N/C */
/* Depende de cuantos clientes est�n involucrados */
DEF VAR pNroDoc AS CHAR NO-UNDO.

/* ******************************************************************************************* */
/* Limpiamos tablas receptoras: txCcbcdocu txCcbddocu */
/* ******************************************************************************************* */
RUN Carga-Temporal-NC.
IF NOT CAN-FIND(FIRST txCcbcdocu) THEN RETURN 'OK'.
/* ******************************************************************************************* */
/* Una vez terminado el proceso anterior debe quedar una o m�s comprobantes referenciados */
/* ******************************************************************************************* */
PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FOR EACH txCcbcdocu:
        RUN FIRST-TRANSACTION (INPUT "N/C",
                               INPUT COMBO-BOX_NroSerNC,
                               INPUT FILL-IN_CodCtaNC,
                               INPUT txCcbcdocu.CodDoc,     /* FAC */
                               INPUT txCcbcdocu.NroDoc,
                               OUTPUT pNroDoc,
                               OUTPUT pMensaje).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            IF TRUE <> (pMensaje > '') THEN pMensaje = 'No se pudo generar la N/C'.
            UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
        END.
    END.
END.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Genera-ND-MASTER W-Win 
PROCEDURE Genera-ND-MASTER :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.

/* Varias formas de generar N/C */
/* Depende de cuantos clientes est�n involucrados */
DEF VAR pNroDoc AS CHAR NO-UNDO.

/* ******************************************************************************************* */
/* Limpiamos tablas receptoras: txCcbcdocu txCcbddocu */
/* ******************************************************************************************* */
RUN Carga-Temporal-ND.
IF NOT CAN-FIND(FIRST txCcbcdocu) THEN RETURN 'OK'.
/* ******************************************************************************************* */
/* Una vez terminado el proceso anterior debe quedar una o m�s comprobantes referenciados */
/* ******************************************************************************************* */
PRINCIPAL:
DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    FOR EACH txCcbcdocu:
        RUN FIRST-TRANSACTION (INPUT "N/D",
                               INPUT COMBO-BOX_NroSerND,
                               INPUT FILL-IN_CodCtaND,
                               INPUT txCcbcdocu.CodDoc,     /* FAC */
                               INPUT txCcbcdocu.NroDoc,
                               OUTPUT pNroDoc,
                               OUTPUT pMensaje).
        IF RETURN-VALUE = 'ADM-ERROR' THEN DO:
            IF TRUE <> (pMensaje > '') THEN pMensaje = 'No se pudo generar la N/C'.
            UNDO PRINCIPAL, RETURN 'ADM-ERROR'.
        END.
    END.
END.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Graba-Temp-FeLogErrores W-Win 
PROCEDURE Graba-Temp-FeLogErrores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH T-FeLogErrores:
    CREATE FeLogErrores.
    BUFFER-COPY T-FeLogErrores TO FeLogErrores NO-ERROR.
    DELETE T-FeLogErrores.
END.
RELEASE FeLogErrores.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-change-page W-Win 
PROCEDURE local-change-page :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'change-page':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN GET-ATTRIBUTE('CURRENT-PAGE').
  CASE RETURN-VALUE:
      WHEN "1" THEN RUN Captura-Temporal IN h_b-nc-dcto-por-vol-mat ( INPUT TABLE T-DDOCU).
      WHEN "2" THEN RUN Captura-Temporal IN h_b-nc-dcto-por-vol-doc ( INPUT TABLE T-CDOCU).
  END CASE.

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
  DO WITH FRAME {&FRAME-NAME}:
      COMBO-BOX_CodDiv:DELIMITER = "|".
      COMBO-BOX_CodDiv:DELETE(1).
      FOR EACH PriDtoVolAcuCab NO-LOCK WHERE PriDtoVolAcuCab.CodCia = s-codcia
          AND PriDtoVolAcuCab.Cerrado = NO,
          FIRST gn-divi OF PriDtoVolAcuCab NO-LOCK:
          COMBO-BOX_CodDiv:ADD-LAST(PriDtoVolAcuCab.CodDiv + ' ' + GN-DIVI.DesDiv + ' ' + 
                                    'Desde ' + STRING(PriDtoVolAcuCab.Inicio) + ' ' +
                                    'Hasta ' + STRING(PriDtoVolAcuCab.Fin), PriDtoVolAcuCab.Internal_Id) .
      END.
      COMBO-BOX_NroSerNC:DELETE(1).
      FOR EACH FacCorre NO-LOCK WHERE FacCorre.CodCia = s-codcia AND
          FacCorre.CodDiv = s-coddiv AND
          FacCorre.CodDoc = "N/C" AND
          FacCorre.FlgEst = YES
          BY FacCorre.NroSer DESC:
          COMBO-BOX_NroSerNC:ADD-LAST(STRING(FacCorre.NroSer,'999')).
          COMBO-BOX_NroSerNC = FacCorre.NroSer.
      END.
      COMBO-BOX_NroSerND:DELETE(1).
      FOR EACH FacCorre NO-LOCK WHERE FacCorre.CodCia = s-codcia AND
          FacCorre.CodDiv = s-coddiv AND
          FacCorre.CodDoc = "N/D" AND
          FacCorre.FlgEst = YES
          BY FacCorre.NroSer DESC:
          COMBO-BOX_NroSerND:ADD-LAST(STRING(FacCorre.NroSer,'999')).
          COMBO-BOX_NroSerND = FacCorre.NroSer.
      END.
      COMBO-BOX_NroSerNC:LABEL = "Nro. de Serie N/C".
      FILL-IN_CodCtaNC = pCodCtaNC.
      FIND FIRST CcbTabla WHERE CcbTabla.CodCia = s-codcia
          AND CcbTabla.Tabla = "N/C" 
          AND CcbTabla.Codigo = pCodCtaNC
          NO-LOCK NO-ERROR.
      IF AVAILABLE CcbTabla THEN FILL-IN_DescripcionNC = CcbTabla.Nombre.
      COMBO-BOX_NroSerND:LABEL = "Nro. de Serie N/D".
      FILL-IN_CodCtaND = pCodCtaND.
      FIND FIRST CcbTabla WHERE CcbTabla.CodCia = s-codcia
          AND CcbTabla.Tabla = "N/D" 
          AND CcbTabla.Codigo = pCodCtaND
          NO-LOCK NO-ERROR.
      IF AVAILABLE CcbTabla THEN FILL-IN_DescripcionND = CcbTabla.Nombre.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MASTER-TRANSACTION W-Win 
PROCEDURE MASTER-TRANSACTION :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF OUTPUT PARAMETER pMensaje AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER pMsgSunat AS CHAR NO-UNDO.

DO TRANSACTION ON ERROR UNDO, RETURN 'ADM-ERROR' ON STOP UNDO, RETURN 'ADM-ERROR':
    EMPTY TEMP-TABLE ttCcbcdocu.    /* Archivo de control */
    EMPTY TEMP-TABLE ttCcbddocu.

    RUN Cierra-Comprobante-Origen (OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
    
    RUN Genera-NC-MASTER (OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.

    RUN Genera-ND-MASTER (OUTPUT pMensaje).
    IF RETURN-VALUE = 'ADM-ERROR' THEN UNDO, RETURN 'ADM-ERROR'.
END.

EMPTY TEMP-TABLE T-FELogErrores.
FOR EACH ttCcbcdocu:
    RUN SECOND-TRANSACTION (INPUT ttCcbcdocu.CodDoc,
                            INPUT ttCcbcdocu.NroDoc, 
                            OUTPUT pMsgSunat).
    IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN 'OK'.
END.

RETURN 'OK'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SECOND-TRANSACTION W-Win 
PROCEDURE SECOND-TRANSACTION :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER x-CodDoc AS CHAR.                                    
    DEFINE INPUT PARAMETER x-NroDoc AS CHAR.
    DEFINE OUTPUT PARAMETER pMsgSunat AS CHAR NO-UNDO.

    DEFINE VAR x-mensaje AS CHAR NO-UNDO.

    EMPTY TEMP-TABLE T-FeLogErrores.

    RUN sunat\progress-to-ppll-v3 ( INPUT s-coddiv,
                                    INPUT x-coddoc,
                                    INPUT x-nrodoc,
                                    INPUT-OUTPUT TABLE T-FELogErrores,
                                    OUTPUT x-mensaje ).
    
    RUN Graba-Temp-FeLogErrores.    /* Control de Errores (si es que hay) */

    IF RETURN-VALUE = 'OK' THEN DO:
        RETURN "OK".
    END.
    ELSE DO:
        pMsgSunat = "Hubo problemas en la generaci�n del documento" + CHR(10) +
                    "Mensaje : " + x-mensaje + CHR(10) +
                    "Por favor intente de nuevo".
        RETURN "ADM-ERROR".
    END.

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

