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
DEF SHARED VAR s-codcia AS INTE.

/* Parameters Definitions ---                                           */
DEF INPUT PARAMETER pCodDoc AS CHAR.
/* Local Variable Definitions ---                                       */

DEF NEW SHARED VAR s-coddoc  AS CHAR INITIAL "BD".

s-CodDoc = pCodDoc.

FIND FacDocum WHERE facdocum.codcia = s-codcia AND facdocum.coddoc = s-coddoc
              NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacDocum
THEN DO:
    MESSAGE "No esta definido el documento" s-coddoc VIEW-AS ALERT-BOX ERROR.
    RETURN.
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
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 BUTTON-Autorizar BUTTON-Anular ~
x-FchAte-1 x-FchAte-2 BUTTON-3 BUTTON-Rechazar COMBO-Situacion 
&Scoped-Define DISPLAYED-OBJECTS x-FchAte-1 x-FchAte-2 COMBO-Situacion 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-autboldep AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-3 
     LABEL "APLICAR FILTRO" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-Anular 
     LABEL "ANULAR" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-Autorizar 
     LABEL "AUTORIZAR" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-Rechazar 
     LABEL "RECHAZAR" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE COMBO-Situacion AS CHARACTER FORMAT "X(256)":U INITIAL "E" 
     LABEL "Situaci�n" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Por Aprobar","E",
                     "Autorizada","P",
                     "Rechazada","R"
     DROP-DOWN-LIST
     SIZE 13 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE x-FchAte-1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Depositados desde" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE VARIABLE x-FchAte-2 AS DATE FORMAT "99/99/9999":U 
     LABEL "hasta" 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 2.69.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 121 BY 2.69.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-Autorizar AT ROW 1.19 COL 53 WIDGET-ID 22
     BUTTON-Anular AT ROW 1.19 COL 85 WIDGET-ID 26
     x-FchAte-1 AT ROW 1.38 COL 15 COLON-ALIGNED WIDGET-ID 12
     x-FchAte-2 AT ROW 1.38 COL 33.72 COLON-ALIGNED WIDGET-ID 14
     BUTTON-3 AT ROW 2.35 COL 32.86 WIDGET-ID 18
     BUTTON-Rechazar AT ROW 2.35 COL 69 WIDGET-ID 24
     COMBO-Situacion AT ROW 2.46 COL 15 COLON-ALIGNED WIDGET-ID 10
     RECT-2 AT ROW 1 COL 2 WIDGET-ID 16
     RECT-3 AT ROW 1 COL 52 WIDGET-ID 20
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 172.86 BY 24.65
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
         TITLE              = "AUTORIZACION/RECHAZO DE BOLETAS DE DEPOSITO"
         HEIGHT             = 24.65
         WIDTH              = 172.86
         MAX-HEIGHT         = 26.15
         MAX-WIDTH          = 191.29
         VIRTUAL-HEIGHT     = 26.15
         VIRTUAL-WIDTH      = 191.29
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* AUTORIZACION/RECHAZO DE BOLETAS DE DEPOSITO */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* AUTORIZACION/RECHAZO DE BOLETAS DE DEPOSITO */
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
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* APLICAR FILTRO */
DO:
  ASSIGN  COMBO-Situacion x-FchAte-1 x-FchAte-2.
  IF COMBO-Situacion <> 'E' AND (x-FchAte-1 = ? OR x-FchAte-2 = ?)
      THEN DO:
      MESSAGE 'Deber�a ingresar un rango de fechas v�lido' SKIP
          'Continuamos con el proceso?' VIEW-AS ALERT-BOX QUESTION
          BUTTONS YES-NO UPDATE rpta AS LOG.
      IF rpta = NO THEN RETURN NO-APPLY.
  END.
  RUN Aplicar-Filtros IN h_b-autboldep
    ( INPUT COMBO-Situacion /* CHARACTER */,
      INPUT x-FchAte-1 /* DATE */,
      INPUT x-FchAte-2 /* DATE */).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Anular
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Anular W-Win
ON CHOOSE OF BUTTON-Anular IN FRAME F-Main /* ANULAR */
DO:
  RUN Anular IN h_b-autboldep.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Autorizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Autorizar W-Win
ON CHOOSE OF BUTTON-Autorizar IN FRAME F-Main /* AUTORIZAR */
DO:

  RUN Autorizar IN h_b-autboldep.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Rechazar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Rechazar W-Win
ON CHOOSE OF BUTTON-Rechazar IN FRAME F-Main /* RECHAZAR */
DO:
  RUN Rechazar IN h_b-autboldep.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-Situacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-Situacion W-Win
ON VALUE-CHANGED OF COMBO-Situacion IN FRAME F-Main /* Situaci�n */
DO:
  ASSIGN COMBO-Situacion.
  RUN dispatch IN THIS-PROCEDURE ('open-query-cases':U).
  CASE TRUE:
      WHEN SELF:SCREEN-VALUE = "E" THEN DO:     /* Por Aprobar */
          ENABLE BUTTON-Anular BUTTON-Autorizar BUTTON-Rechazar WITH FRAME {&FRAME-NAME}.
      END.
      WHEN SELF:SCREEN-VALUE = "P" THEN DO:     /* Autorizada */
          ENABLE BUTTON-Anular WITH FRAME {&FRAME-NAME}.
          DISABLE BUTTON-Autorizar BUTTON-Rechazar WITH FRAME {&FRAME-NAME}.
      END.
      WHEN SELF:SCREEN-VALUE = "R" THEN DO:     /* Rechazada */
          DISABLE BUTTON-Anular BUTTON-Autorizar BUTTON-Rechazar WITH FRAME {&FRAME-NAME}.
      END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

{&WINDOW-NAME}:TITLE = "AUTORIZACION/RECHAZO - " + CAPS(FacDocum.NomDoc).

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
             INPUT  'aplic/ccb/b-autboldep.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-autboldep ).
       RUN set-position IN h_b-autboldep ( 3.62 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-autboldep ( 22.04 , 171.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-autboldep ,
             COMBO-Situacion:HANDLE IN FRAME F-Main , 'AFTER':U ).
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
  DISPLAY x-FchAte-1 x-FchAte-2 COMBO-Situacion 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-2 RECT-3 BUTTON-Autorizar BUTTON-Anular x-FchAte-1 x-FchAte-2 
         BUTTON-3 BUTTON-Rechazar COMBO-Situacion 
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
