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
DEFINE SHARED VAR s-codcia AS INT.

/* Local Variable Definitions ---                                       */

DEFINE VAR x-vtatabla AS CHAR INIT "CONFIG-SESSION".
DEFINE VAR x-llave_c1 AS CHAR INIT "DESBLOQUEO".
DEFINE VAR x-llave_c2 AS CHAR INIT "AUTOMATICO".

DEFINE SHARED VAR pRCID AS INT.

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
&Scoped-Define ENABLED-OBJECTS COMBO-BOX-1 BUTTON-1 BUTTON-2 BUTTON-3 ~
FILL-IN-intervalo 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-1 FILL-IN-intervalo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Modificar" 
     SIZE 9 BY .96.

DEFINE BUTTON BUTTON-2 
     LABEL "Grabar" 
     SIZE 9.29 BY .96.

DEFINE BUTTON BUTTON-3 
     LABEL "Cancelar" 
     SIZE 9 BY .96.

DEFINE VARIABLE COMBO-BOX-1 AS CHARACTER FORMAT "X(256)":U INITIAL "No" 
     LABEL "Eliga" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "SI","NO" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-intervalo AS INTEGER FORMAT ">>,>>9":U INITIAL 0 
     LABEL "Minutos" 
     VIEW-AS FILL-IN 
     SIZE 7.29 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     COMBO-BOX-1 AT ROW 4.19 COL 17 COLON-ALIGNED WIDGET-ID 34
     BUTTON-1 AT ROW 7.46 COL 8 WIDGET-ID 20
     BUTTON-2 AT ROW 7.46 COL 27 WIDGET-ID 22
     BUTTON-3 AT ROW 7.46 COL 17.14 WIDGET-ID 24
     FILL-IN-intervalo AT ROW 6.04 COL 16.72 COLON-ALIGNED WIDGET-ID 26
     "Forzar el cierre automatico para sesiones activas" VIEW-AS TEXT
          SIZE 41 BY .62 AT ROW 1.38 COL 4 WIDGET-ID 2
          FGCOLOR 9 FONT 6
     "que se quedaron colgadas cuando el progress" VIEW-AS TEXT
          SIZE 41 BY .62 AT ROW 1.96 COL 4 WIDGET-ID 36
          FGCOLOR 9 FONT 6
     "se cerro inesperadamente o el usuario salio" VIEW-AS TEXT
          SIZE 41 BY .62 AT ROW 2.58 COL 4 WIDGET-ID 38
          FGCOLOR 9 FONT 6
     "de la forma incorrecta" VIEW-AS TEXT
          SIZE 41 BY .62 AT ROW 3.15 COL 4 WIDGET-ID 40
          FGCOLOR 9 FONT 6
     "Intervalo de tiempo" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 5.31 COL 4 WIDGET-ID 28
     "Valor minimo 1 minuto" VIEW-AS TEXT
          SIZE 16.57 BY .62 AT ROW 6.12 COL 27.72 WIDGET-ID 30
          FGCOLOR 4 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 49.43 BY 8.31
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
         TITLE              = "Configuracion cierre de sesiones"
         HEIGHT             = 8.31
         WIDTH              = 49.43
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 80
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Configuracion cierre de sesiones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Configuracion cierre de sesiones */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Win
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Modificar */
DO:
  DO WITH FRAME {&FRAME-NAME} :
    ENABLE fill-in-intervalo.
    ENABLE combo-box-1.

    ENABLE button-3.
    ENABLE button-2.
    DISABLE button-1.

  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Win
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Grabar */
DO:
    MESSAGE 'Seguro de Grabar?' VIEW-AS ALERT-BOX QUESTION
            BUTTONS YES-NO UPDATE rpta AS LOG.
    IF rpta = NO THEN RETURN NO-APPLY.

    ASSIGN fill-in-intervalo combo-box-1.

    IF combo-box-1 = 'SI' THEN DO:
        IF fill-in-intervalo < 1 THEN DO:
            MESSAGE "El intervalo debe ser mayor o igual a uno (1)"
                VIEW-AS ALERT-BOX INFORMATION.
            RETURN NO-APPLY.
        END.
    END.

    /* --- */
    DISABLE TRIGGERS FOR LOAD OF vtatabla.

    FIND FIRST vtatabla WHERE vtatabla.codcia = s-codcia AND 
                              vtatabla.tabla = x-vtatabla AND
                              vtatabla.llave_c1 = x-llave_c1 AND 
                              vtatabla.llave_c2 = x-llave_c2 EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE vtatabla THEN DO:
        CREATE vtatabla.
            ASSIGN vtatabla.codcia = s-codcia
                    vtatabla.tabla = x-vtatabla
                    vtatabla.llave_c1 = x-llave_c1
                    vtatabla.llave_c2 = x-llave_c2.
    END.
    ASSIGN vtatabla.libre_c01 = combo-box-1
            vtatabla.valor[1] = fill-in-intervalo.

    RELEASE vtatabla.

    CREATE LogTabla.
    ASSIGN
      logtabla.codcia = s-codcia
      logtabla.Dia = TODAY
      logtabla.Evento = 'WRITE'
      logtabla.Hora = STRING(TIME, 'HH:MM:SS')
      logtabla.Tabla = 'VTATABLA'
      logtabla.Usuario = USERID("DICTDB")
      logtabla.ValorLlave = "SESIONES" + "|" + combo-box-1 + '|' + STRING(fill-in-intervalo).
      logtabla.numid = pRCID.

   RELEASE logtabla NO-ERROR.

    DO WITH FRAME {&FRAME-NAME} :
        ENABLE button-3.
        DISABLE button-2.
        ENABLE button-1.

        DISABLE fill-in-intervalo.
        DISABLE combo-box-1.
    END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Cancelar */
DO:
  DO WITH FRAME {&FRAME-NAME} :
    DISABLE fill-in-intervalo.
    DISABLE combo-box-1.

    DISABLE button-3.
    DISABLE button-2.
    ENABLE button-1.

  END.
  
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
  DISPLAY COMBO-BOX-1 FILL-IN-intervalo 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE COMBO-BOX-1 BUTTON-1 BUTTON-2 BUTTON-3 FILL-IN-intervalo 
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

  FIND FIRST vtatabla WHERE vtatabla.codcia = 1 AND 
                            vtatabla.tabla = x-vtatabla AND
                            vtatabla.llave_c1 = x-llave_c1 AND 
                            vtatabla.llave_c2 = x-llave_c2 NO-LOCK NO-ERROR.
  IF AVAILABLE vtatabla THEN DO:
    IF vtatabla.libre_c01 = 'SI' OR vtatabla.libre_c01 = 'NO' THEN DO:
        combo-box-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = vtatabla.libre_c01.
    END.
    fill-in-intervalo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(vtatabla.valor[1],">>,>>9").
  END.


  DO WITH FRAME {&FRAME-NAME} :
    DISABLE combo-box-1.
    DISABLE fill-in-intervalo.

    DISABLE button-3.
    DISABLE button-2.
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
