&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-p-bolm AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-calc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-plan-m AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-calc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-plan AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "img/b-cancel":U
     LABEL "Button 4" 
     SIZE 10.72 BY 1.54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-4 AT ROW 10.96 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 70.29 BY 12.77.


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
         TITLE              = "Generacion de Contrato"
         HEIGHT             = 12.88
         WIDTH              = 70.72
         MAX-HEIGHT         = 12.88
         MAX-WIDTH          = 70.72
         VIRTUAL-HEIGHT     = 12.88
         VIRTUAL-WIDTH      = 70.72
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

IF NOT W-Win:LOAD-ICON("img/plemrbol":U) THEN
    MESSAGE "Unable to load icon: img/plemrbol"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Generacion de Contrato */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Generacion de Contrato */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 W-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* Button 4 */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win _ADM-CREATE-OBJECTS
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
             INPUT  'pln/v-plan.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-plan ).
       RUN set-position IN h_v-plan ( 1.15 , 1.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.35 , 52.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'pln/v-calc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-calc ).
       RUN set-position IN h_v-calc ( 2.65 , 1.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.35 , 52.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'pln/b-contrato-05.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-p-bolm ).
       RUN set-position IN h_b-p-bolm ( 4.15 , 1.43 ) NO-ERROR.
       /* Size in UIB:  ( 9.50 , 69.72 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 1.15 , 53.86 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.35 , 17.29 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico-2 ).
       RUN set-position IN h_p-navico-2 ( 2.65 , 53.86 ) NO-ERROR.
       RUN set-size IN h_p-navico-2 ( 1.35 , 17.29 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'pln/q-plan-m5.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-plan-m ).
       RUN set-position IN h_q-plan-m ( 1.15 , 40.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.31 , 5.43 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'pln/q-calc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-calc ).
       RUN set-position IN h_q-calc ( 2.65 , 40.14 ) NO-ERROR.
       /* Size in UIB:  ( 1.31 , 14.86 ) */

       /* Links to SmartViewer h_v-plan. */
       RUN add-link IN adm-broker-hdl ( h_q-plan-m , 'Record':U , h_v-plan ).

       /* Links to SmartViewer h_v-calc. */
       RUN add-link IN adm-broker-hdl ( h_q-calc , 'Record':U , h_v-calc ).

       /* Links to SmartBrowser h_b-p-bolm. */
       RUN add-link IN adm-broker-hdl ( h_q-calc , 'Record':U , h_b-p-bolm ).

       /* Links to SmartQuery h_q-plan-m. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_q-plan-m ).

       /* Links to SmartQuery h_q-calc. */
       RUN add-link IN adm-broker-hdl ( h_p-navico-2 , 'Navigation':U , h_q-calc ).
       RUN add-link IN adm-broker-hdl ( h_q-plan-m , 'Navigation':U , h_q-calc ).
       RUN add-link IN adm-broker-hdl ( h_q-plan-m , 'Record':U , h_q-calc ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-plan ,
             BUTTON-4:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-calc ,
             h_v-plan , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-p-bolm ,
             h_v-calc , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             BUTTON-4:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico-2 ,
             h_p-navico , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_q-plan-m ,
             h_p-navico-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_q-calc ,
             h_q-plan-m , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win _ADM-ROW-AVAILABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win _DEFAULT-ENABLE
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
  ENABLE BUTTON-4 
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win _ADM-SEND-RECORDS
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


