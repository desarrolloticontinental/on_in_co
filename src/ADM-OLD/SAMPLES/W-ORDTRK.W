&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r2 GUI
&ANALYZE-RESUME
/* Connected Databases 
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
&Scoped-Define ENABLED-OBJECTS Btn_updt-cus BTN-EXIT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_B-CUSLKP AS HANDLE NO-UNDO.
DEFINE VARIABLE h_B-LINORD AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_Q-ITMCAT AS HANDLE NO-UNDO.
DEFINE VARIABLE h_Q-ORDCUS AS HANDLE NO-UNDO.
DEFINE VARIABLE h_V-CUSADD AS HANDLE NO-UNDO.
DEFINE VARIABLE h_V-CUSCDT AS HANDLE NO-UNDO.
DEFINE VARIABLE h_V-CUSCOM AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cusord AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cusord-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_V-ITMINF AS HANDLE NO-UNDO.
DEFINE VARIABLE h_V-SHPINF AS HANDLE NO-UNDO.
DEFINE VARIABLE h_V-SPIN AS HANDLE NO-UNDO.
DEFINE VARIABLE h_w-cusupd AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-EXIT 
     LABEL "Exit" 
     SIZE 10.86 BY 1.04.

DEFINE BUTTON Btn_updt-cus 
     LABEL "&Update Customer" 
     SIZE 18 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_updt-cus AT ROW 6.92 COL 71
     BTN-EXIT AT ROW 8 COL 78
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.29 BY 17.5.

 

/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Customer Order Tracking"
         HEIGHT             = 17.5
         WIDTH              = 90.29
         MAX-HEIGHT         = 17.77
         MAX-WIDTH          = 90.29
         VIRTUAL-HEIGHT     = 17.77
         VIRTUAL-WIDTH      = 90.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
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
ON END-ERROR OF W-Win /* Customer Order Tracking */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Customer Order Tracking */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-EXIT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-EXIT W-Win
ON CHOOSE OF BTN-EXIT IN FRAME F-Main /* Exit */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_updt-cus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_updt-cus W-Win
ON CHOOSE OF Btn_updt-cus IN FRAME F-Main /* Update Customer */
DO:
  /* Ask the SmartContainer to view a different page.
     NOTE: this will only work if this procedure contains the method
     procedures to handle multi- paged applications.  Otherwise there will
     be an error. */
  RUN View-Page (5).
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


  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/b-cuslkp.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_B-CUSLKP ).
       RUN set-position IN h_B-CUSLKP ( 1.00 , 8.00 ).
       /* Size in UIB:  ( 5.77 , 79.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/v-cuscom.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_V-CUSCOM ).
       RUN set-position IN h_V-CUSCOM ( 7.00 , 7.00 ).
       /* Size in UIB:  ( 1.04 , 61.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/folder.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'FOLDER-TAB-TYPE = 1,
                     FOLDER-LABELS = Customer|Shipment|Order lines|Catalog':U ,
             OUTPUT h_folder ).
       RUN set-position IN h_folder ( 8.81 , 3.00 ).
       RUN set-size IN h_folder ( 9.50 , 85.86 ).

       /* Links to SmartViewer h_V-CUSCOM. */
       RUN add-link IN adm-broker-hdl ( h_B-CUSLKP , 'Record':U , h_V-CUSCOM ).

       /* Links to SmartFolder h_folder. */
       RUN add-link IN adm-broker-hdl ( h_folder , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/v-cusadd.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_V-CUSADD ).
       RUN set-position IN h_V-CUSADD ( 10.50 , 9.00 ).
       /* Size in UIB:  ( 7.42 , 72.00 ) */

       /* Links to SmartViewer h_V-CUSADD. */
       RUN add-link IN adm-broker-hdl ( h_B-CUSLKP , 'Record':U , h_V-CUSADD ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/v-cusord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_v-cusord ).
       RUN set-position IN h_v-cusord ( 10.27 , 5.00 ).
       /* Size in UIB:  ( 1.88 , 28.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/q-ordcus.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_Q-ORDCUS ).
       /* Position in UIB:  ( 10.50 , 39.00 ) */
       /* Size in UIB:  ( 1.31 , 5.43 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'SmartPanelType = NAV-ICON,
                     Right-To-Left = First-On-Left,
                     Edge-Pixels = 2':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 10.69 , 65.00 ).
       RUN set-size IN h_p-navico ( 1.62 , 18.00 ).

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/v-cuscdt.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_V-CUSCDT ).
       RUN set-position IN h_V-CUSCDT ( 12.27 , 5.00 ).
       /* Size in UIB:  ( 5.77 , 36.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/v-shpinf.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_V-SHPINF ).
       RUN set-position IN h_V-SHPINF ( 12.31 , 44.00 ).
       /* Size in UIB:  ( 5.77 , 39.43 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('3,1') NO-ERROR.

       /* Links to SmartViewer h_v-cusord. */
       RUN add-link IN adm-broker-hdl ( h_Q-ORDCUS , 'Record':U , h_v-cusord ).

       /* Links to SmartQuery h_Q-ORDCUS. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_Q-ORDCUS ).
       RUN add-link IN adm-broker-hdl ( h_p-navico-2 , 'Navigation':U , h_Q-ORDCUS ).
       RUN add-link IN adm-broker-hdl ( h_V-CUSCDT , 'Record':U , h_Q-ORDCUS ).

       /* Links to SmartViewer h_V-CUSCDT. */
       RUN add-link IN adm-broker-hdl ( h_V-CUSADD , 'Record':U , h_V-CUSCDT ).

       /* Links to SmartViewer h_V-SHPINF. */
       RUN add-link IN adm-broker-hdl ( h_Q-ORDCUS , 'Record':U , h_V-SHPINF ).

    END. /* Page 2 */

    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/v-cusord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_v-cusord-2 ).
       RUN set-position IN h_v-cusord-2 ( 10.42 , 5.00 ).
       /* Size in UIB:  ( 1.88 , 28.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-updsav.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'SmartPanelType = Save,
                     Edge-Pixels = 2,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 10.42 , 33.00 ).
       RUN set-size IN h_p-updsav ( 1.88 , 38.00 ).

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'SmartPanelType = NAV-ICON,
                     Right-To-Left = First-On-Left,
                     Edge-Pixels = 2':U ,
             OUTPUT h_p-navico-2 ).
       RUN set-position IN h_p-navico-2 ( 10.42 , 71.00 ).
       RUN set-size IN h_p-navico-2 ( 1.88 , 16.00 ).

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/b-linord.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_B-LINORD ).
       RUN set-position IN h_B-LINORD ( 12.58 , 5.00 ).
       /* Size in UIB:  ( 5.42 , 81.00 ) */

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('2') NO-ERROR.

       /* Links to SmartViewer h_v-cusord-2. */
       RUN add-link IN adm-broker-hdl ( h_Q-ORDCUS , 'Record':U , h_v-cusord-2 ).

       /* Links to SmartBrowser h_B-LINORD. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_B-LINORD ).
       RUN add-link IN adm-broker-hdl ( h_Q-ORDCUS , 'Record':U , h_B-LINORD ).

    END. /* Page 3 */

    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/v-spin.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_V-SPIN ).
       RUN set-position IN h_V-SPIN ( 10.15 , 7.00 ).
       /* Size in UIB:  ( 1.50 , 27.14 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/v-itminf.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_V-ITMINF ).
       RUN set-position IN h_V-ITMINF ( 11.77 , 7.00 ).
       /* Size in UIB:  ( 6.08 , 79.14 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/q-itmcat.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_Q-ITMCAT ).
       /* Position in UIB:  ( 16.00 , 4.00 ) */
       /* Size in UIB:  ( 1.31 , 5.43 ) */

       /* Links to SmartViewer h_V-SPIN. */
       RUN add-link IN adm-broker-hdl ( h_Q-ITMCAT , 'Record':U , h_V-SPIN ).

       /* Links to SmartViewer h_V-ITMINF. */
       RUN add-link IN adm-broker-hdl ( h_Q-ITMCAT , 'Record':U , h_V-ITMINF ).

    END. /* Page 4 */

    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/samples/w-cusupd.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  '':U ,
             OUTPUT h_w-cusupd ).
       /* Position in UIB:  ( 2.77 , 2.00 ) */
       /* Size in UIB:  ( 1.31 , 5.43 ) */

       /* Links to SmartWindow h_w-cusupd. */
       RUN add-link IN adm-broker-hdl ( h_B-CUSLKP , 'Record':U , h_w-cusupd ).
       RUN add-link IN adm-broker-hdl ( h_B-CUSLKP , 'State':U , h_w-cusupd ).

    END. /* Page 5 */

  END CASE.

  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the record-
               source has a new row available.  This procedure
               tries to get the new row and display it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* ADM-ROW-AVAILABLE does nothing because there are no External
     Tables specified for this SmartWindow.
     (See 'Procedure Settings' dialog to set External Tables.) */

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
  ENABLE Btn_updt-cus BTN-EXIT 
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


