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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-adcto-vol-time AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-edcto-vol-time AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-mdcto-vol-time AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-pdcto-vol-time AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-sdcto-vol-time AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv10 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv10-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv10-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv10-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv10-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_tab95 AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160.43 BY 26.15
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 3
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "DESCUENTO POR VOLUMEN ACUMULADO POR PERIODO DE TIEMPO"
         HEIGHT             = 26.15
         WIDTH              = 160.43
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
ON END-ERROR OF W-Win /* DESCUENTO POR VOLUMEN ACUMULADO POR PERIODO DE TIEMPO */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* DESCUENTO POR VOLUMEN ACUMULADO POR PERIODO DE TIEMPO */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
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
             INPUT  'aplic/pri/b-mdcto-vol-time.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-mdcto-vol-time ).
       RUN set-position IN h_b-mdcto-vol-time ( 1.27 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-mdcto-vol-time ( 7.77 , 109.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv10.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_p-updv10 ).
       RUN set-position IN h_p-updv10 ( 3.15 , 112.00 ) NO-ERROR.
       RUN set-size IN h_p-updv10 ( 5.38 , 10.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-free/objects/tab95.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'LABEL-FONT = 4,
                     LABEL-FGCOLOR = 0,
                     FOLDER-BGCOLOR = 8,
                     FOLDER-PARENT-BGCOLOR = 8,
                     LABELS = Pack de Productos|Productos x Pack|Divisiones Hijas':U ,
             OUTPUT h_tab95 ).
       RUN set-position IN h_tab95 ( 9.35 , 2.00 ) NO-ERROR.
       RUN set-size IN h_tab95 ( 17.50 , 154.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-mdcto-vol-time. */
       RUN add-link IN adm-broker-hdl ( h_p-updv10 , 'TableIO':U , h_b-mdcto-vol-time ).

       /* Links to SmartTab95 h_tab95. */
       RUN add-link IN adm-broker-hdl ( h_tab95 , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv10 ,
             h_b-mdcto-vol-time , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_tab95 ,
             h_p-updv10 , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/pri/b-pdcto-vol-time.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-pdcto-vol-time ).
       RUN set-position IN h_b-pdcto-vol-time ( 10.69 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-pdcto-vol-time ( 6.69 , 103.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv10.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_p-updv10-3 ).
       RUN set-position IN h_p-updv10-3 ( 11.50 , 108.00 ) NO-ERROR.
       RUN set-size IN h_p-updv10-3 ( 5.38 , 10.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/pri/b-edcto-vol-time.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-edcto-vol-time ).
       RUN set-position IN h_b-edcto-vol-time ( 17.69 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-edcto-vol-time ( 8.62 , 72.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv10.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_p-updv10-5 ).
       RUN set-position IN h_p-updv10-5 ( 18.50 , 77.00 ) NO-ERROR.
       RUN set-size IN h_p-updv10-5 ( 5.38 , 10.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-pdcto-vol-time. */
       RUN add-link IN adm-broker-hdl ( h_b-mdcto-vol-time , 'Record':U , h_b-pdcto-vol-time ).
       RUN add-link IN adm-broker-hdl ( h_p-updv10-3 , 'TableIO':U , h_b-pdcto-vol-time ).

       /* Links to SmartBrowser h_b-edcto-vol-time. */
       RUN add-link IN adm-broker-hdl ( h_b-pdcto-vol-time , 'Record':U , h_b-edcto-vol-time ).
       RUN add-link IN adm-broker-hdl ( h_p-updv10-5 , 'TableIO':U , h_b-edcto-vol-time ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-pdcto-vol-time ,
             h_tab95 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv10-3 ,
             h_b-pdcto-vol-time , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-edcto-vol-time ,
             h_p-updv10-3 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv10-5 ,
             h_b-edcto-vol-time , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/pri/b-adcto-vol-time.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-adcto-vol-time ).
       RUN set-position IN h_b-adcto-vol-time ( 10.69 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-adcto-vol-time ( 15.88 , 127.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv10.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_p-updv10-4 ).
       RUN set-position IN h_p-updv10-4 ( 11.50 , 132.00 ) NO-ERROR.
       RUN set-size IN h_p-updv10-4 ( 5.38 , 10.00 ) NO-ERROR.

       /* Initialize other pages that this page requires. */
       RUN init-pages IN THIS-PROCEDURE ('1':U) NO-ERROR.

       /* Links to SmartBrowser h_b-adcto-vol-time. */
       RUN add-link IN adm-broker-hdl ( h_b-pdcto-vol-time , 'Record':U , h_b-adcto-vol-time ).
       RUN add-link IN adm-broker-hdl ( h_p-updv10-4 , 'TableIO':U , h_b-adcto-vol-time ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-adcto-vol-time ,
             h_tab95 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv10-4 ,
             h_b-adcto-vol-time , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/pri/b-sdcto-vol-time.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-sdcto-vol-time ).
       RUN set-position IN h_b-sdcto-vol-time ( 10.69 , 4.00 ) NO-ERROR.
       RUN set-size IN h_b-sdcto-vol-time ( 6.69 , 113.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv10.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = Multiple-Records':U ,
             OUTPUT h_p-updv10-2 ).
       RUN set-position IN h_p-updv10-2 ( 11.50 , 118.00 ) NO-ERROR.
       RUN set-size IN h_p-updv10-2 ( 5.38 , 10.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-sdcto-vol-time. */
       RUN add-link IN adm-broker-hdl ( h_b-mdcto-vol-time , 'Record':U , h_b-sdcto-vol-time ).
       RUN add-link IN adm-broker-hdl ( h_p-updv10-2 , 'TableIO':U , h_b-sdcto-vol-time ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-sdcto-vol-time ,
             h_tab95 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv10-2 ,
             h_b-sdcto-vol-time , 'AFTER':U ).
    END. /* Page 3 */

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
  VIEW FRAME F-Main IN WINDOW W-Win.
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

