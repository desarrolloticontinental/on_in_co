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
DEF INPUT PARAMETER pParam AS CHAR.
/* Local Variable Definitions ---                                       */

DEFINE NEW SHARED VAR s-acceso-total  AS LOG INIT NO NO-UNDO.

ASSIGN s-acceso-total = LOGICAL(pParam) NO-ERROR.

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
DEFINE VARIABLE h_b-consulta-precios-may AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-lista-contrato-marco AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-lista-minorista-gral-v2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-lista-remates AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-listas-mayorista-division-v2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-listas-mayorista-eventos-v2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-listas-mayorista-gral AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv12 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv12-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv12-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv12-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv12-5 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv97 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_tab95 AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 192.72 BY 26.38 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 7
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "LISTA DE PRECIOS MAYORISTA Y UTILEX"
         HEIGHT             = 26.38
         WIDTH              = 192.72
         MAX-HEIGHT         = 26.69
         MAX-WIDTH          = 192.72
         VIRTUAL-HEIGHT     = 26.69
         VIRTUAL-WIDTH      = 192.72
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
ON END-ERROR OF W-Win /* LISTA DE PRECIOS MAYORISTA Y UTILEX */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* LISTA DE PRECIOS MAYORISTA Y UTILEX */
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
             INPUT  'src/adm-free/objects/tab95.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'LABEL-FONT = 4,
                     LABEL-FGCOLOR = 0,
                     FOLDER-BGCOLOR = 8,
                     FOLDER-PARENT-BGCOLOR = 8,
                     LABELS = Mayorista|Consulta de Precios|Eventos|Por Divisi�n|Contrato Marco|Remates|UTILEX':U ,
             OUTPUT h_tab95 ).
       RUN set-position IN h_tab95 ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_tab95 ( 26.38 , 192.00 ) NO-ERROR.

       /* Links to SmartTab95 h_tab95. */
       RUN add-link IN adm-broker-hdl ( h_tab95 , 'Page':U , THIS-PROCEDURE ).

    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'PRI/b-listas-mayorista-gral-v2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-listas-mayorista-gral ).
       RUN set-position IN h_b-listas-mayorista-gral ( 2.08 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-listas-mayorista-gral ( 23.15 , 189.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv97.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv97 ).
       RUN set-position IN h_p-updv97 ( 25.23 , 2.00 ) NO-ERROR.
       RUN set-size IN h_p-updv97 ( 1.54 , 34.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-listas-mayorista-gral. */
       RUN add-link IN adm-broker-hdl ( h_p-updv97 , 'TableIO':U , h_b-listas-mayorista-gral ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-listas-mayorista-gral ,
             h_tab95 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv97 ,
             h_b-listas-mayorista-gral , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/pri/b-consulta-precios-may.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-consulta-precios-may ).
       RUN set-position IN h_b-consulta-precios-may ( 2.08 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-consulta-precios-may ( 25.04 , 188.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-consulta-precios-may ,
             h_tab95 , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/pri/b-listas-mayorista-eventos-v2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-listas-mayorista-eventos-v2 ).
       RUN set-position IN h_b-listas-mayorista-eventos-v2 ( 2.08 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-listas-mayorista-eventos-v2 ( 23.69 , 167.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv12.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv12-2 ).
       RUN set-position IN h_p-updv12-2 ( 25.77 , 2.00 ) NO-ERROR.
       RUN set-size IN h_p-updv12-2 ( 1.42 , 34.14 ) NO-ERROR.

       /* Links to SmartBrowser h_b-listas-mayorista-eventos-v2. */
       RUN add-link IN adm-broker-hdl ( h_p-updv12-2 , 'TableIO':U , h_b-listas-mayorista-eventos-v2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-listas-mayorista-eventos-v2 ,
             h_tab95 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv12-2 ,
             h_b-listas-mayorista-eventos-v2 , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/pri/b-listas-mayorista-division-v2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-listas-mayorista-division-v2 ).
       RUN set-position IN h_b-listas-mayorista-division-v2 ( 2.08 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-listas-mayorista-division-v2 ( 23.69 , 167.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm-vm/objects/p-updv12.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv12 ).
       RUN set-position IN h_p-updv12 ( 25.77 , 2.00 ) NO-ERROR.
       RUN set-size IN h_p-updv12 ( 1.54 , 34.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-listas-mayorista-division-v2. */
       RUN add-link IN adm-broker-hdl ( h_p-updv12 , 'TableIO':U , h_b-listas-mayorista-division-v2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-listas-mayorista-division-v2 ,
             h_tab95 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv12 ,
             h_b-listas-mayorista-division-v2 , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 5 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/pri/b-lista-contrato-marco.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-lista-contrato-marco ).
       RUN set-position IN h_b-lista-contrato-marco ( 2.35 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-lista-contrato-marco ( 22.88 , 160.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv12.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv12-3 ).
       RUN set-position IN h_p-updv12-3 ( 25.23 , 3.00 ) NO-ERROR.
       RUN set-size IN h_p-updv12-3 ( 1.42 , 34.14 ) NO-ERROR.

       /* Links to SmartBrowser h_b-lista-contrato-marco. */
       RUN add-link IN adm-broker-hdl ( h_p-updv12-3 , 'TableIO':U , h_b-lista-contrato-marco ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-lista-contrato-marco ,
             h_tab95 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv12-3 ,
             h_b-lista-contrato-marco , 'AFTER':U ).
    END. /* Page 5 */
    WHEN 6 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/pri/b-lista-remates.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-lista-remates ).
       RUN set-position IN h_b-lista-remates ( 2.35 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-lista-remates ( 22.88 , 120.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv12.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv12-4 ).
       RUN set-position IN h_p-updv12-4 ( 25.23 , 3.00 ) NO-ERROR.
       RUN set-size IN h_p-updv12-4 ( 1.42 , 34.14 ) NO-ERROR.

       /* Links to SmartBrowser h_b-lista-remates. */
       RUN add-link IN adm-broker-hdl ( h_p-updv12-4 , 'TableIO':U , h_b-lista-remates ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-lista-remates ,
             h_tab95 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv12-4 ,
             h_b-lista-remates , 'AFTER':U ).
    END. /* Page 6 */
    WHEN 7 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/pri/b-lista-minorista-gral-v2.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-lista-minorista-gral-v2 ).
       RUN set-position IN h_b-lista-minorista-gral-v2 ( 2.08 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-lista-minorista-gral-v2 ( 23.15 , 170.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv12.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv12-5 ).
       RUN set-position IN h_p-updv12-5 ( 25.23 , 3.00 ) NO-ERROR.
       RUN set-size IN h_p-updv12-5 ( 1.42 , 34.14 ) NO-ERROR.

       /* Links to SmartBrowser h_b-lista-minorista-gral-v2. */
       RUN add-link IN adm-broker-hdl ( h_p-updv12-5 , 'TableIO':U , h_b-lista-minorista-gral-v2 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-lista-minorista-gral-v2 ,
             h_tab95 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv12-5 ,
             h_b-lista-minorista-gral-v2 , 'AFTER':U ).
    END. /* Page 7 */

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

