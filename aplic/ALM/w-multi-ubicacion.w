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

DEF SHARED VAR s-codcia AS INTE.
DEF SHARED VAR s-coddiv AS CHAR.
DEF SHARED VAR s-codalm AS CHAR.

DEF NEW SHARED VAR lh_handle AS HANDLE.

DEF VAR pRowid AS ROWID NO-UNDO.

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
DEFINE VARIABLE h_b-multi-ubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-multi-ubic-depurar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-multi-ubic-graba-reubicar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-multi-ubic-no-reubicar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-multi-ubic-reubicar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-multi-ubicacion AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv10 AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 64.86 BY 24
         FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "MAESTRO DE MULTIUBICACIONES"
         HEIGHT             = 24
         WIDTH              = 64.86
         MAX-HEIGHT         = 26.15
         MAX-WIDTH          = 191.29
         VIRTUAL-HEIGHT     = 26.15
         VIRTUAL-WIDTH      = 191.29
         MAX-BUTTON         = no
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
ON END-ERROR OF W-Win /* MAESTRO DE MULTIUBICACIONES */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* MAESTRO DE MULTIUBICACIONES */
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
lh_handle = THIS-PROCEDURE.

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

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/b-multi-ubicacion.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-multi-ubicacion ).
       RUN set-position IN h_b-multi-ubicacion ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_b-multi-ubicacion ( 22.27 , 64.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv10.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv10 ).
       RUN set-position IN h_p-updv10 ( 23.35 , 2.00 ) NO-ERROR.
       RUN set-size IN h_p-updv10 ( 1.42 , 39.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/f-multi-ubic-reubicar.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-multi-ubic-reubicar ).
       RUN set-position IN h_f-multi-ubic-reubicar ( 23.35 , 41.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.35 , 12.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/f-multi-ubic-depurar.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-multi-ubic-depurar ).
       RUN set-position IN h_f-multi-ubic-depurar ( 23.35 , 53.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.35 , 12.00 ) */

       /* Links to SmartBrowser h_b-multi-ubicacion. */
       RUN add-link IN adm-broker-hdl ( h_p-updv10 , 'TableIO':U , h_b-multi-ubicacion ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv10 ,
             h_b-multi-ubicacion , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_f-multi-ubic-reubicar ,
             h_p-updv10 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_f-multi-ubic-depurar ,
             h_f-multi-ubic-reubicar , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/f-multi-ubicacion.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-multi-ubicacion ).
       RUN set-position IN h_f-multi-ubicacion ( 1.27 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.38 , 61.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/f-multi-ubic-graba-reubicar.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-multi-ubic-graba-reubicar ).
       RUN set-position IN h_f-multi-ubic-graba-reubicar ( 8.81 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.35 , 26.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/f-multi-ubic-no-reubicar.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-multi-ubic-no-reubicar ).
       RUN set-position IN h_f-multi-ubic-no-reubicar ( 8.81 , 28.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.35 , 26.00 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_f-multi-ubic-graba-reubicar ,
             h_f-multi-ubicacion , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_f-multi-ubic-no-reubicar ,
             h_f-multi-ubic-graba-reubicar , 'AFTER':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Botones W-Win 
PROCEDURE Procesa-Botones :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER pParam AS CHAR.

CASE pParam:
    WHEN "Reubicar" THEN DO:
        RUN Devuelve-Parametros IN h_b-multi-ubicacion ( OUTPUT pRowid /* ROWID */).
        IF pRowid <> ? THEN DO:

            FIND Almubimat WHERE ROWID(Almubimat) = pRowid NO-LOCK NO-ERROR.
            IF AVAILABLE Almubimat THEN DO:
                DEFINE VAR hLibAlm AS HANDLE NO-UNDO.

                RUN alm/almacen-library PERSISTENT SET hLibAlm.
                RUN ALM_Tipo-MultiUbic IN hLibAlm (almubimat.CodAlm,
                                                   almubimat.CodMat,
                                                   almubimat.CodUbi).
                IF RETURN-VALUE = 'PRINCIPAL' THEN DO:
                    MESSAGE 'NO se puede reubicar ubicación principal'
                        VIEW-AS ALERT-BOX ERROR.
                    RETURN 'ADM-ERROR'.
                END.
                DELETE PROCEDURE hLibAlm.
            END.

            RUN select-page('2').
            RUN Captura-Parametros IN h_f-multi-ubicacion ( INPUT pRowid /* ROWID */).
        END.
    END.
    WHEN "Depurar" THEN DO:
        RUN Depurar IN h_b-multi-ubicacion.
    END.
    WHEN "Graba-Reubicacion" THEN DO:
        RUN Grabar IN h_f-multi-ubicacion.
        IF RETURN-VALUE = 'ADM-ERROR' THEN RETURN.
        RUN select-page('1').
        RUN Ubica-Registro IN h_b-multi-ubicacion ( INPUT pRowid /* ROWID */).
    END.
    WHEN "No-Reubicacion" THEN DO:
        RUN select-page('1').
    END.
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

