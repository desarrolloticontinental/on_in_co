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

DEF INPUT PARAMETER pCodMov AS CHAR.

/* ********************************************************************************** */
/* RHC 20/07/2015 definimos s-codcia s-codalm s-desalm y s-coddiv SOLO para ALMACENES */
{alm/windowalmacen.i}
/* ********************************************************************************** */

DEFINE NEW SHARED VARIABLE S-TPOMOV AS CHAR INIT 'I'.   /* Ingresos */
DEFINE NEW SHARED VARIABLE C-CODMOV AS CHAR.    /* Mov V�lidos */
DEFINE NEW SHARED VARIABLE L-NROSER AS CHAR.    /* N� Serie V�lidos */

FIND FIRST Almtmov WHERE Almtmov.CodCia = S-CODCIA 
    AND Almtmovm.TipMov = S-TPOMOV 
    AND Almtmovm.CodMov = INTEGER (pCodMov)
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtmovm THEN DO:
    MESSAGE 'El c�digo de movimiento de ingreso' pCodMov SKIP
        'NO est� definido en la tabla de movimientos'
        VIEW-AS ALERT-BOX WARNING.
    RETURN ERROR.
END.
IF Almtmovm.Indicador[1] = NO THEN DO:
    MESSAGE 'El c�digo de movimiento de ingreso' pCodMov SKIP
        'NO es un movimiento reservado'
        VIEW-AS ALERT-BOX WARNING.
    RETURN ERROR.
END.
ASSIGN
    c-CodMov = TRIM(STRING(Almtmovm.codmov)).
/* CHEQUEAMOS LA CONFIGURACION DE CORRELATIVOS */
FIND FIRST Almtdocm WHERE Almtdocm.CodCia = S-CODCIA AND
     Almtdocm.CodAlm = S-CODALM AND
     Almtdocm.TipMov = S-TPOMOV AND
     LOOKUP(TRIM(STRING(Almtdocm.CodMov)), c-CodMov) > 0 NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtdocm THEN DO:
   MESSAGE "No existen movimientos asignados al almacen" VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
END.

DEFINE NEW SHARED TEMP-TABLE ITEM LIKE almdmov.

DEFINE NEW SHARED VARIABLE S-NROSER  AS INTEGER.
DEFINE NEW SHARED VARIABLE lh_Handle AS HANDLE.
DEFINE NEW SHARED VARIABLE S-MOVVAL  AS LOGICAL.
DEFINE NEW SHARED VARIABLE C-CODALM  AS CHAR.
DEFINE NEW SHARED VARIABLE ORDTRB    AS CHAR.

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
DEFINE VARIABLE h_b-ingres AS HANDLE NO-UNDO.
DEFINE VARIABLE h_f-imp-excel-plantilla AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv05 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv12 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-alcmov AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-ingres AS HANDLE NO-UNDO.
DEFINE VARIABLE h_t-ing-solovalor AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-ingres-solo-valor AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-movmto AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.43 BY 19.88 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 2
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "INGRESO POR AJUSTE DE VALORES"
         HEIGHT             = 19.88
         WIDTH              = 110.43
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
ON END-ERROR OF W-Win /* INGRESO POR AJUSTE DE VALORES */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* INGRESO POR AJUSTE DE VALORES */
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

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/v-movmto.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-movmto ).
       RUN set-position IN h_v-movmto ( 1.00 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.27 , 58.29 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm/objects/p-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 1.00 , 72.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.35 , 17.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/v-ingres-solo-valor.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-ingres-solo-valor ).
       RUN set-position IN h_v-ingres-solo-valor ( 2.35 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.77 , 89.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm/objects/p-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico-2 ).
       RUN set-position IN h_p-navico-2 ( 7.19 , 23.00 ) NO-ERROR.
       RUN set-size IN h_p-navico-2 ( 1.35 , 18.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv05.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv05 ).
       RUN set-position IN h_p-updv05 ( 7.19 , 41.00 ) NO-ERROR.
       RUN set-size IN h_p-updv05 ( 1.42 , 57.86 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/q-ingres.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-ingres ).
       RUN set-position IN h_q-ingres ( 1.00 , 61.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.08 , 10.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/q-alcmov.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-alcmov ).
       RUN set-position IN h_q-alcmov ( 7.19 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.12 , 20.29 ) */

       /* Links to SmartViewer h_v-movmto. */
       RUN add-link IN adm-broker-hdl ( h_q-ingres , 'Record':U , h_v-movmto ).

       /* Links to SmartViewer h_v-ingres-solo-valor. */
       RUN add-link IN adm-broker-hdl ( h_p-updv05 , 'TableIO':U , h_v-ingres-solo-valor ).
       RUN add-link IN adm-broker-hdl ( h_q-alcmov , 'Record':U , h_v-ingres-solo-valor ).

       /* Links to SmartQuery h_q-ingres. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_q-ingres ).

       /* Links to SmartQuery h_q-alcmov. */
       RUN add-link IN adm-broker-hdl ( h_p-navico-2 , 'Navigation':U , h_q-alcmov ).
       RUN add-link IN adm-broker-hdl ( h_q-ingres , 'Record':U , h_q-alcmov ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_v-movmto , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-ingres-solo-valor ,
             h_p-navico , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico-2 ,
             h_v-ingres-solo-valor , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv05 ,
             h_p-navico-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_q-ingres ,
             h_p-updv05 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_q-alcmov ,
             h_q-ingres , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/b-ingres.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-ingres ).
       RUN set-position IN h_b-ingres ( 8.81 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-ingres ( 7.85 , 92.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-ingres. */
       RUN add-link IN adm-broker-hdl ( h_q-alcmov , 'Record':U , h_b-ingres ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-ingres ,
             h_p-updv05 , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/t-ing-solovalor.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_t-ing-solovalor ).
       RUN set-position IN h_t-ing-solovalor ( 8.81 , 2.00 ) NO-ERROR.
       RUN set-size IN h_t-ing-solovalor ( 11.31 , 89.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/alm/f-imp-excel-plantilla.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_f-imp-excel-plantilla ).
       RUN set-position IN h_f-imp-excel-plantilla ( 9.08 , 92.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.08 , 17.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv12.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv12 ).
       RUN set-position IN h_p-updv12 ( 10.42 , 92.00 ) NO-ERROR.
       RUN set-size IN h_p-updv12 ( 5.38 , 10.72 ) NO-ERROR.

       /* Links to SmartBrowser h_t-ing-solovalor. */
       RUN add-link IN adm-broker-hdl ( h_p-updv12 , 'TableIO':U , h_t-ing-solovalor ).
       RUN add-link IN adm-broker-hdl ( h_q-ingres , 'Record':U , h_t-ing-solovalor ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_t-ing-solovalor ,
             h_p-updv05 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_f-imp-excel-plantilla ,
             h_t-ing-solovalor , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv12 ,
             h_f-imp-excel-plantilla , 'AFTER':U ).
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

DEF INPUT PARAMETER pParam AS CHAR NO-UNDO.

CASE pParam:
    WHEN "Importar-Excel" THEN DO:
        RUN Importar-Excel IN h_t-ing-solovalor.
    END.
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Handle W-Win 
PROCEDURE Procesa-Handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER pParam AS CHAR.

CASE pParam:
    WHEN "browse" THEN DO:
        IF h_b-ingres <> ? THEN RUN dispatch IN h_b-ingres ('open-query':U).
        IF h_t-ing-solovalor <> ? THEN RUN dispatch IN h_t-ing-solovalor ('open-query':U).
    END.
    WHEN "pagina1" THEN DO:
        RUN select-page(1).
        RUN dispatch IN h_b-ingres ('open-query':U).
        RUN dispatch IN h_q-alcmov ('enable':U).
    END.
    WHEN "pagina2" THEN DO:
        RUN select-page(2).
        RUN dispatch IN h_t-ing-solovalor ('open-query':U).
        RUN dispatch IN h_q-alcmov ('disable':U).
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
