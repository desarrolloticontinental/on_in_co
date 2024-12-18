&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
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
/* MOVIMIENTO RESERVADO */
DEF INPUT PARAMETER pCodMov AS CHAR.

/* Local Variable Definitions ---                                       */
DEFINE SHARED VAR S-CODALM AS CHAR.
DEFINE SHARED VAR S-DESALM AS CHAR.
DEFINE SHARED VAR S-CODCIA AS INTEGER.
DEFINE SHARED VAR S-CODDIV AS CHAR.

DEFINE NEW SHARED VARIABLE S-TPOMOV AS CHAR INIT 'S'.   /* Salidas */
DEFINE NEW SHARED VARIABLE C-CODMOV AS CHAR.    /* Mov V�lidos */
DEFINE NEW SHARED VARIABLE L-NROSER AS CHAR.    /* N� Serie V�lidos */

FIND FIRST Almtmov WHERE Almtmov.CodCia = S-CODCIA 
    AND Almtmovm.TipMov = S-TPOMOV 
    AND Almtmovm.CodMov = INTEGER (pCodMov)
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE Almtmovm THEN DO:
    MESSAGE 'El c�digo de movimiento de salida' pCodMov SKIP
        'NO est� definido en la tabla de movimientos'
        VIEW-AS ALERT-BOX WARNING.
    RETURN ERROR.
END.
IF Almtmovm.Indicador[1] = NO THEN DO:
    MESSAGE 'El c�digo de movimiento de salida' pCodMov SKIP
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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-salida AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv05 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv95 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-alcmov AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-salida AS HANDLE NO-UNDO.
DEFINE VARIABLE h_tsalidausointerno AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-movmto AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vsalidausointerno AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 103 BY 1.73
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 103 BY 6.15.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RECT-2 AT ROW 1 COL 1 WIDGET-ID 4
     RECT-4 AT ROW 2.73 COL 1 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.29 BY 20.58
         FONT 4.


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
         TITLE              = "Salidas de Almacen"
         HEIGHT             = 20.58
         WIDTH              = 103.29
         MAX-HEIGHT         = 21.31
         MAX-WIDTH          = 114.43
         VIRTUAL-HEIGHT     = 21.31
         VIRTUAL-WIDTH      = 114.43
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
ON END-ERROR OF W-Win /* Salidas de Almacen */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Salidas de Almacen */
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
/* VM - INCLUDE PARA LA CREACION DEL MENU BAR */
/*{src/adm/template/cntnrwin.i}*/

  ASSIGN lh_Handle = THIS-PROCEDURE.

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
             INPUT  'alm/v-movmto.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-movmto ).
       RUN set-position IN h_v-movmto ( 1.19 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.27 , 58.29 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 1.19 , 71.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.31 , 18.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico-2 ).
       RUN set-position IN h_p-navico-2 ( 2.92 , 23.00 ) NO-ERROR.
       RUN set-size IN h_p-navico-2 ( 1.35 , 18.57 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm-vm/objects/p-updv05.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv05 ).
       RUN set-position IN h_p-updv05 ( 2.92 , 41.00 ) NO-ERROR.
       RUN set-size IN h_p-updv05 ( 1.31 , 49.43 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vsalidausointerno.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_vsalidausointerno ).
       RUN set-position IN h_vsalidausointerno ( 4.46 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 4.08 , 85.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'alm/q-salida.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-salida ).
       RUN set-position IN h_q-salida ( 1.23 , 60.43 ) NO-ERROR.
       /* Size in UIB:  ( 1.08 , 9.57 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'alm/q-alcmov.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-alcmov ).
       RUN set-position IN h_q-alcmov ( 3.08 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.12 , 20.29 ) */

       /* Links to SmartViewer h_v-movmto. */
       RUN add-link IN adm-broker-hdl ( h_q-salida , 'Record':U , h_v-movmto ).

       /* Links to SmartViewer h_vsalidausointerno. */
       RUN add-link IN adm-broker-hdl ( h_p-updv05 , 'TableIO':U , h_vsalidausointerno ).
       RUN add-link IN adm-broker-hdl ( h_q-alcmov , 'Record':U , h_vsalidausointerno ).

       /* Links to SmartQuery h_q-salida. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_q-salida ).

       /* Links to SmartQuery h_q-alcmov. */
       RUN add-link IN adm-broker-hdl ( h_p-navico-2 , 'Navigation':U , h_q-alcmov ).
       RUN add-link IN adm-broker-hdl ( h_q-salida , 'Record':U , h_q-alcmov ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             h_v-movmto , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico-2 ,
             h_p-navico , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv05 ,
             h_p-navico-2 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_vsalidausointerno ,
             h_p-updv05 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_q-salida ,
             h_vsalidausointerno , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_q-alcmov ,
             h_q-salida , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'alm/b-salida.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-salida ).
       RUN set-position IN h_b-salida ( 8.88 , 1.00 ) NO-ERROR.
       RUN set-size IN h_b-salida ( 11.92 , 103.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-salida. */
       RUN add-link IN adm-broker-hdl ( h_q-alcmov , 'Record':U , h_b-salida ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-salida ,
             h_vsalidausointerno , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'tsalidausointerno.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_tsalidausointerno ).
       RUN set-position IN h_tsalidausointerno ( 8.88 , 2.00 ) NO-ERROR.
       RUN set-size IN h_tsalidausointerno ( 12.50 , 88.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'src/adm-vm/objects/p-updv95.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv95 ).
       RUN set-position IN h_p-updv95 ( 9.46 , 91.00 ) NO-ERROR.
       RUN set-size IN h_p-updv95 ( 4.81 , 10.00 ) NO-ERROR.

       /* Links to SmartBrowser h_tsalidausointerno. */
       RUN add-link IN adm-broker-hdl ( h_p-updv95 , 'TableIO':U , h_tsalidausointerno ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_tsalidausointerno ,
             h_vsalidausointerno , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv95 ,
             h_tsalidausointerno , 'AFTER':U ).
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 2 ).

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
  ENABLE RECT-2 RECT-4 
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
  ASSIGN {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " [ " + S-CODALM + " - " + S-DESALM + " ]".
  RUN Procesa-Handle("Pagina1").

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
DEFINE INPUT PARAMETER L-Handle AS CHAR.
CASE L-Handle:
    WHEN "browse"   THEN DO:
          RUN dispatch IN h_b-salida ('open-query':U).
          RUN dispatch IN h_tsalidausointerno ('open-query':U).
       END.
    WHEN "Inactiva" THEN RUN dispatch IN h_b-salida ('hide':U).
    WHEN "Activa"   THEN RUN dispatch IN h_b-salida ('view':U).
    WHEN "Pagina1"  THEN DO:
          RUN select-page(1).
          RUN dispatch IN h_b-salida ('open-query':U).
      END.
    WHEN "Pagina2"  THEN DO:
          RUN select-page(2).
          RUN dispatch IN h_tsalidausointerno ('open-query':U).
      END.
    WHEN "Carga-Series" THEN RUN Carga-Series IN h_q-salida.
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

