&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE PEDI NO-UNDO LIKE FacDPedi.
DEFINE NEW SHARED TEMP-TABLE T-CPEDI NO-UNDO LIKE FacCPedi.



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
DEFINE NEW SHARED VARIABLE s-coddoc   AS CHAR INITIAL "O/D".
DEFINE NEW SHARED VARIABLE s-codref   AS CHAR INITIAL "P/M".
DEFINE NEW SHARED VARIABLE lh_Handle  AS HANDLE.
DEFINE NEW SHARED VARIABLE s-NroSer AS INTEGER.
DEFINE NEW SHARED VARIABLE s-FlgPicking LIKE GN-DIVI.FlgPicking.
DEFINE NEW SHARED VARIABLE s-FlgBarras LIKE GN-DIVI.FlgBarras.

DEFINE SHARED VARIABLE S-CODCIA   AS INTEGER.
DEFINE SHARED VARIABLE S-CODDIV   AS CHAR.
DEFINE SHARED VARIABLE S-CODVEN   AS CHAR.
DEFINE SHARED VARIABLE S-USER-ID  AS CHAR.
DEFINE SHARED VARIABLE CL-CODCIA  AS INTEGER.

FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
     FacCorre.CodDoc = S-CODDOC AND
     FacCorre.CodDiv = S-CODDIV 
     NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
   MESSAGE "Codigo de Documento no configurado" VIEW-AS ALERT-BOX WARNING.
   RETURN.
END.

/* PARAMETROS DE COTIZACION PARA LA DIVISION */
FIND gn-divi WHERE gn-divi.codcia = s-codcia
    AND gn-divi.coddiv = s-coddiv
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-divi THEN DO:
    MESSAGE 'Divisi�n' s-coddiv 'NO configurada' VIEW-AS ALERT-BOX WARNING.
    RETURN.
END.
ASSIGN
    s-FlgPicking = GN-DIVI.FlgPicking
    s-FlgBarras  = GN-DIVI.FlgBarras.

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
&Scoped-Define ENABLED-OBJECTS COMBO-NroSer B-AGTRANS 
&Scoped-Define DISPLAYED-OBJECTS COMBO-NroSer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-pedido-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-orddes-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_t-orddes-4 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-orddes-4 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-AGTRANS 
     LABEL "Transportista" 
     SIZE 14 BY 1.

DEFINE VARIABLE COMBO-NroSer AS CHARACTER FORMAT "X(3)":U INITIAL "0" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 6.72 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     COMBO-NroSer AT ROW 1.27 COL 2 NO-LABEL WIDGET-ID 4
     B-AGTRANS AT ROW 1.27 COL 89 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 137.86 BY 23.81
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 2
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: PEDI T "NEW SHARED" NO-UNDO INTEGRAL FacDPedi
      TABLE: T-CPEDI T "NEW SHARED" NO-UNDO INTEGRAL FacCPedi
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "ORDEN DE DESPACHO VENTA MOSTRADOR"
         HEIGHT             = 23.81
         WIDTH              = 137.86
         MAX-HEIGHT         = 24.62
         MAX-WIDTH          = 142.29
         VIRTUAL-HEIGHT     = 24.62
         VIRTUAL-WIDTH      = 142.29
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
/* SETTINGS FOR COMBO-BOX COMBO-NroSer IN FRAME F-Main
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* ORDEN DE DESPACHO VENTA MOSTRADOR */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* ORDEN DE DESPACHO VENTA MOSTRADOR */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-AGTRANS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-AGTRANS W-Win
ON CHOOSE OF B-AGTRANS IN FRAME F-Main /* Transportista */
DO:
   RUN Asigna-datos IN h_v-orddes-4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-NroSer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-NroSer W-Win
ON VALUE-CHANGED OF COMBO-NroSer IN FRAME F-Main
DO:
    ASSIGN COMBO-NroSer.
    s-NroSer = INTEGER(COMBO-NroSer).
    RUN dispatch IN h_q-orddes-4 ('open-query':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
/* VM - INCLUDE PARA LA CREACION DEL MENU BAR */
/*{src/adm/template/cntnrwin.i}*/

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
             INPUT  'adm/objects/p-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 1.00 , 18.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.54 , 18.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vtamay/v-orddes-4.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-orddes-4 ).
       RUN set-position IN h_v-orddes-4 ( 2.62 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.35 , 98.86 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vtamay/q-orddes-4.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-orddes-4 ).
       RUN set-position IN h_q-orddes-4 ( 1.00 , 9.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.35 , 9.00 ) */

       /* Links to SmartViewer h_v-orddes-4. */
       RUN add-link IN adm-broker-hdl ( h_q-orddes-4 , 'Record':U , h_v-orddes-4 ).

       /* Links to SmartQuery h_q-orddes-4. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_q-orddes-4 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             COMBO-NroSer:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-orddes-4 ,
             B-AGTRANS:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_q-orddes-4 ,
             h_v-orddes-4 , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vtamay/b-pedido-4.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-pedido-4 ).
       RUN set-position IN h_b-pedido-4 ( 11.23 , 1.00 ) NO-ERROR.
       RUN set-size IN h_b-pedido-4 ( 13.38 , 136.00 ) NO-ERROR.

       /* Links to SmartBrowser h_b-pedido-4. */
       RUN add-link IN adm-broker-hdl ( h_q-orddes-4 , 'Record':U , h_b-pedido-4 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-pedido-4 ,
             h_v-orddes-4 , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vtamay/t-orddes-4.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_t-orddes-4 ).
       RUN set-position IN h_t-orddes-4 ( 11.23 , 1.00 ) NO-ERROR.
       RUN set-size IN h_t-orddes-4 ( 13.38 , 137.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_t-orddes-4 ,
             h_v-orddes-4 , 'AFTER':U ).
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
  DISPLAY COMBO-NroSer 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE COMBO-NroSer B-AGTRANS 
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
  DEFINE VARIABLE cListItems AS CHARACTER NO-UNDO.

  FIND FacCfgGn WHERE FacCfgGn.CodCia = s-CodCia NO-LOCK NO-ERROR.
  FOR EACH FacCorre NO-LOCK WHERE 
      FacCorre.CodCia = s-CodCia AND
      FacCorre.CodDoc = s-CodDoc AND
      FacCorre.CodDiv = s-CodDiv:
      IF cListItems = "" THEN cListItems = STRING(FacCorre.NroSer,"999").
      ELSE cListItems = cListItems + "," + STRING(FacCorre.NroSer,"999").
  END.
  DO WITH FRAME {&FRAME-NAME}:
      COMBO-NroSer:LIST-ITEMS = cListItems.
      COMBO-NroSer = ENTRY(1,COMBO-NroSer:LIST-ITEMS).
      s-NroSer = INTEGER(COMBO-NroSer).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN lh_Handle = THIS-PROCEDURE.
  RUN Procesa-Handle ("Pagina1").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-handle W-Win 
PROCEDURE Procesa-handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER L-Handle AS CHAR.
CASE L-Handle:
    WHEN "browse" THEN DO:
          IF h_b-pedido-4 <> ? THEN RUN dispatch IN h_b-pedido-4 ('open-query':U).
          IF h_t-orddes-4 <> ? THEN RUN dispatch IN h_t-orddes-4 ('open-query':U).
      END.
    WHEN "Pagina1"  THEN DO WITH FRAME {&FRAME-NAME}:
         RUN select-page(1).
         COMBO-NroSer:SENSITIVE = YES.
         B-AGTRANS:SENSITIVE = YES.
      END.
    WHEN "Pagina2"  THEN DO WITH FRAME {&FRAME-NAME}:
         RUN select-page(2).
         COMBO-NroSer:SENSITIVE = NO.
         B-AGTRANS:SENSITIVE = NO.
      END.
    WHEN "Recalculo" THEN DO WITH FRAME {&FRAME-NAME}:
        RUN Recalcular-Precios IN h_t-orddes-4.
      END.
/*     WHEN "Disable-Head" THEN DO:                  */
/*         RUN dispatch IN h_p-updv05 ('disable':U). */
/*       END.                                        */
/*     WHEN "Enable-Head" THEN DO:                   */
/*         RUN dispatch IN h_p-updv05 ('enable':U).  */
/*       END.                                        */
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

