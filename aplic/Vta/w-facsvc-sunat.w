&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE DETA LIKE CcbDDocu.



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
DEF NEW SHARED VAR s-CodDoc AS CHAR INIT 'FAC'.
DEF NEW SHARED VAR s-CodRef AS CHAR INIT 'GRS'.
DEF NEW SHARED VAR s-NroSer AS INT.
DEF NEW SHARED VAR Lh_Handle AS HANDLE.
DEF NEW SHARED VAR s-TpoFac AS CHAR INIT 'S'.

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-coddiv AS CHAR.
DEF NEW SHARED VAR s-codalm AS CHAR.

/* TODOS los Almacenes */
FOR EACH VtaAlmDiv NO-LOCK WHERE Vtaalmdiv.codcia = s-codcia
    AND Vtaalmdiv.coddiv = s-coddiv,
    FIRST Almacen OF Vtaalmdiv NO-LOCK 
    BY VtaAlmDiv.Orden:
    IF s-CodAlm = "" THEN s-CodAlm = TRIM(VtaAlmDiv.CodAlm).
    ELSE s-CodAlm = s-CodAlm + "," + TRIM(VtaAlmDiv.CodAlm).
END.
IF s-CodAlm = "" THEN DO:
    MESSAGE 'NO se han definido los almacenes de ventas para la divisi�n' s-coddiv VIEW-AS ALERT-BOX WARNING.
    RETURN ERROR.
END.
s-CodAlm = ENTRY(1, s-CodAlm).


FIND FIRST FacCorre WHERE Faccorre.codcia = s-codcia
    AND LOOKUP(Faccorre.coddoc, 'FAC,BOL') > 0
    AND Faccorre.coddiv = s-coddiv
    AND Faccorre.codalm = s-codalm
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
    MESSAGE 'Correlativo NO configurado para esta divisi�n'
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
s-CodDoc = FacCorre.CodDoc.
s-NroSer = FacCorre.NroSer.
FIND FIRST FacCorre WHERE Faccorre.codcia = s-codcia
    AND Faccorre.coddoc = s-coddoc
    AND Faccorre.coddiv = s-coddiv
    AND FacCorre.FlgEst = YES
    AND Faccorre.codalm = s-codalm
    NO-LOCK NO-ERROR.
IF AVAILABLE FacCorre THEN s-NroSer = FacCorre.NroSer.

DEF NEW SHARED VAR s-Sunat-Activo AS LOG INIT NO.
FIND gn-divi WHERE GN-DIVI.CodCia = s-codcia 
    AND GN-DIVI.CodDiv = s-coddiv NO-LOCK NO-ERROR.
IF NOT AVAILABLE gn-divi THEN DO:
    MESSAGE 'NO est� configurada la divisi�n' s-coddiv VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.
s-Sunat-Activo = gn-divi.campo-log[10].

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
DEFINE VARIABLE h_b-facsvc-sunat AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv04 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-cdocu AS HANDLE NO-UNDO.
DEFINE VARIABLE h_t-facsvc AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-facsvc-sunat AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-totfac-sunat AS HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.29 BY 18.77
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Design Page: 1
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: DETA T "NEW SHARED" ? INTEGRAL CcbDDocu
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "FACTURACION DE SERVICIOS"
         HEIGHT             = 18.81
         WIDTH              = 100.43
         MAX-HEIGHT         = 19.81
         MAX-WIDTH          = 106.86
         VIRTUAL-HEIGHT     = 19.81
         VIRTUAL-WIDTH      = 106.86
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
ON END-ERROR OF W-Win /* FACTURACION DE SERVICIOS */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* FACTURACION DE SERVICIOS */
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
{src/adm/template/cntnrwin.i}

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
             INPUT  'adm/objects/p-navico.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Right':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 1.00 , 25.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.54 , 18.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm-vm/objects/p-updv04.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv04 ).
       RUN set-position IN h_p-updv04 ( 1.00 , 43.00 ) NO-ERROR.
       RUN set-size IN h_p-updv04 ( 1.54 , 56.14 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vta/v-facsvc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-facsvc-sunat ).
       RUN set-position IN h_v-facsvc-sunat ( 2.54 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.69 , 98.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta/q-cdocu.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-cdocu ).
       RUN set-position IN h_q-cdocu ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.27 , 23.43 ) */

       /* Links to SmartViewer h_v-facsvc-sunat. */
       RUN add-link IN adm-broker-hdl ( h_p-updv04 , 'TableIO':U , h_v-facsvc-sunat ).
       RUN add-link IN adm-broker-hdl ( h_q-cdocu , 'Record':U , h_v-facsvc-sunat ).

       /* Links to SmartQuery h_q-cdocu. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_q-cdocu ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv04 ,
             h_p-navico , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-facsvc-sunat ,
             h_p-updv04 , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_q-cdocu ,
             h_v-facsvc-sunat , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vta/b-facsvc-sunat.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-facsvc-sunat ).
       RUN set-position IN h_b-facsvc-sunat ( 10.42 , 2.00 ) NO-ERROR.
       RUN set-size IN h_b-facsvc-sunat ( 6.69 , 98.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'vta/v-totfac-sunat.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-totfac-sunat ).
       RUN set-position IN h_v-totfac-sunat ( 17.35 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 2.12 , 97.00 ) */

       /* Links to SmartBrowser h_b-facsvc-sunat. */
       RUN add-link IN adm-broker-hdl ( h_q-cdocu , 'Record':U , h_b-facsvc-sunat ).

       /* Links to SmartViewer h_v-totfac-sunat. */
       RUN add-link IN adm-broker-hdl ( h_q-cdocu , 'Record':U , h_v-totfac-sunat ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-facsvc-sunat ,
             h_v-facsvc-sunat , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-totfac-sunat ,
             h_b-facsvc-sunat , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta/t-facsvc.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_t-facsvc ).
       RUN set-position IN h_t-facsvc ( 10.42 , 2.00 ) NO-ERROR.
       RUN set-size IN h_t-facsvc ( 6.69 , 97.00 ) NO-ERROR.

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_t-facsvc ,
             h_v-facsvc-sunat , 'AFTER':U ).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Handle W-Win 
PROCEDURE Procesa-Handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER L-Handle AS CHAR.
  
  CASE L-Handle:
    WHEN "browse" THEN DO:
          RUN dispatch IN h_b-facsvc-sunat ('open-query':U).
          RUN dispatch IN h_t-facsvc ('open-query':U).
      END.
    WHEN "Pagina1"  THEN DO WITH FRAME {&FRAME-NAME}:
         RUN select-page(1).
         RUN dispatch IN h_q-cdocu ('enable':U).
         RUN dispatch IN h_b-facsvc-sunat ('open-query':U).
      END.
    WHEN "Pagina2"  THEN DO WITH FRAME {&FRAME-NAME}:
         RUN select-page(2).
         RUN dispatch IN h_q-cdocu ('disable':U).
         RUN dispatch IN h_t-facsvc ('open-query':U).
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

