&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME sW-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS sW-Win 
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
DEF INPUT PARAMETER pParameter AS CHAR.
/* RHC 06/11/2014 Par�metro para definir si se aplica o no restricciones
definidas por S.L. */
/* YES: bloqueado
/* NO:  desbloqueado
*/ */
DEF NEW SHARED VAR s-FlgSit AS CHAR INIT "YES".     /* Bloqueado x defecto */
IF LOOKUP(pParameter, "YES,NO") > 0 THEN s-FlgSit = pParameter.

/* Local Variable Definitions ---                                       */
DEF NEW SHARED VAR s-coddoc  AS CHAR INITIAL "E/C".
DEF NEW SHARED VAR s-ptovta  AS INT.
DEF NEW SHARED VAR s-tipo    AS CHAR INITIAL "ANTREC".
DEF NEW SHARED VAR f-CodDoc  AS CHAR INITIAL "A/R".
DEF NEW SHARED VAR lh_Handle AS HANDLE.

DEF SHARED VAR s-codcia AS INT.
DEF SHARED VAR s-coddiv LIKE gn-divi.coddiv.
DEF SHARED VAR s-codter LIKE ccbcterm.codter.
DEF SHARED VAR s-user-id AS CHAR.

{ccb\i-ChqUser.i}

FIND CcbDTerm WHERE 
    CcbDTerm.CodCia = s-codcia AND
    CcbDTerm.CodDiv = s-coddiv AND
    CcbDTerm.CodDoc = s-coddoc AND
    CcbDTerm.CodTer = s-codter NO-LOCK NO-ERROR.
IF NOT AVAILABLE ccbdterm THEN DO:
    MESSAGE
        "Egreso de caja no esta configurado en este terminal"
        VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.
  
s-ptovta = ccbdterm.nroser.
  
/* Control de correlativos */
FIND FacCorre WHERE 
    faccorre.codcia = s-codcia AND 
    faccorre.coddoc = s-coddoc AND
    faccorre.nroser = s-ptovta NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
    MESSAGE
        "No esta definida la serie" s-ptovta SKIP
        "para el ingreso a caja" VIEW-AS ALERT-BOX ERROR.
    RETURN "ADM-ERROR".
END.

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
&Scoped-Define ENABLED-OBJECTS CB-Tipo Btn-Print 
&Scoped-Define DISPLAYED-OBJECTS CB-Tipo FILL-IN-desc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR sW-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv10 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-remesa AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-remenc-01 AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Print 
     IMAGE-UP FILE "img\print":U
     LABEL "Button 2" 
     SIZE 5 BY 1.42.

DEFINE VARIABLE CB-Tipo AS CHARACTER FORMAT "X(256)":U INITIAL "ANTREC" 
     LABEL "Tipo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "ANTREC","DEVONC","DEVOBD" 
     DROP-DOWN-LIST
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-desc AS CHARACTER FORMAT "X(256)":U INITIAL "Anticipos" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     FGCOLOR 9 FONT 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     CB-Tipo AT ROW 1.19 COL 3.14
     FILL-IN-desc AT ROW 1.19 COL 17 COLON-ALIGNED NO-LABEL
     Btn-Print AT ROW 11.08 COL 73
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SIZE 78.29 BY 11.73
         FONT 4.


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
  CREATE WINDOW sW-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Devolucion de Efectivo"
         HEIGHT             = 11.77
         WIDTH              = 78.43
         MAX-HEIGHT         = 13.15
         MAX-WIDTH          = 78.43
         VIRTUAL-HEIGHT     = 13.15
         VIRTUAL-WIDTH      = 78.43
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB sW-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW sW-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR COMBO-BOX CB-Tipo IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-desc IN FRAME F-Main
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(sW-Win)
THEN sW-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME sW-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sW-Win sW-Win
ON END-ERROR OF sW-Win /* Devolucion de Efectivo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sW-Win sW-Win
ON WINDOW-CLOSE OF sW-Win /* Devolucion de Efectivo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Print sW-Win
ON CHOOSE OF Btn-Print IN FRAME F-Main /* Button 2 */
DO:
    RUN dispatch IN h_v-remenc-01('imprime':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Tipo sW-Win
ON VALUE-CHANGED OF CB-Tipo IN FRAME F-Main /* Tipo */
DO:

    ASSIGN
        CB-Tipo
        s-tipo = CB-Tipo.

    CASE s-Tipo:
        WHEN "ANTREC" 
            THEN ASSIGN
                    FILL-IN-desc = "Anticipo"
                    f-CodDoc = "A/R".  /* Para Anticipos */
        WHEN 'DEVONC'
            THEN ASSIGN
                FILL-IN-desc = "Nota de Credito"
                f-CodDoc = "N/C".                       /* Para Notas de Creditos */
        WHEN "DEVOBD" 
            THEN ASSIGN
                    FILL-IN-desc = "Boletas de Deposito"
                    f-CodDoc = "BD".
        WHEN 'DEVOAC'
            THEN ASSIGN
                FILL-IN-desc = "Anticipos de Campa�a"
                f-CodDoc = "A/C".                       /* Para Notas de Creditos */
    END CASE.
    
    DISPLAY FILL-IN-desc WITH FRAME {&FRAME-NAME}.

    RUN dispatch IN h_q-remesa ('open-query':U).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK sW-Win 


/* ***************************  Main Block  *************************** */
/* VM - INCLUDE PARA LA CREACION DEL MENU BAR */
{src/adm/template/cntnrwin.i}

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects sW-Win  _ADM-CREATE-OBJECTS
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
       RUN set-position IN h_p-navico ( 11.00 , 19.00 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.54 , 16.86 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm-vm/objects/p-updv10.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv10 ).
       RUN set-position IN h_p-updv10 ( 11.00 , 36.00 ) NO-ERROR.
       RUN set-size IN h_p-updv10 ( 1.54 , 36.86 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/ccb/q-remesa.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-remesa ).
       RUN set-position IN h_q-remesa ( 11.19 , 6.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.00 , 10.86 ) */

       /* Links to SmartQuery h_q-remesa. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_q-remesa ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-navico ,
             FILL-IN-desc:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updv10 ,
             h_p-navico , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_q-remesa ,
             Btn-Print:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'ccb/v-remenc-01.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-remenc-01 ).
       RUN set-position IN h_v-remenc-01 ( 2.15 , 2.00 ) NO-ERROR.
       /* Size in UIB:  ( 8.65 , 76.57 ) */

       /* Links to SmartViewer h_v-remenc-01. */
       RUN add-link IN adm-broker-hdl ( h_p-updv10 , 'TableIO':U , h_v-remenc-01 ).
       RUN add-link IN adm-broker-hdl ( h_q-remesa , 'Record':U , h_v-remenc-01 ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-remenc-01 ,
             FILL-IN-desc:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 1 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available sW-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI sW-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(sW-Win)
  THEN DELETE WIDGET sW-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI sW-Win  _DEFAULT-ENABLE
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
  DISPLAY CB-Tipo FILL-IN-desc 
      WITH FRAME F-Main IN WINDOW sW-Win.
  ENABLE CB-Tipo Btn-Print 
      WITH FRAME F-Main IN WINDOW sW-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW sW-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit sW-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize sW-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
/*   IF s-FlgSit = 'NO' THEN DO WITH FRAME {&FRAME-NAME}:  */
/*        CB-Tipo:ADD-LAST('DEVOAC').                      */
/*   END.                                                  */
  ASSIGN lh_Handle = THIS-PROCEDURE.
  RUN Procesa-Handle ('Pagina1').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesa-Handle sW-Win 
PROCEDURE Procesa-Handle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER L-Handle AS CHAR.
CASE L-Handle:
    WHEN "Pagina1"  THEN DO WITH FRAME {&FRAME-NAME}:    /* Origen Ninguna */
         RUN select-page(1).
      END.
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc_enable sW-Win 
PROCEDURE proc_enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER para_enable AS LOGICAL.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            MENU MENU-BAR-W-Win:SENSITIVE = para_enable
            Btn-Print:SENSITIVE = para_enable
            CB-Tipo:SENSITIVE = para_enable
            h_q-remesa:SENSITIVE = para_enable.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records sW-Win  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed sW-Win 
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

