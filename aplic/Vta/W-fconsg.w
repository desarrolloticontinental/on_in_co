&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
DEFINE NEW SHARED TEMP-TABLE DETA LIKE CcbDDocu.
DEFINE NEW SHARED VARIABLE S-CODDOC   AS CHAR INITIAL "FAC".
DEFINE NEW SHARED VARIABLE lh_Handle  AS HANDLE.
DEFINE NEW SHARED VARIABLE S-NROSER   AS INTEGER.
DEFINE NEW SHARED VARIABLE S-CODCLI   AS CHAR.

DEFINE NEW SHARED VARIABLE S-PORDTO  AS DECIMAL.

DEFINE     SHARED VARIABLE S-CODCIA   AS INTEGER.
DEFINE     SHARED VARIABLE S-CODDIV   AS CHAR.
DEFINE     SHARED VARIABLE S-CODVEN   AS CHAR.
DEFINE     SHARED VARIABLE S-USER-ID  AS CHAR.

DEFINE VARIABLE L-NROSER   AS CHAR NO-UNDO.

FIND FIRST FacCorre WHERE 
           FacCorre.CodCia = S-CODCIA AND 
           FacCorre.CodDoc = S-CODDOC AND 
           FacCorre.CodDiv = S-CODDIV 
           NO-LOCK NO-ERROR.
IF NOT AVAILABLE FacCorre THEN DO:
   S-CODDOC = "BOL".
   FIND FIRST FacCorre WHERE 
              FacCorre.CodCia = S-CODCIA AND
              FacCorre.CodDoc = S-CODDOC AND
              FacCorre.CodDiv = S-CODDIV 
              NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FacCorre THEN DO:
      MESSAGE "Codigo de Documento no configurado" VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-20 C-CodDoc C-NroSer Btn-Print ~
Btn-Liquida 
&Scoped-Define DISPLAYED-OBJECTS C-CodDoc C-NroSer 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-docmto AS HANDLE NO-UNDO.
DEFINE VARIABLE h_b-tmpcsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-navico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_p-updv10 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-fconsg AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-fconsg AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Liquida 
     LABEL "Liquidacion" 
     SIZE 9 BY .85.

DEFINE BUTTON Btn-Print 
     LABEL "Imprimir" 
     SIZE 6.86 BY .77.

DEFINE VARIABLE C-CodDoc AS CHARACTER FORMAT "X(256)":U INITIAL "FAC" 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEMS "FAC","BOL" 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE C-NroSer AS CHARACTER FORMAT "X(3)":U INITIAL "0" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","" 
     SIZE 6.72 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 8.14 BY 1.31.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     C-CodDoc AT ROW 6.58 COL 1 NO-LABEL
     C-NroSer AT ROW 6.58 COL 8 NO-LABEL
     Btn-Print AT ROW 6.77 COL 80
     Btn-Liquida AT ROW 8.12 COL 77.86
     RECT-20 AT ROW 6.38 COL 79
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.72 BY 16.27
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
         TITLE              = "REGISTRO FACTURAS ( CONSIGNACION )"
         HEIGHT             = 16.27
         WIDTH              = 86.72
         MAX-HEIGHT         = 17.46
         MAX-WIDTH          = 86.72
         VIRTUAL-HEIGHT     = 17.46
         VIRTUAL-WIDTH      = 86.72
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR COMBO-BOX C-CodDoc IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX C-NroSer IN FRAME F-Main
   ALIGN-L                                                              */
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
ON END-ERROR OF W-Win /* REGISTRO FACTURAS ( CONSIGNACION ) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* REGISTRO FACTURAS ( CONSIGNACION ) */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Liquida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Liquida W-Win
ON CHOOSE OF Btn-Liquida IN FRAME F-Main /* Liquidacion */
DO:
  RUN Asigna-Liquidacion IN h_v-fconsg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Print W-Win
ON CHOOSE OF Btn-Print IN FRAME F-Main /* Imprimir */
DO:
  RUN dispatch IN h_v-fconsg ('imprime':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME C-CodDoc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-CodDoc W-Win
ON VALUE-CHANGED OF C-CodDoc IN FRAME F-Main
DO:
   FIND FIRST FacCorre WHERE FacCorre.CodCia = S-CODCIA AND
        FacCorre.CodDoc = C-CODDOC:SCREEN-VALUE AND
        FacCorre.CodDiv = S-CODDIV NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FacCorre THEN DO:
      MESSAGE "Codigo de Documento no configurado" VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   ASSIGN C-CODDOC 
          S-CODDOC = C-CODDOC.
   RUN Carga-Series.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME C-NroSer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-NroSer W-Win
ON VALUE-CHANGED OF C-NroSer IN FRAME F-Main
DO:
  ASSIGN C-NroSer.
  S-NROSER = INTEGER(ENTRY(LOOKUP(C-NroSer,C-NroSer:LIST-ITEMS),C-NroSer:LIST-ITEMS)).
  RUN dispatch IN h_q-fconsg ('open-query':U).
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
             INPUT  'aplic/vta/v-fconsg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-fconsg ).
       RUN set-position IN h_v-fconsg ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.27 , 86.14 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm/objects/p-navico.r':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = NAV-ICON,
                     Right-to-Left = First-On-Left':U ,
             OUTPUT h_p-navico ).
       RUN set-position IN h_p-navico ( 6.38 , 24.43 ) NO-ERROR.
       RUN set-size IN h_p-navico ( 1.31 , 17.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'adm-vm/objects/p-updv10.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Update,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updv10 ).
       RUN set-position IN h_p-updv10 ( 6.38 , 41.29 ) NO-ERROR.
       RUN set-size IN h_p-updv10 ( 1.31 , 37.57 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta/q-fconsg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-fconsg ).
       RUN set-position IN h_q-fconsg ( 6.46 , 15.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.04 , 9.29 ) */

       /* Links to SmartViewer h_v-fconsg. */
       RUN add-link IN adm-broker-hdl ( h_p-updv10 , 'TableIO':U , h_v-fconsg ).
       RUN add-link IN adm-broker-hdl ( h_q-fconsg , 'Record':U , h_v-fconsg ).

       /* Links to SmartQuery h_q-fconsg. */
       RUN add-link IN adm-broker-hdl ( h_p-navico , 'Navigation':U , h_q-fconsg ).

    END. /* Page 0 */

    WHEN 1 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta/b-docmto.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-docmto ).
       RUN set-position IN h_b-docmto ( 7.81 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 7.65 , 85.86 ) */

       /* Links to SmartBrowser h_b-docmto. */
       RUN add-link IN adm-broker-hdl ( h_q-fconsg , 'Record':U , h_b-docmto ).

    END. /* Page 1 */

    WHEN 2 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'aplic/vta/b-tmpcsg.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-tmpcsg ).
       RUN set-position IN h_b-tmpcsg ( 7.77 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 9.27 , 76.29 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 2 */

  END CASE.
  /* Select a Startup page. */
  IF adm-current-page eq 0 
  THEN RUN select-page IN THIS-PROCEDURE ( 2 ).

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Carga-Series W-Win 
PROCEDURE Carga-Series :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  L-NROSER = "".
  FOR EACH FacCorre NO-LOCK WHERE 
           FacCorre.CodCia = S-CODCIA AND
           FacCorre.CodDoc = S-CODDOC AND
           FacCorre.CodDiv = S-CODDIV:
      IF L-NROSER = "" THEN L-NROSER = STRING(FacCorre.NroSer,"999").
      ELSE L-NROSER = L-NROSER + "," + STRING(FacCorre.NroSer,"999").
  END.
  DO WITH FRAME {&FRAME-NAME}:
     C-NroSer:LIST-ITEMS = L-NROSER.
     S-NROSER = INTEGER(ENTRY(1,C-NroSer:LIST-ITEMS)).
     C-NROSER = ENTRY(1,C-NroSer:LIST-ITEMS).
     DISPLAY C-NROSER.
  END.
  
  RUN dispatch IN h_q-fconsg ('open-query':U).

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
  DISPLAY C-CodDoc C-NroSer 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-20 C-CodDoc C-NroSer Btn-Print Btn-Liquida 
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
  ASSIGN lh_Handle = THIS-PROCEDURE.
  RUN Procesa-Handle ("Pagina1").
  C-CodDoc = S-CODDOC.
  DISPLAY C-CodDoc WITH FRAME {&FRAME-NAME}.
  RUN Carga-Series.
    
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
          RUN dispatch IN h_b-docmto ('open-query':U).
          RUN dispatch IN h_b-tmpcsg ('open-query':U).
      END.
    WHEN "Pagina1"  THEN DO WITH FRAME {&FRAME-NAME}:
         RUN select-page(1).
         Btn-Liquida:VISIBLE   = NO.
         Btn-Print:SENSITIVE = YES.
         C-NroSer:SENSITIVE  = YES.
         C-CodDoc:SENSITIVE  = YES.
         RUN dispatch IN h_q-fconsg ('enable':U).
      END.
    WHEN "Pagina2"  THEN DO WITH FRAME {&FRAME-NAME}:
         RUN select-page(2).
         Btn-Liquida:VISIBLE   = YES.
         Btn-Print:SENSITIVE = NO.
         C-NroSer:SENSITIVE  = NO.
         C-CodDoc:SENSITIVE  = NO.
         RUN dispatch IN h_q-fconsg ('disable':U).
      END.
END CASE.

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

