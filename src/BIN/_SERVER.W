&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
/* Procedure Description
"Conexión a los Servidore de Base de Datos"
*/
&ANALYZE-RESUME
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
DEFINE VARIABLE OK AS LOGICAL NO-UNDO.
OK = SESSION:SET-WAIT-STATE("").

DEFINE VARIABLE H-IMAGE  AS WIDGET EXTENT 20.
DEFINE VARIABLE H-SERVER AS WIDGET EXTENT 20.

DEFINE TEMP-TABLE T-SERVER
        FIELD ITEM    AS INTEGER
        FIELD SERVER  AS CHARACTER FORMAT "X(40)"
        FIELD PROFILE AS CHARACTER.

DEFINE VARIABLE H-ESPERA AS WIDGET-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-ayuda BUTTON-salir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-ayuda 
     LABEL "A&yuda" 
     SIZE 9 BY .85.

DEFINE BUTTON BUTTON-salir 
     LABEL "&Salir" 
     SIZE 9 BY .85.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-ayuda AT ROW 1.15 COL 43.86
     BUTTON-salir AT ROW 2.31 COL 43.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 17
         FONT 4.

DEFINE FRAME FRAME-SVR
    WITH DOWN KEEP-TAB-ORDER OVERLAY 
         NO-LABELS SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.04
         SCROLLABLE SIZE 41.57 BY 100
         BGCOLOR 15 FGCOLOR 0 FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Conexión al Servidor de Progress"
         HEIGHT             = 11.38
         WIDTH              = 52.29
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 80
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-SVR:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-SVR:MOVE-BEFORE-TAB-ITEM (BUTTON-ayuda:HANDLE IN FRAME F-Main)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-SVR
                                                                        */
ASSIGN 
       FRAME FRAME-SVR:HEIGHT           = 11.31
       FRAME FRAME-SVR:WIDTH            = 41.57.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-SVR
/* Query rebuild information for FRAME FRAME-SVR
     _Query            is NOT OPENED
*/  /* FRAME FRAME-SVR */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{scr/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Conexión al Servidor de Progress */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Conexión al Servidor de Progress */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-salir W-Win
ON CHOOSE OF BUTTON-salir IN FRAME F-Main /* Salir */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{scr/adm/template/windowmn.i}

/* Buscando los servidores existentes */
  DEFINE VARIABLE NroSrv   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE key-name AS CHARACTER NO-UNDO.
  DEFINE VARIABLE srv-name AS CHARACTER FORMAT "X(40)" NO-UNDO.
  DEFINE VARIABLE pf-name  AS CHARACTER NO-UNDO.

  NroSrv = 0.
  REPEAT:
    key-name = "SRV" + STRING( NroSrv + 1, "99").
    GET-KEY-VALUE SECTION "Servidores" KEY key-name VALUE pf-name.
    IF PF-NAME = ? THEN LEAVE.
    /* Jalando la definición del archivo */
    INPUT FROM VALUE(pf-name).
    IMPORT UNFORMATTED Srv-name.
    srv-name = SUBSTR(srv-name, 2).
    INPUT CLOSE.
    NroSrv = NroSrv + 1.
    CREATE IMAGE H-IMAGE[ NroSrv ]
       ASSIGN ROW          = 1 + (NroSrv - 1) * 1.85
              COLUMN       = 1
              HEIGHT       = 1.85
              WIDTH        = 8.4 
              FRAME        = FRAME FRAME-SVR:HANDLE
              SENSITIVE    = TRUE
              VISIBLE      = TRUE
              PRIVATE-DATA = STRING(NroSrv)
              TRIGGERS:
               ON MOUSE-SELECT-CLICK
                   DO:
                      NroSrv = INTEGER( SELF:PRIVATE-DATA ).
                      APPLY "ENTRY":U TO H-SERVER[ NroSrv ]. 
                   END.
               END TRIGGERS.

    IF NroSrv = 1          
    THEN OK = H-IMAGE[ NroSrv ]:LOAD-IMAGE("img\srv00").
    ELSE OK = H-IMAGE[ NroSrv ]:LOAD-IMAGE("img\srv01").
    
    CREATE BUTTON H-SERVER[ NroSrv ]
       ASSIGN ROW = 1 + (NroSrv - 1) * 1.85 + 1
              COLUMN       = 10
              HEIGHT       = .81
              WIDTH        = 25
              LABEL = SRV-NAME
              FRAME        = FRAME FRAME-SVR:HANDLE
              PRIVATE-DATA = STRING(NroSrv)
              SENSITIVE    = TRUE
              VISIBLE      = TRUE
              TRIGGERS:
               ON ENTRY
                   DO:
                      NroSrv = INTEGER( SELF:PRIVATE-DATA ).
                   END.
                ON CHOOSE
                    DO:
                        RUN CONECTAR ( SELF:PRIVATE-DATA ).
                        IF CONNECTED("integral")
                        THEN APPLY "CLOSE" TO THIS-PROCEDURE.
                    END.
               END TRIGGERS.

    CREATE T-SERVER.
    ASSIGN T-SERVER.ITEM    = NroSrv
           T-SERVER.SERVER  = Srv-name
           T-SERVER.PROFILE = Pf-name.

  END.
  IF NroSrv = 1          
  THEN OK = H-IMAGE[ NroSrv ]:LOAD-IMAGE("img\srv00c").
  ELSE OK = H-IMAGE[ NroSrv ]:LOAD-IMAGE("img\srv01c").

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
  {scr/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {scr/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CONECTAR W-Win 
PROCEDURE CONECTAR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER NroSrv AS CHARACTER.
    DEFINE VARIABLE MENSAJE AS CHARACTER.
    
    key-name = "SRV" + STRING( INTEGER( NroSrv ), "99").
    GET-KEY-VALUE SECTION "Servidores" KEY key-name VALUE pf-name.
    IF PF-NAME = ? THEN LEAVE.

    MENSAJE = "Conectando al servidor de Base Datos" + CHR(10) + SELF:LABEL.
    RUN BIN\_ESPERA.W  PERSISTENT SET H-ESPERA (MENSAJE).
    RUN dispatch IN H-ESPERA ('initialize':U).
    DO ON ERROR UNDO, LEAVE ON STOP UNDO, LEAVE:
        /* DESCONECTANDO LA ANTERIOR BASE DE DATOS */
        IF CONNECTED("integral")
        THEN DISCONNECT "INTEGRAL".
        
        CONNECT -pf VALUE(PF-NAME) -ld integral.
    END.
    DELETE PROCEDURE H-ESPERA.
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
  ENABLE BUTTON-ayuda BUTTON-salir 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW FRAME FRAME-SVR IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-SVR}
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


