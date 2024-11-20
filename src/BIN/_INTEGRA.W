&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"Ingreso al sistema Integral"
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

DEFINE SHARED VAR pRCID AS INT.
DEFINE NEW GLOBAL SHARED VARIABLE s-user-id AS CHARACTER FORMAT "x(16)".
DEFINE NEW GLOBAL SHARED VARIABLE hSocket AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hWebService AS HANDLE NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE hPortType AS HANDLE NO-UNDO.

DEFINE VARIABLE botones     AS WIDGET-HANDLE EXTENT 50 NO-UNDO.
DEFINE VARIABLE s-fil       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE s-col       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fila        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE columna     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE fila-ini    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE columna-ini AS DECIMAL   NO-UNDO.
DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
DEFINE VARIABLE max-app     AS INTEGER   NO-UNDO.
DEFINE VARIABLE x-codcia    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lista-cia   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lista-cod   AS CHARACTER NO-UNDO.
DEFINE VARIABLE Ok          AS LOGICAL   NO-UNDO.

/* CONFIGURACIONES DE ENTORNO POR DEFECTO */
SESSION:DATA-ENTRY-RETURN = TRUE.    /* << OJO actua como si fuera TAB */
SESSION:APPL-ALERT-BOXES  = NO.     /* << OJO si es TRUE los mensajes son alert-box */
SESSION:SYSTEM-ALERT-BOXES  = NO.     /* << OJO si es TRUE los mensajes son alert-box */
SESSION:SUPPRESS-WARNINGS  = YES.     /* << OJO si es TRUE los mensajes son alert-box */
SESSION:TIME-SOURCE = "integral".   /* Toma la fecha y hora del servidor de 'integral' */
ASSIGN
    Ok        = SESSION:SET-WAIT-STATE("GENERAL")
    s-user-id = USERID("integral").

RUN bin/_registr.w("YES").

IF ( s-user-id <> "MASTER" AND s-user-id <> "ADMIN" ) AND
    NOT CAN-FIND(FIRST integral.PF-G004 WHERE integral.PF-G004.User-Id = s-user-id) THEN DO:
    BELL.
    MESSAGE "El usuario no tiene aplicaciones inscritas"
        VIEW-AS ALERT-BOX ERROR.
    QUIT.
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
&Scoped-Define ENABLED-OBJECTS IMAGE-1 COMBO-BOX-CIAS 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-CIAS txtPCName FILL-IN-Fecha 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE COMBO-BOX-CIAS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Compañía" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 50.43 BY 1
     BGCOLOR 15 FGCOLOR 1 FONT 0 NO-UNDO.

DEFINE VARIABLE FILL-IN-Fecha AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE txtPCName AS CHARACTER FORMAT "X(256)":U 
     LABEL "PC Name" 
     VIEW-AS FILL-IN 
     SIZE 23 BY .81 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "img/continental.jpg":U
     SIZE 49 BY 3.42.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     COMBO-BOX-CIAS AT ROW 10.23 COL 10 COLON-ALIGNED
     txtPCName AT ROW 14.65 COL 8.29 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-Fecha AT ROW 14.65 COL 35 NO-LABEL
     IMAGE-1 AT ROW 11.19 COL 12.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.86 BY 14.69
         BGCOLOR 15 FONT 4.

DEFINE FRAME FRAME-A
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 1.38
         SIZE 60 BY 8.65
         BGCOLOR 8 FGCOLOR 0 FONT 4.


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
         TITLE              = ""
         HEIGHT             = 14.69
         WIDTH              = 63.86
         MAX-HEIGHT         = 14.69
         MAX-WIDTH          = 63.86
         VIRTUAL-HEIGHT     = 14.69
         VIRTUAL-WIDTH      = 63.86
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = 0
         THREE-D            = yes
         FONT               = 4
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-Fecha IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN txtPCName IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
    IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win
DO:
    /* This ADM code must be left here in order for the SmartWindow
       and its descendents to terminate properly on exit. */    

    /* Grabar la SALIDA */
    FIND FIRST AdmCtrlUser WHERE AdmCtrlUser.Numid = pRCID EXCLUSIVE NO-ERROR.
    IF AVAILABLE AdmCtrlUser THEN DO:
        ASSIGN AdmCtrlUser.FechTermino  = TODAY
            AdmCtrlUser.HoraTermino     = STRING(TIME,"HH:MM:SS").
    END.

    QUIT.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

IF VALID-HANDLE({&WINDOW-NAME}) THEN DO:
    ASSIGN
        CURRENT-WINDOW                    = {&WINDOW-NAME} 
        {&WINDOW-NAME}:KEEP-FRAME-Z-ORDER = YES
        THIS-PROCEDURE:CURRENT-WINDOW     = {&WINDOW-NAME}.

    /* The CLOSE event can be used from inside or outside the procedure to  */
    /* terminate it.                                                        */
    ON CLOSE OF THIS-PROCEDURE RUN dispatch IN THIS-PROCEDURE ('destroy':U).

    RUN dispatch ('create-objects':U).

    /* Execute this code only if not being run PERSISTENT, i.e., if in test mode
    of one kind or another or if this is a Main Window. Otherwise postpone 
    'initialize' until told to do so. */

    &IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
    IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
    &ENDIF
 
    /* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
    MAIN-BLOCK:
    DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
 
        /* Now enable the interface and wait for the exit condition.            */
        ASSIGN
            s-fil       = 0.4
            s-col       = 2.43      
            fila-ini    = 1.38
            columna-ini = 3.43
            lista-cod   = ""
            max-app     = 0
            fila        = fila-ini
            columna     = columna-ini.
        IF s-User-id = "MASTER" OR s-User-id = "ADMIN" THEN
            FOR EACH integral.PF-G003 NO-LOCK:
                max-app = max-app + 1.
                CREATE BUTTON botones[ max-app ] 
                ASSIGN
                    LABEL        = integral.PF-G003.Aplic-id 
                    HELP         = integral.PF-G003.Detalle 
                    PRIVATE-DATA = integral.PF-G003.Procedimiento 
                    ROW          = fila 
                    COLUMN       = columna 
                    WIDTH-CHARS  = 11.50 
                    HEIGHT-CHARS = 2.35 
                    FRAME        = FRAME FRAME-A:HANDLE 
                    SENSITIVE    = YES 
                    VISIBLE      = YES
                    TOOLTIP      = integral.PF-G003.Detalle
                    NO-FOCUS     = YES
                    FLAT-BUTTON  = YES
                TRIGGERS:
                    ON CHOOSE DO:
                        W-WIN:WINDOW-STATE = 2.
                        SELF:SENSITIVE = NO.
                        Ok = SESSION:SET-WAIT-STATE("GENERAL"). 
                        x-codcia = INTEGER( ENTRY( LOOKUP(COMBO-BOX-CIAS:SCREEN-VALUE IN FRAME F-MAIN, lista-cia)
                            , lista-cod ) ).
                        RUN bin/_medio.p( x-codcia, SELF:LABEL ).
                        Ok = SESSION:SET-WAIT-STATE("").
                        W-WIN:WINDOW-STATE = 3.
                        SELF:SENSITIVE = YES.
                    END.
                END.
                Ok = botones[ max-app ]:LOAD-IMAGE( "img/app-" + integral.PF-G003.Aplic-id  ).
                Ok = botones[ max-app ]:LOAD-IMAGE-INSENSITIVE( "img/apx-" + integral.PF-G003.Aplic-id  ).
                IF ( max-app MODULO 4 ) = 0 THEN
                    ASSIGN
                        fila    = fila + s-fil + 2.35
                        columna = columna-ini.
                ELSE ASSIGN columna = columna + s-col + 11.50.
            END.
        ELSE
            FOR EACH integral.PF-G003,
                EACH integral.PF-G004 OF integral.PF-G003 WHERE
                    integral.PF-G004.User-Id = s-User-id NO-LOCK:
                /* Verificamos que la aplicación no se repita */
                Ok = FALSE.
                DO i = 1 TO max-app:
                    IF botones[ max-app ]:LABEL = integral.PF-G003.Aplic-id THEN DO:
                        Ok = TRUE.
                        LEAVE.
                    END.
                END.
                IF NOT Ok THEN DO:
                    max-app = max-app + 1.
                    CREATE BUTTON botones[ max-app ] 
                    ASSIGN
                        LABEL        = integral.PF-G003.Aplic-id 
                        HELP         = integral.PF-G003.Detalle 
                        PRIVATE-DATA = integral.PF-G003.Procedimiento 
                        ROW          = fila 
                        COLUMN       = columna 
                        WIDTH-CHARS  = 11.50 
                        HEIGHT-CHARS = 2.35 
                        FRAME        = FRAME FRAME-A:HANDLE 
                        SENSITIVE    = YES 
                        VISIBLE      = YES
                        TOOLTIP      = integral.PF-G003.Detalle
                        NO-FOCUS     = YES
                        FLAT-BUTTON  = YES
                    TRIGGERS:
                        ON CHOOSE DO:
                            W-WIN:WINDOW-STATE = 2.
                            SELF:SENSITIVE = NO.
                            Ok = SESSION:SET-WAIT-STATE("GENERAL"). 
                            x-codcia = INTEGER( ENTRY( LOOKUP(COMBO-BOX-CIAS:SCREEN-VALUE IN FRAME F-MAIN, lista-cia)
                                , lista-cod ) ).
                            RUN bin/_medio.p( x-codcia, SELF:LABEL ).
                            Ok = SESSION:SET-WAIT-STATE("").
                            W-Win:WINDOW-STATE = 3.
                            SELF:SENSITIVE = YES.
                        END.
                    END. 
                    Ok = botones[ max-app ]:LOAD-IMAGE( "img/app-" + integral.PF-G003.Aplic-Id ).
                    Ok = botones[ max-app ]:LOAD-IMAGE-INSENSITIVE( "img/apx-" + integral.PF-G003.Aplic-Id ).
                    IF ( max-app MODULO 4 ) = 0 THEN
                        ASSIGN
                            fila    = fila + s-fil + 2.35
                            columna = columna-ini.
                    ELSE ASSIGN columna = columna + s-col + 11.50.
                END.
            END.
        OK = FRAME FRAME-A:LOAD-MOUSE-POINTER( "GLOVE").
        RUN dispatch ('initialize':U).
        IF NOT THIS-PROCEDURE:PERSISTENT THEN WAIT-FOR CLOSE OF THIS-PROCEDURE.
    END.
    &IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
    END.
    &ENDIF

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crear_cias W-Win 
PROCEDURE crear_cias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE GN-CIAS.
    ASSIGN
        GN-CIAS.codcia = 1.
        GN-CIAS.nomcia = "Compañia por inscribir".
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
  DISPLAY COMBO-BOX-CIAS txtPCName FILL-IN-Fecha 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE IMAGE-1 COMBO-BOX-CIAS 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW FRAME FRAME-A IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
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
    DEFINE VARIABLE cListaMes AS CHARACTER EXTENT 12
        INITIAL ["Enero", "Febrero", "Marzo", "Abril",
        "Mayo", "Junio", "Julio", "Agosto", "Setiembre",
        "Octubre", "Noviembre", "Diciembre"].
    DEFINE VARIABLE cListaDia AS CHARACTER EXTENT 7
        INITIAL ["Domingo", "Lunes", "Martes", "Miércoles",
        "Jueves", "Viernes", "Sábado"].

    FIND FIRST GN-CIAS NO-LOCK NO-ERROR.
    IF NOT AVAILABLE GN-CIAS THEN DO:
        RUN crear_cias.
        FIND FIRST GN-CIAS NO-LOCK NO-ERROR.
    END.
    IF AVAILABLE GN-CIAS THEN DO:
        lista-cia = GN-CIAS.nomcia.
        lista-cod = STRING(GN-CIAS.codcia, "999").
        i = i + 1.
        REPEAT:
            FIND NEXT GN-CIAS NO-LOCK.
            lista-cia = lista-cia + "," + GN-CIAS.nomcia.
            lista-cod = lista-cod + "," + STRING(GN-CIAS.codcia, "999").
            i = i + 1.
        END.
        ASSIGN
            COMBO-BOX-CIAS:LIST-ITEMS IN FRAME F-Main   = lista-cia
            COMBO-BOX-CIAS:SCREEN-VALUE IN FRAME F-Main = COMBO-BOX-CIAS:ENTRY( 1 ) IN FRAME F-Main.
    END.
    Ok = SESSION:SET-WAIT-STATE("").

    /* Fecha */
    FILL-IN-Fecha =
        cListaDia[WEEKDAY(TODAY)] + ", " +
        STRING(DAY(TODAY)) + " de " +
        cListaMes[MONTH(TODAY)] + " de " +
        STRING(YEAR(TODAY)).

/*
DEFINE VAR lClientIPAddress     AS CHAR.
DEFINE VAR lClientComputerName  AS CHAR.
DEFINE VAR lClientName          AS CHAR.
DEFINE VAR lComputerName        AS CHAR.
DEFINE VAR lRemote-user         AS CHAR.
DEFINE VAR lUserName            AS CHAR.
DEFINE VAR lxClientName         AS CHAR.
DEFINE VAR lXRowId              AS CHAR.
DEFINE VAR lRowId               AS INT.

lClientIPAddress    = OS-GETENV ( "CLIENTCOMPUTERNAMEIPADDRESS").
lClientComputerName = OS-GETENV ( "CLIENTCOMPUTERNAME").
lClientName         = OS-GETENV ( "CLIENTNAME").
lComputerName       = OS-GETENV ( "COMPUTERNAME").
lRemote-user        = OS-GETENV ( "REMOTE_USER").
lUserName           = OS-GETENV ( "USERNAME").

lxClientName        = IF (lClientComputerName = ? OR lClientComputerName = "") THEN lClientName ELSE lClientComputerName.
lxClientName        = IF (lxClientName = ? OR lxClientName = "") THEN lComputerName ELSE lxClientName.

/* Grabo los datos */

CREATE AdmCtrlUser.

    lxRowId = STRING(ROWID(AdmCtrlUser),"9999999999").
    lRowId  = INTEGER(lxRowId).

    ASSIGN  Numid       = lRowid
            IPRemoto    = lClientIPAddress
            PCRemoto    = IF (lClientComputerName = ? OR lClientComputerName = "") THEN lClientName ELSE lClientComputerName
            PCCliente   = lComputerName
*/

    DEFINE VAR lClientComputerName  AS CHAR.
    DEFINE VAR lClientName          AS CHAR.
    DEFINE VAR lComputerName        AS CHAR.


    DEFINE VAR lPCName AS CHAR.
    /*
    lConnID = STRING(_MyConnection._MyConn-id,">>;>>>,>>9").
    lConnPID = STRING(_MyConnection._MyConn-pid,">>;>>>,>>9").
    */
    lClientComputerName = OS-GETENV ( "CLIENTCOMPUTERNAME").
    lClientName         = OS-GETENV ( "CLIENTNAME").
    lComputerName       = OS-GETENV ( "COMPUTERNAME").

    lPcName = IF (lClientComputerName = ? OR lClientComputerName = "") THEN lClientName ELSE lClientComputerName.
    lPCName = IF (CAPS(lPCName) = "CONSOLE") THEN "" ELSE lPCName.
    lPCName = IF (lPCName = ? OR lPCName = "") THEN lComputerName ELSE lPCName.

    DEFINE VAR lConnPID AS CHAR.

    FIND FIRST _Myconnection.
    IF AVAILABLE _Myconnection THEN DO:    
        /*lConnPID = STRING(_MyConnection._MyConn-pid,">>>>9").*/
        lConnPID = STRING(_MyConnection._MyConn-UserId,">>>>9").
    END.
                 
    ASSIGN txtPCName = lPCName + " / " + lConnPID.       
            /*txtConnID = lConnID
            txtConnPID = lConnPID*/.
    DISPLAY txtPCName.

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE( INPUT 'initialize':U ).

    /* Code placed here will execute AFTER standard behavior.    */

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

