&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"Registro de Usuario por Aplicaci¢n"
*/
&ANALYZE-RESUME
/* Connected Databases 
          integral         PROGRESS
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
DEFINE SHARED VARIABLE s-aplic-id AS CHARACTER.

DEFINE VARIABLE cias-usuario  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cias-selecc   AS CHARACTER NO-UNDO.
DEFINE VARIABLE grupos-aplic  AS CHARACTER NO-UNDO.
DEFINE VARIABLE x-nomcia      AS CHARACTER NO-UNDO.
DEFINE VARIABLE ROWID-act     AS ROWID NO-UNDO.


DEFINE BUFFER b-G004 FOR PF-G004.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-control
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES PF-G004

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 PF-G004.CodCia x-nomcia @ x-nomcia ~
PF-G004.Admin 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 PF-G004.Admin 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 PF-G004
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 PF-G004
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH PF-G004 ~
      WHERE PF-G004.Aplic-Id = s-aplic-id ~
 AND PF-G004.User-Id = INPUT FRAME F-Main FILL-IN-user-id NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH PF-G004 ~
      WHERE PF-G004.Aplic-Id = s-aplic-id ~
 AND PF-G004.User-Id = INPUT FRAME F-Main FILL-IN-user-id NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 PF-G004
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 PF-G004


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn-First Btn-Prev Btn-Next Btn-Last ~
BUTTON-CONSULTA Btn-add Btn-del Btn-save Btn-exit RECT-2 RECT-3 RECT-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 RECT-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-add 
     LABEL "&Adicionar" 
     SIZE 7.72 BY 1.

DEFINE BUTTON Btn-cancel 
     LABEL "&Cancelar" 
     SIZE 7.72 BY 1.

DEFINE BUTTON Btn-del 
     LABEL "&Eliminar" 
     SIZE 7.72 BY 1.

DEFINE BUTTON Btn-exit 
     LABEL "&Salir" 
     SIZE 7.72 BY 1.

DEFINE BUTTON Btn-First 
     IMAGE-UP FILE "adeicon/first-au":U
     IMAGE-INSENSITIVE FILE "adeicon/first-ai":U
     LABEL "&First":L 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-Last 
     IMAGE-UP FILE "adeicon/last-au":U
     IMAGE-INSENSITIVE FILE "adeicon/last-ai":U
     LABEL "&Last":L 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-Next 
     IMAGE-UP FILE "adeicon/next-au":U
     IMAGE-INSENSITIVE FILE "adeicon/next-ai":U
     LABEL "&Next":L 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-Prev 
     IMAGE-UP FILE "adeicon/prev-au":U
     IMAGE-INSENSITIVE FILE "adeicon/prev-ai":U
     LABEL "&Prev":L 
     SIZE 4 BY 1.

DEFINE BUTTON Btn-save 
     LABEL "&Grabar" 
     SIZE 7.72 BY 1.

DEFINE BUTTON BUTTON-CONSULTA 
     IMAGE-UP FILE "img/pvbuscar":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 18 BY 1.46
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 42.57 BY 1.46
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 5.72 BY 1.46
     BGCOLOR 8 FGCOLOR 0 .

DEFINE BUTTON Btn-add-cias 
     LABEL "&Añadir" 
     SIZE 11.57 BY .85.

DEFINE BUTTON Btn-anade 
     LABEL "<< &Añadir" 
     SIZE 11 BY .85.

DEFINE BUTTON Btn-del-cias 
     LABEL "&Quitar" 
     SIZE 11.57 BY .85.

DEFINE BUTTON Btn-remove 
     LABEL "&Remover >>" 
     SIZE 11 BY .85.

DEFINE VARIABLE FILL-IN-aplicacion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aplicación" 
     VIEW-AS FILL-IN 
     SIZE 53.29 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-Comentario AS CHARACTER FORMAT "X(256)":U 
     LABEL "Comentario" 
     VIEW-AS FILL-IN 
     SIZE 53.29 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-User-Id AS CHARACTER FORMAT "X(11)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 16.43 BY .88 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64.57 BY 2.31.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64.57 BY 3.54.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64.57 BY 5.04.

DEFINE VARIABLE SELECT-grp-aplic AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24.14 BY 2.69 NO-UNDO.

DEFINE VARIABLE SELECT-grp-user AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24.14 BY 2.69 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      PF-G004 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Win _STRUCTURED
  QUERY BROWSE-1 DISPLAY
      PF-G004.CodCia FORMAT "999":U
      x-nomcia @ x-nomcia COLUMN-LABEL "Descripción" FORMAT "x(50)":U
      PF-G004.Admin FORMAT "Si/No":U
  ENABLE
      PF-G004.Admin
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 45.86 BY 4.31
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     FILL-IN-aplicacion AT ROW 1.15 COL 10 COLON-ALIGNED
     FILL-IN-User-Id AT ROW 2.46 COL 6
     FILL-IN-Comentario AT ROW 3.38 COL 10 COLON-ALIGNED
     Btn-add-cias AT ROW 5.65 COL 52.14
     Btn-del-cias AT ROW 6.65 COL 52.14
     SELECT-grp-user AT ROW 10.77 COL 3.72 NO-LABEL
     Btn-anade AT ROW 11.23 COL 29
     Btn-remove AT ROW 12.15 COL 29
     SELECT-grp-aplic AT ROW 10.77 COL 41 NO-LABEL
     BROWSE-1 AT ROW 5.31 COL 3.43
     " Usuario:" VIEW-AS TEXT
          SIZE 6.29 BY .46 AT ROW 2 COL 3.29
     " Compañías Asignadas:" VIEW-AS TEXT
          SIZE 16.57 BY .5 AT ROW 4.69 COL 3.57
     " Grupos Asignados:" VIEW-AS TEXT
          SIZE 13.72 BY .5 AT ROW 10.12 COL 3.29
     " Grupos de la Aplicación:" VIEW-AS TEXT
          SIZE 17.43 BY .5 AT ROW 10.12 COL 40.86
     RECT-1 AT ROW 2.19 COL 2
     RECT-5 AT ROW 10.31 COL 2
     RECT-6 AT ROW 4.88 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 66.57 BY 14.69
         FONT 4.

DEFINE FRAME F-control
     Btn-First AT ROW 1.23 COL 2
     Btn-Prev AT ROW 1.23 COL 6
     Btn-Next AT ROW 1.23 COL 10
     Btn-Last AT ROW 1.23 COL 14
     BUTTON-CONSULTA AT ROW 1.23 COL 20
     Btn-add AT ROW 1.23 COL 26.43
     Btn-del AT ROW 1.23 COL 34.14
     Btn-cancel AT ROW 1.23 COL 41.86
     Btn-save AT ROW 1.23 COL 49.57
     Btn-exit AT ROW 1.23 COL 57.29
     RECT-2 AT ROW 1 COL 1
     RECT-3 AT ROW 1 COL 24.72
     RECT-4 AT ROW 1 COL 19
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 14.15
         SIZE 66.57 BY 1.54
         FONT 4.


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
         TITLE              = "Configuración de Usuarios"
         HEIGHT             = 14.69
         WIDTH              = 66.57
         MAX-HEIGHT         = 14.69
         MAX-WIDTH          = 66.57
         VIRTUAL-HEIGHT     = 14.69
         VIRTUAL-WIDTH      = 66.57
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("img\user":U) THEN
    MESSAGE "Unable to load icon: img\user"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
ASSIGN FRAME F-control:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-control
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR BUTTON Btn-cancel IN FRAME F-control
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME F-control
   1                                                                    */
/* SETTINGS FOR FRAME F-Main
   Custom                                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F-control:MOVE-AFTER-TAB-ITEM (SELECT-grp-aplic:HANDLE IN FRAME F-Main)
       XXTABVALXX = FRAME F-control:MOVE-BEFORE-TAB-ITEM (BROWSE-1:HANDLE IN FRAME F-Main)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB BROWSE-1 F-control F-Main */
/* SETTINGS FOR FILL-IN FILL-IN-aplicacion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-User-Id IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "integral.PF-G004"
     _Where[1]         = "PF-G004.Aplic-Id = s-aplic-id
 AND PF-G004.User-Id = INPUT FRAME F-Main FILL-IN-user-id"
     _FldNameList[1]   = integral.PF-G004.CodCia
     _FldNameList[2]   > "_<CALC>"
"x-nomcia @ x-nomcia" "Descripción" "x(50)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > integral.PF-G004.Admin
"PF-G004.Admin" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-control
/* Query rebuild information for FRAME F-control
     _Query            is NOT OPENED
*/  /* FRAME F-control */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Configuración de Usuarios */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Configuración de Usuarios */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
    IF Btn-exit:SENSITIVE IN FRAME F-Control = FALSE THEN RETURN NO-APPLY.
  
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON VALUE-CHANGED OF BROWSE-1 IN FRAME F-Main
DO:
    ASSIGN ROWID-act = IF AVAILABLE PF-G004 THEN ROWID(PF-G004) ELSE ?.
    RUN carga-grupos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-control
&Scoped-define SELF-NAME Btn-add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-add W-Win
ON CHOOSE OF Btn-add IN FRAME F-control /* Adicionar */
DO:
    IF AVAILABLE PF-G004 THEN ASSIGN ROWID-act = ROWID(PF-G004).
    ELSE ASSIGN ROWID-act = ?.

    RUN deshabilita-campos("nuevo registro").

    APPLY "ENTRY" TO FILL-IN-User-Id IN FRAME F-Main.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn-add-cias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-add-cias W-Win
ON CHOOSE OF Btn-add-cias IN FRAME F-Main /* Añadir */
DO:
    ASSIGN
        cias-selecc = ""
        cias-usuario = "".
    FOR EACH b-G004 WHERE
        b-G004.Aplic-Id = s-aplic-id AND
        b-G004.User-Id = FILL-IN-User-Id NO-LOCK:
        IF cias-usuario = "" THEN ASSIGN cias-usuario = STRING(b-G004.CodCia, "999").
        ELSE ASSIGN cias-usuario = cias-usuario + "," + STRING(b-G004.CodCia, "999").
    END.
    RUN bin/_user1.r(INPUT cias-usuario , OUTPUT cias-selecc ).
    IF cias-selecc <> ? OR cias-selecc <> "" THEN RUN crea_cias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-anade
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-anade W-Win
ON CHOOSE OF Btn-anade IN FRAME F-Main /* << Añadir */
DO:
    IF SELECT-grp-aplic:SCREEN-VALUE = ? THEN RETURN NO-APPLY.

    DEFINE VARIABLE estado AS LOGICAL.

    ASSIGN
        ESTADO = SELECT-grp-user:ADD-LAST(SELECT-grp-aplic:SCREEN-VALUE)
        ESTADO = SELECT-grp-aplic:DELETE(SELECT-grp-aplic:SCREEN-VALUE).

    IF Btn-remove:SENSITIVE = FALSE THEN ASSIGN Btn-remove:SENSITIVE = TRUE.
    IF SELECT-grp-aplic:LIST-ITEMS = ? THEN
        ASSIGN Btn-anade:SENSITIVE = FALSE.
    ELSE
        ASSIGN
            SELECT-grp-aplic:SCREEN-VALUE =
            SELECT-grp-aplic:ENTRY(SELECT-grp-aplic:NUM-ITEMS).
    ASSIGN
        SELECT-grp-user:SCREEN-VALUE =
        SELECT-grp-user:ENTRY(SELECT-grp-user:NUM-ITEMS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-control
&Scoped-define SELF-NAME Btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-cancel W-Win
ON CHOOSE OF Btn-cancel IN FRAME F-control /* Cancelar */
DO:
    FIND PF-G004 WHERE ROWID(PF-G004) = ROWID-act NO-LOCK NO-ERROR.
    IF NOT AVAILABLE PF-G004 THEN
        FIND FIRST PF-G004 WHERE PF-G004.Aplic-Id = s-aplic-id NO-LOCK NO-ERROR.
    IF AVAILABLE PF-G004 THEN DO:
        RUN habilita-campos.
        RUN carga-usuario.
    END.
    ELSE RUN deshabilita-campos("sin registros").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-del W-Win
ON CHOOSE OF Btn-del IN FRAME F-control /* Eliminar */
DO:
    BELL.
    MESSAGE "Está seguro de eliminar este usuario" SKIP
        "de esta aplicación" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE rpta AS LOGICAL.
    IF rpta <> TRUE THEN RETURN NO-APPLY.
    RUN elimina-usuario.
    FIND PREV PF-G004 WHERE PF-G004.Aplic-Id = s-aplic-id NO-LOCK NO-ERROR.
    IF NOT AVAILABLE PF-G004 THEN
        FIND FIRST PF-G004 WHERE PF-G004.Aplic-Id = s-aplic-id NO-LOCK NO-ERROR.
    IF AVAILABLE PF-G004 THEN RUN carga-usuario.
    ELSE RUN deshabilita-campos("sin registros").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn-del-cias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-del-cias W-Win
ON CHOOSE OF Btn-del-cias IN FRAME F-Main /* Quitar */
DO:

    DEFINE VARIABLE i AS INTEGER.

    FOR EACH b-G004 WHERE
        b-G004.Aplic-Id = s-aplic-id AND
        b-G004.User-Id = FILL-IN-User-Id NO-LOCK:
        ASSIGN i = i + 1.
    END.

    IF i = 1 THEN DO:
        BELL.
        MESSAGE "No puede dejar a este usuario" SKIP
            "sin compañía asignada" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
    END.

    IF NOT AVAILABLE PF-G004 THEN RETURN NO-APPLY.
    RUN elimina-registro(ROWID(PF-G004)).

    {&OPEN-QUERY-{&BROWSE-NAME}}

    APPLY "VALUE-CHANGED" TO BROWSE-1 IN FRAME F-Main.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-control
&Scoped-define SELF-NAME Btn-exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-exit W-Win
ON CHOOSE OF Btn-exit IN FRAME F-control /* Salir */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-First
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-First W-Win
ON CHOOSE OF Btn-First IN FRAME F-control /* First */
DO:
    FIND FIRST PF-G004 WHERE PF-G004.Aplic-Id = s-aplic-id NO-LOCK NO-ERROR.
    IF AVAILABLE PF-G004 THEN RUN carga-usuario.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Last W-Win
ON CHOOSE OF Btn-Last IN FRAME F-control /* Last */
DO:
    FIND LAST PF-G004 WHERE PF-G004.Aplic-Id = s-aplic-id NO-LOCK NO-ERROR.
    IF AVAILABLE PF-G004 THEN RUN carga-usuario.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Next W-Win
ON CHOOSE OF Btn-Next IN FRAME F-control /* Next */
DO:
    FIND LAST PF-G004 WHERE
        PF-G004.Aplic-Id = s-aplic-id AND
        PF-G004.User-Id = INPUT FRAME F-Main FILL-IN-User-Id NO-LOCK NO-ERROR.
    IF AVAILABLE PF-G004 THEN DO:
        FIND NEXT PF-G004 WHERE PF-G004.Aplic-Id = s-aplic-id NO-LOCK NO-ERROR.
        IF AVAILABLE PF-G004 THEN RUN carga-usuario.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Prev W-Win
ON CHOOSE OF Btn-Prev IN FRAME F-control /* Prev */
DO:
    FIND FIRST PF-G004 WHERE
        PF-G004.Aplic-Id = s-aplic-id AND
        PF-G004.User-Id = INPUT FRAME F-Main FILL-IN-User-Id NO-LOCK NO-ERROR.
    IF AVAILABLE PF-G004 THEN DO:
        FIND PREV PF-G004 WHERE PF-G004.Aplic-Id = s-aplic-id NO-LOCK NO-ERROR.
        IF AVAILABLE PF-G004 THEN RUN carga-usuario.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn-remove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-remove W-Win
ON CHOOSE OF Btn-remove IN FRAME F-Main /* Remover >> */
DO:
    IF SELECT-grp-user:SCREEN-VALUE = ? THEN RETURN NO-APPLY.

    DEFINE VARIABLE estado AS LOGICAL.

    ASSIGN
        ESTADO = SELECT-grp-aplic:ADD-LAST(SELECT-grp-user:SCREEN-VALUE)
        ESTADO = SELECT-grp-user:DELETE(SELECT-grp-user:SCREEN-VALUE).

    IF Btn-anade:SENSITIVE = FALSE THEN ASSIGN Btn-anade:SENSITIVE = TRUE.
    IF SELECT-grp-user:LIST-ITEMS = ? THEN ASSIGN Btn-remove:SENSITIVE = FALSE.

    IF SELECT-grp-aplic:LIST-ITEMS = ? THEN
        ASSIGN Btn-remove:SENSITIVE = FALSE.
    ELSE
        ASSIGN
            SELECT-grp-user:SCREEN-VALUE =
            SELECT-grp-user:ENTRY(SELECT-grp-user:NUM-ITEMS).
    ASSIGN
        SELECT-grp-aplic:SCREEN-VALUE =
        SELECT-grp-aplic:ENTRY(SELECT-grp-aplic:NUM-ITEMS).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-control
&Scoped-define SELF-NAME Btn-save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-save W-Win
ON CHOOSE OF Btn-save IN FRAME F-control /* Grabar */
DO:

    IF Btn-add:SENSITIVE = TRUE THEN DO:
        ASSIGN FRAME F-Main FILL-IN-Comentario.
        RUN graba_usuario.
        RUN graba_grupos(ROWID-act).
        RETURN.
    END.

    IF INPUT FRAME F-Main FILL-IN-User-Id = "" THEN DO:
        BELL.
        MESSAGE "No se permiten usuarios en blanco" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO FILL-IN-User-Id.
        RETURN NO-APPLY.
    END.
    IF CAN-FIND(FIRST PF-G004 WHERE
        PF-G004.Aplic-Id = s-aplic-id AND
        PF-G004.User-Id = INPUT FRAME F-Main FILL-IN-User-Id) THEN DO:
        BELL.
        MESSAGE "Usuario ya registrado para esta aplicación"
            VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO FILL-IN-User-Id.
        RETURN NO-APPLY.
    END.
    RUN habilita-campos.
    RUN crea-registro.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-CONSULTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-CONSULTA W-Win
ON CHOOSE OF BUTTON-CONSULTA IN FRAME F-control
DO:
    DEFINE VARIABLE list-user AS CHARACTER.
    DEFINE VARIABLE usuario  AS CHARACTER.

    FOR EACH b-G004 WHERE b-G004.Aplic-Id = s-aplic-id NO-LOCK:
        IF list-user = "" THEN ASSIGN list-user = b-G004.User-Id.
        ELSE DO:
            IF LOOKUP(b-G004.User-Id, list-user) = 0 THEN
                ASSIGN list-user = list-user + "," + b-G004.User-Id.
        END.
    END.
    RUN bin/_hlista.r(
        INPUT list-user ,
        INPUT "USUARIOS: APLICACION " + FILL-IN-aplicacion ,
        OUTPUT usuario ).
    IF usuario <> ? OR usuario <> "" THEN DO:
        FIND FIRST PF-G004 WHERE
            PF-G004.Aplic-Id = s-aplic-id AND
            PF-G004.User-Id = usuario NO-LOCK NO-ERROR.
        IF AVAILABLE PF-G004 THEN RUN carga-usuario.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME SELECT-grp-aplic
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-grp-aplic W-Win
ON MOUSE-SELECT-DBLCLICK OF SELECT-grp-aplic IN FRAME F-Main
DO:
    IF Btn-anade:SENSITIVE = TRUE THEN APPLY "CHOOSE" TO Btn-anade.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-grp-user
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-grp-user W-Win
ON MOUSE-SELECT-DBLCLICK OF SELECT-grp-user IN FRAME F-Main
DO:
    IF Btn-remove:SENSITIVE = TRUE THEN APPLY "CHOOSE" TO Btn-remove.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-control
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

ON FIND OF PF-G004 DO:
    FIND GN-CIAS WHERE GN-CIAS.codcia = PF-G004.codcia NO-LOCK NO-ERROR.
    IF AVAILABLE GN-CIAS THEN ASSIGN x-nomcia = GN-CIAS.nomcia.
    ELSE
        IF PF-G004.codcia = 0 THEN ASSIGN x-nomcia = "Acceso a todas las compañías".
        ELSE ASSIGN x-nomcia = "ERROR!!... Compañía no registrada en el sistema".
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-grupos W-Win 
PROCEDURE carga-grupos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE i      AS INTEGER.
    DEFINE VARIABLE estado AS LOGICAL.

    ASSIGN
        SELECT-grp-user:LIST-ITEMS IN FRAME F-Main = PF-G004.Seguridad.
        SELECT-grp-aplic:LIST-ITEMS IN FRAME F-Main = "".

    IF grupos-aplic = "" THEN DO:
        ASSIGN
            Btn-anade:SENSITIVE = FALSE
            Btn-remove:SENSITIVE = FALSE.
        RETURN.
    END.

    IF SELECT-grp-user:LIST-ITEMS = ? THEN DO:
        ASSIGN
            SELECT-grp-aplic:LIST-ITEMS = grupos-aplic
            SELECT-grp-aplic:SCREEN-VALUE = SELECT-grp-aplic:ENTRY(1)
            Btn-remove:SENSITIVE = FALSE
            Btn-anade:SENSITIVE = TRUE.
    END.
    ELSE DO:
        DO i = 1 TO NUM-ENTRIES(grupos-aplic):
            IF LOOKUP(ENTRY(i, grupos-aplic), SELECT-grp-user:LIST-ITEMS) = 0 THEN DO:
                ESTADO = SELECT-grp-aplic:ADD-LAST(ENTRY(i, grupos-aplic)).
            END.
        END.
        IF SELECT-grp-aplic:LIST-ITEMS = ? THEN ASSIGN Btn-anade:SENSITIVE = FALSE.
        ELSE SELECT-grp-aplic:SCREEN-VALUE = SELECT-grp-aplic:ENTRY(1).
        ASSIGN
            SELECT-grp-user:SCREEN-VALUE = SELECT-grp-user:ENTRY(1)
            Btn-remove:SENSITIVE = TRUE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-usuario W-Win 
PROCEDURE carga-usuario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT AVAILABLE PF-G004 THEN RETURN.

    ASSIGN FILL-IN-User-Id = PF-G004.User-Id.

    FIND DICTDB._user WHERE DICTDB._user._userid = PF-G004.User-Id NO-LOCK NO-ERROR.
    IF AVAILABLE DICTDB._user THEN
        ASSIGN FILL-IN-Comentario = DICTDB._user._user-name.
    ELSE ASSIGN FILL-IN-Comentario = "".

    DISPLAY
        FILL-IN-Comentario
        FILL-IN-User-Id
        WITH FRAME F-Main.

    {&OPEN-QUERY-{&BROWSE-NAME}}

    APPLY "VALUE-CHANGED" TO BROWSE-1 IN FRAME F-Main.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crea-registro W-Win 
PROCEDURE crea-registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN FRAME F-Main FILL-IN-User-Id FILL-IN-Comentario.

    CREATE PF-G004.
    ASSIGN
        PF-G004.Aplic-Id = s-aplic-id
        PF-G004.User-Id = FILL-IN-User-Id.

    RUN graba_usuario.
    RUN carga-usuario.

END PROCEDURE.

PROCEDURE graba_usuario:

    FIND DICTDB._user WHERE DICTDB._user._userid = FILL-IN-User-Id NO-ERROR.
    IF NOT AVAILABLE DICTDB._user THEN DO:
        CREATE DICTDB._user.
        ASSIGN
            DICTDB._user._userid = FILL-IN-User-Id
            DICTDB._User._Password = ENCODE("").
    END.
    ASSIGN DICTDB._user._user-name = FILL-IN-Comentario.

END PROCEDURE.

PROCEDURE graba_grupos:

    DEFINE INPUT PARAMETER ROWID-reg AS ROWID.

    FIND PF-G004 WHERE ROWID(PF-G004) = ROWID-reg NO-ERROR.
    IF AVAILABLE PF-G004 THEN
        ASSIGN PF-G004.Seguridad = SELECT-grp-user:LIST-ITEMS IN FRAME F-Main.
    ELSE DO:
        BELL.
        MESSAGE "Registro de usuario no encontrado" VIEW-AS ALERT-BOX.
    END.

END PROCEDURE.

PROCEDURE crea_cias:

    DEFINE VARIABLE i AS INTEGER.

    DO i = 1 TO NUM-ENTRIES(cias-selecc):
        CREATE b-G004.
        ASSIGN
            b-G004.Aplic-Id = s-aplic-id
            b-G004.User-Id = FILL-IN-User-Id
            b-G004.CodCia = INTEGER(ENTRY(i, cias-selecc)).
    END.

    {&OPEN-QUERY-{&BROWSE-NAME}}

    APPLY "VALUE-CHANGED" TO BROWSE-1 IN FRAME F-Main.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deshabilita-campos W-Win 
PROCEDURE deshabilita-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER sin_reg AS CHARACTER.

    ASSIGN
        BROWSE-1:SENSITIVE IN FRAME F-Main = FALSE
        Btn-add-cias:SENSITIVE IN FRAME F-Main = FALSE
        Btn-del-cias:SENSITIVE IN FRAME F-Main = FALSE
        Btn-anade:SENSITIVE IN FRAME F-Main = FALSE
        Btn-remove:SENSITIVE IN FRAME F-Main = FALSE
        SELECT-grp-aplic:SENSITIVE IN FRAME F-Main = FALSE
        SELECT-grp-user:SENSITIVE IN FRAME F-Main = FALSE
        Btn-del:SENSITIVE IN FRAME F-Control = FALSE
        Btn-First:SENSITIVE IN FRAME F-Control = FALSE
        Btn-Last:SENSITIVE IN FRAME F-Control = FALSE
        Btn-Next:SENSITIVE IN FRAME F-Control = FALSE
        Btn-Prev:SENSITIVE IN FRAME F-Control = FALSE
        BUTTON-CONSULTA:SENSITIVE IN FRAME F-Control = FALSE
        FILL-IN-User-Id:SCREEN-VALUE = ""
        FILL-IN-Comentario:SCREEN-VALUE = ""
        SELECT-grp-aplic:LIST-ITEMS = ""
        SELECT-grp-user:LIST-ITEMS = "".

    IF sin_reg = "sin registros" THEN
        ASSIGN
            FILL-IN-User-Id:SENSITIVE IN FRAME F-Main = FALSE
            FILL-IN-Comentario:SENSITIVE IN FRAME F-Main = FALSE
            Btn-add:SENSITIVE IN FRAME F-Control = TRUE
            Btn-cancel:SENSITIVE IN FRAME F-Control = FALSE
            Btn-save:SENSITIVE IN FRAME F-Control = FALSE
            Btn-exit:SENSITIVE IN FRAME F-Control = TRUE.
    ELSE
        ASSIGN
            FILL-IN-User-Id:SENSITIVE IN FRAME F-Main = TRUE
            FILL-IN-Comentario:SENSITIVE IN FRAME F-Main = TRUE
            Btn-add:SENSITIVE IN FRAME F-Control = FALSE
            Btn-cancel:SENSITIVE IN FRAME F-Control = TRUE
            Btn-save:SENSITIVE IN FRAME F-Control = TRUE
            Btn-exit:SENSITIVE IN FRAME F-Control = FALSE.

    {&OPEN-QUERY-{&BROWSE-NAME}}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE elimina-usuario W-Win 
PROCEDURE elimina-usuario :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH PF-G004 WHERE
        PF-G004.Aplic-Id = s-aplic-id AND
        PF-G004.User-Id = INPUT FRAME F-Main FILL-IN-User-Id NO-LOCK:
        RUN elimina-registro(ROWID(PF-G004)).
    END.

END PROCEDURE.

PROCEDURE elimina-registro.

    DEFINE INPUT PARAMETER ROWID-reg AS ROWID.

    FIND PF-G004 WHERE ROWID(PF-G004) = ROWID-reg NO-ERROR.
    IF AVAILABLE PF-G004 THEN DELETE PF-G004.

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
  DISPLAY FILL-IN-aplicacion FILL-IN-User-Id FILL-IN-Comentario SELECT-grp-user 
          SELECT-grp-aplic 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE FILL-IN-Comentario Btn-add-cias Btn-del-cias SELECT-grp-user Btn-anade 
         Btn-remove SELECT-grp-aplic BROWSE-1 RECT-1 RECT-5 RECT-6 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE Btn-First Btn-Prev Btn-Next Btn-Last BUTTON-CONSULTA Btn-add Btn-del 
         Btn-save Btn-exit RECT-2 RECT-3 RECT-4 
      WITH FRAME F-control IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-control}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE habilita-campos W-Win 
PROCEDURE habilita-campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN
        BROWSE-1:SENSITIVE IN FRAME F-Main = TRUE
        Btn-add-cias:SENSITIVE IN FRAME F-Main = TRUE
        Btn-del-cias:SENSITIVE IN FRAME F-Main = TRUE
        Btn-anade:SENSITIVE IN FRAME F-Main = TRUE
        Btn-remove:SENSITIVE IN FRAME F-Main = TRUE
        SELECT-grp-aplic:SENSITIVE IN FRAME F-Main = TRUE
        SELECT-grp-user:SENSITIVE IN FRAME F-Main = TRUE
        FILL-IN-User-Id:SENSITIVE IN FRAME F-Main = FALSE
        Btn-add:SENSITIVE IN FRAME F-Control = TRUE
        Btn-del:SENSITIVE IN FRAME F-Control = TRUE
        Btn-cancel:SENSITIVE IN FRAME F-Control = FALSE
        Btn-exit:SENSITIVE IN FRAME F-Control = TRUE
        Btn-First:SENSITIVE IN FRAME F-Control = TRUE
        Btn-Last:SENSITIVE IN FRAME F-Control = TRUE
        Btn-Next:SENSITIVE IN FRAME F-Control = TRUE
        Btn-Prev:SENSITIVE IN FRAME F-Control = TRUE
        BUTTON-CONSULTA:SENSITIVE IN FRAME F-Control = TRUE.

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
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

    FIND PF-G003 WHERE PF-G003.Aplic-Id = s-aplic-id NO-LOCK NO-ERROR.
    IF NOT AVAILABLE PF-G003 THEN DO:
        BELL.
        MESSAGE "Aplicación" s-aplic-id "ya no existe"
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
    END.

    ASSIGN
        FILL-IN-aplicacion = s-aplic-id + " " + PF-G003.Detalle
        grupos-aplic = PF-G003.Grupos.

    DISPLAY FILL-IN-aplicacion WITH FRAME F-Main.

    FIND FIRST PF-G004 WHERE PF-G004.Aplic-Id = s-aplic-id NO-LOCK NO-ERROR.
    IF AVAILABLE PF-G004 THEN RUN carga-usuario.
    ELSE RUN deshabilita-campos("sin registros").

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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "PF-G004"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

