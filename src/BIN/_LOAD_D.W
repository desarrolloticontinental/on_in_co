&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"Importar contenido de Tablas (DB PROGRESS)"
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
&Scoped-Define ENABLED-OBJECTS Btn_OK procname BUTTON-FILE Btn_Cancel ~
SELECT-1 Btn_Help BUTTON-9 BUTTON-8 
&Scoped-Define DISPLAYED-OBJECTS procname SELECT-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "A&yuda" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Aceptar" 
     SIZE 13 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-8 
     LABEL "Deseleccionar" 
     SIZE 13 BY 1.

DEFINE BUTTON BUTTON-9 
     LABEL "Seleccionar" 
     SIZE 13 BY 1.

DEFINE BUTTON BUTTON-FILE 
     LABEL "..." 
     SIZE 2.86 BY .81.

DEFINE VARIABLE procname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.86 BY .81 NO-UNDO.

DEFINE VARIABLE SELECT-1 AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
     SIZE 39 BY 8.85 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Btn_OK AT ROW 1.42 COL 42.86
     procname AT ROW 2.08 COL 2.14 NO-LABEL
     BUTTON-FILE AT ROW 2.08 COL 38.43
     Btn_Cancel AT ROW 2.62 COL 42.86
     SELECT-1 AT ROW 3.04 COL 2.29 NO-LABEL
     Btn_Help AT ROW 4.42 COL 42.86
     BUTTON-9 AT ROW 9.27 COL 42.86
     BUTTON-8 AT ROW 10.58 COL 42.86
     "Directorio de Importación:" VIEW-AS TEXT
          SIZE 38.57 BY .5 AT ROW 1.38 COL 2.14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 56.43 BY 11.73
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
         TITLE              = "Importar Contenido de Tablas"
         HEIGHT             = 11.81
         WIDTH              = 56.43
         MAX-HEIGHT         = 11.81
         MAX-WIDTH          = 56.43
         VIRTUAL-HEIGHT     = 11.81
         VIRTUAL-WIDTH      = 56.43
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = yes
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("img\cfg":U) THEN
    MESSAGE "Unable to load icon: img\cfg"
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
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN procname IN FRAME F-Main
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Importar Contenido de Tablas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Importar Contenido de Tablas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel W-Win
ON CHOOSE OF Btn_Cancel IN FRAME F-Main /* Cancelar */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help W-Win
ON CHOOSE OF Btn_Help IN FRAME F-Main /* Ayuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
MESSAGE "Permite actualizar las tablas del sistema" skip
        "a partir de archivos de definiciones en":U SKIP
        "formato .d (DUMP TABLE CONTENTS)."
     VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK W-Win
ON CHOOSE OF Btn_OK IN FRAME F-Main /* Aceptar */
DO:
  RUN ACTUALIZA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 W-Win
ON CHOOSE OF BUTTON-8 IN FRAME F-Main /* Deseleccionar */
DO:
  DEFINE VARIABLE MATCH-FILE AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.86 BY .81 NO-UNDO.
  DEFINE VARIABLE I AS INTEGER.
  DEFINE VARIABLE NUEVA-LISTA AS CHARACTER NO-UNDO.
  
  MATCH-FILE = "*.*".
  DEFINE FRAME F-MSG-2
    SKIP(1)
    SPACE(2)
    MATCH-FILE  BGCOLOR 15 FGCOLOR 0
    SPACE(2)
    SKIP(1)
    WITH VIEW-AS DIALOG-BOX NO-LABEL TITLE "Deselección de Tablas"
         FONT 4 BGCOLOR 8 FGCOLOR 0.
  UPDATE MATCH-FILE WITH FRAME F-MSG-2.
  NUEVA-LISTA = ?.
  IF NOT MATCH-FILE = "*.*"
  THEN  DO WITH FRAME {&FRAME-NAME}:
      DO I = 1 TO NUM-ENTRIES(SELECT-1:SCREEN-VALUE):
         IF NOT CAN-DO(MATCH-FILE, ENTRY(I,SELECT-1:SCREEN-VALUE))
         THEN IF NUEVA-LISTA = ?
              THEN NUEVA-LISTA = ENTRY(I, SELECT-1:SCREEN-VALUE).
              ELSE NUEVA-LISTA = NUEVA-LISTA + "," + 
                                         ENTRY(I,SELECT-1:SCREEN-VALUE). 
      END.
  END.
  SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ?.
  SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = NUEVA-LISTA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 W-Win
ON CHOOSE OF BUTTON-9 IN FRAME F-Main /* Seleccionar */
DO:
  DEFINE VARIABLE MATCH-FILE AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.86 BY .81 NO-UNDO.
  DEFINE VARIABLE I AS INTEGER.
  
  MATCH-FILE = "*.*".
  DEFINE FRAME F-MSG-1
    SKIP(1)
    SPACE(2)
    MATCH-FILE  BGCOLOR 15 FGCOLOR 0
    SPACE(2)
    SKIP(1)
    WITH VIEW-AS DIALOG-BOX NO-LABEL TITLE "Selección de Tablas"
         FONT 4 BGCOLOR 8 FGCOLOR 0.
  UPDATE MATCH-FILE WITH FRAME F-MSG-1.
  DO WITH FRAME {&FRAME-NAME}:
      DO I = I TO SELECT-1:NUM-ITEMS:
         IF CAN-DO(MATCH-FILE, SELECT-1:ENTRY( I )) 
     /*       AND NOT SELECT-1:IS-SELECTED( "AA" ) */
         THEN IF SELECT-1:SCREEN-VALUE = ? OR SELECT-1:SCREEN-VALUE = ""
              THEN SELECT-1:SCREEN-VALUE = SELECT-1:ENTRY( I ).
              ELSE SELECT-1:SCREEN-VALUE = SELECT-1:SCREEN-VALUE + "," + 
                                                SELECT-1:ENTRY( I ).
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-FILE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-FILE W-Win
ON CHOOSE OF BUTTON-FILE IN FRAME F-Main /* ... */
DO:
  DEFINE VARIABLE ARCHIVO AS CHARACTER NO-UNDO INITIAL "*.d".
  DEFINE VARIABLE OKpressed AS LOGICAL INITIAL TRUE.
  SYSTEM-DIALOG GET-FILE ARCHIVO
        TITLE      "Directorio de Importaci¢n ..."
        FILTERS    "Source Files (*.d)"   "*.d"
        INITIAL-DIR procname
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.
      
    IF OKpressed = TRUE THEN DO:
        RUN adecomm/_osprefx.p ( ARCHIVO, output procname, output archivo ).
        FILE-INFO:FILE-NAME = procname.
        IF INDEX(FILE-INFO:FILE-TYPE,"D") = 0 OR FILE-INFO:FILE-TYPE = ?
        THEN DO:
            MESSAGE "Invalido directorio ingresado" VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
        procname:SCREEN-VALUE = FILE-INFO:FULL-PATHNAME.
        SELECT-1:SCREEN-VALUE = ?.
        RUN CARGA-d.
        SELECT-1:SCREEN-VALUE = ARCHIVO.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME procname
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL procname W-Win
ON RETURN OF procname IN FRAME F-Main
OR TAB OF procname
DO:
  assign procname.
  /* VERIFICANDO LA EXISTENCIA DEL ARCHIVO */
  FILE-INFO:FILE-NAME = procname.
  IF INDEX(FILE-INFO:FILE-TYPE,"D") = 0 OR FILE-INFO:FILE-TYPE = ?
  THEN DO:
    MESSAGE "Invalido directorio ingresado" VIEW-AS ALERT-BOX ERROR.
    RETURN NO-APPLY.
  END.
  procname:SCREEN-VALUE = FILE-INFO:FULL-PATHNAME.
  RUN CARGA-d.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
/* VM - INCLUDE PARA LA CREACION DEL MENU BAR */
{src/adm/template/cntnrwin.i}

{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE actualiza W-Win 
PROCEDURE actualiza :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ARCHIVO AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE I       AS INTEGER   NO-UNDO.
    MESSAGE SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} SKIP procname
    VIEW-AS ALERT-BOX.
/*    RUN prodict/load_d.p (SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}, "").      */
    RUN prodict/load_d.p ("All", ".").      
    SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-d W-Win 
PROCEDURE carga-d :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* cargando la lista de archivo .d */
  DEFINE VARIABLE ARCHIVO AS CHARACTER NO-UNDO.
  
  INPUT FROM OS-DIR (procname).
  SELECT-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ?.
  SELECT-1:LIST-ITEMS   IN FRAME {&FRAME-NAME} = ?.
  
  REPEAT:
    IMPORT ARCHIVO.
    IF LOOKUP("d", ARCHIVO,".") <> 0 THEN
      OK = SELECT-1:ADD-LAST( ARCHIVO ) IN FRAME {&FRAME-NAME}.
  END.
  INPUT CLOSE.
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
  DISPLAY procname SELECT-1 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE Btn_OK procname BUTTON-FILE Btn_Cancel SELECT-1 Btn_Help BUTTON-9 
         BUTTON-8 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects W-Win 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  procname = ".\".
  FILE-INFO:FILE-NAME = procname.
  procname = FILE-INFO:FULL-PATHNAME.
  RUN CARGA-d.
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

