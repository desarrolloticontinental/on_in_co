&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
/* Procedure Description
"Configurador de lookups (ALT-F8)"
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER campo-var    AS CHARACTER.
DEFINE INPUT PARAMETER program-call AS CHARACTER.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE OK AS LOGICAL NO-UNDO.
OK = SESSION:SET-WAIT-STATE("").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-10 FILL-IN-desc FILL-IN-prog-lookup ~
TGL-gnes Btn_archivos Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-campo FILL-IN-prog-call ~
FILL-IN-desc FILL-IN-prog-lookup TGL-gnes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_archivos 
     LABEL "A&rchivos" 
     SIZE 8.57 BY .85.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10.43 BY .85
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "A&yuda" 
     SIZE 10.43 BY .85
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Aceptar" 
     SIZE 10.43 BY .85
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-campo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Campo/Variable" 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-desc AS CHARACTER FORMAT "X(256)":U 
     LABEL "Descripción" 
     VIEW-AS FILL-IN 
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-prog-call AS CHARACTER FORMAT "X(16)":U 
     LABEL "Programa" 
     VIEW-AS FILL-IN 
     SIZE 22.57 BY .81 NO-UNDO.

DEFINE VARIABLE FILL-IN-prog-lookup AS CHARACTER FORMAT "X(256)":U 
     LABEL "Prog. Consulta" 
     VIEW-AS FILL-IN 
     SIZE 25.14 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49 BY 3.96.

DEFINE VARIABLE TGL-gnes AS LOGICAL INITIAL no 
     LABEL "Genérico" 
     VIEW-AS TOGGLE-BOX
     SIZE 9.86 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-campo AT ROW 1.54 COL 13 COLON-ALIGNED
     FILL-IN-prog-call AT ROW 2.35 COL 13 COLON-ALIGNED
     FILL-IN-desc AT ROW 3.15 COL 13 COLON-ALIGNED
     FILL-IN-prog-lookup AT ROW 3.96 COL 13 COLON-ALIGNED
     TGL-gnes AT ROW 2.38 COL 39
     Btn_archivos AT ROW 3.96 COL 40.29
     Btn_OK AT ROW 1.38 COL 53
     Btn_Cancel AT ROW 2.27 COL 53
     Btn_Help AT ROW 3.85 COL 53
     RECT-10 AT ROW 1.19 COL 1.72
     SPACE(14.41) SKIP(0.30)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Inscripción de Programas de Consulta"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-campo IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-prog-call IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Inscripción de Programas de Consulta */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_archivos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_archivos Dialog-Frame
ON CHOOSE OF Btn_archivos IN FRAME Dialog-Frame /* Archivos */
DO:
    DEFINE VARIABLE OKpressed AS LOGICAL.
    DEFINE VARIABLE path-main AS CHARACTER.

    ASSIGN
        FILE-INFO:FILE-NAME = ".".
        path-main = FILE-INFO:FULL-PATHNAME.

    SYSTEM-DIALOG GET-FILE FILL-IN-prog-lookup
    TITLE "Archivo(s) de Consulta..."
    FILTERS
        "Todos Fuentes (*.p,*.w)" "*.p *.w",
        "Ventanas (*.w)"          "*.w",
        "Procedimientos (*.p)"    "*.p",
        "Compilados (*.r)"        "*.r",
        "Todos (*.*)"             "*.*"
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
    IF OKpressed = TRUE THEN DO:
        IF FILL-IN-prog-lookup BEGINS PATH-MAIN THEN
            ASSIGN FILL-IN-prog-lookup = SUBSTR( FILL-IN-prog-lookup, LENGTH(path-main) + 2).
        ASSIGN FILL-IN-prog-lookup:SCREEN-VALUE = FILL-IN-prog-lookup.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Ayuda */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Aceptar */
DO:
    ASSIGN FILL-IN-desc FILL-IN-prog-lookup.

    FIND PF-G005 WHERE
        PF-G005.campo = campo-var AND
        PF-G005.programa_call = program-call NO-ERROR.
    IF NOT AVAILABLE PF-G005 THEN DO:
        IF FILL-IN-prog-lookup <> "" THEN DO:
            CREATE PF-G005.
            ASSIGN
                PF-G005.campo = campo-var
                PF-G005.programa_call = program-call.
        END.
        ELSE RETURN.
    END.
    ASSIGN
        PF-G005.descripcion = FILL-IN-desc
        PF-G005.programa_lookup = FILL-IN-prog-lookup.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TGL-gnes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TGL-gnes Dialog-Frame
ON VALUE-CHANGED OF TGL-gnes IN FRAME Dialog-Frame /* Genérico */
DO:
    ASSIGN TGL-gnes.
    IF TGL-gnes THEN ASSIGN FILL-IN-prog-call:SCREEN-VALUE = "*".
    ELSE ASSIGN FILL-IN-prog-call:SCREEN-VALUE = program-call.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

FIND PF-G005 WHERE
    PF-G005.campo = campo-var AND
    PF-G005.programa_call = program-call NO-LOCK NO-ERROR.
IF AVAILABLE PF-G005 THEN DO:
    ASSIGN
        FILL-IN-desc = PF-G005.descripcion
        FILL-IN-prog-lookup = PF-G005.programa_lookup.
END.

ASSIGN
    FILL-IN-campo = campo-var
    FILL-IN-prog-call = program-call.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-campo FILL-IN-prog-call FILL-IN-desc FILL-IN-prog-lookup 
          TGL-gnes 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-10 FILL-IN-desc FILL-IN-prog-lookup TGL-gnes Btn_archivos Btn_OK 
         Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


