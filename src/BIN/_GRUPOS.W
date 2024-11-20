&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
/* Procedure Description
"Asigna Grupos de acceso al generador de menús "
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME F-grupos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-grupos 
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

&IF "{&NEW}" = "" &THEN
    DEFINE OUTPUT PARAMETER lista-seleccion AS CHARACTER.
&ELSE
    DEFINE VARIABLE lista-seleccion AS CHARACTER.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE SHARED VARIABLE s-aplic-id AS CHARACTER.

FIND PF-G003 WHERE PF-G003.Aplic-Id = s-aplic-id NO-LOCK NO-ERROR.
IF NOT AVAILABLE PF-G003 THEN DO:
    BELL.
    MESSAGE "Registro de aplicación no existe" VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

DEFINE VARIABLE i  AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-grupos

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 SELECT-grupos Btn_selec Btn_deselec ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS SELECT-grupos 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 13.43 BY .88
     BGCOLOR 8 .

DEFINE BUTTON Btn_deselec 
     LABEL "&Deseleccionar" 
     SIZE 13.43 BY .88
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Aceptar" 
     SIZE 13.43 BY .88
     BGCOLOR 8 .

DEFINE BUTTON Btn_selec 
     LABEL "&Seleccionar Todo" 
     SIZE 13.43 BY .88
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39.72 BY 6.54.

DEFINE VARIABLE SELECT-grupos AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 37.72 BY 6.08 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-grupos
     SELECT-grupos AT ROW 1.42 COL 2.57 NO-LABEL
     Btn_selec AT ROW 1.73 COL 42.43
     Btn_deselec AT ROW 2.69 COL 42.43
     Btn_OK AT ROW 5.5 COL 42.43
     Btn_Cancel AT ROW 6.42 COL 42.43
     RECT-7 AT ROW 1.15 COL 1.57
     SPACE(15.70) SKIP(0.26)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Grupos de la Aplicación"
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
/* SETTINGS FOR DIALOG-BOX F-grupos
                                                                        */
ASSIGN 
       FRAME F-grupos:SCROLLABLE       = FALSE
       FRAME F-grupos:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-grupos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-grupos F-grupos
ON WINDOW-CLOSE OF FRAME F-grupos /* Grupos de la Aplicación */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_deselec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_deselec F-grupos
ON CHOOSE OF Btn_deselec IN FRAME F-grupos /* Deseleccionar */
DO:
    ASSIGN SELECT-grupos:SCREEN-VALUE = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK F-grupos
ON CHOOSE OF Btn_OK IN FRAME F-grupos /* Aceptar */
DO:
    IF SELECT-grupos:SCREEN-VALUE = ? THEN DO:
        BELL.
        MESSAGE "No hay selección alguna" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO SELECT-grupos.
        RETURN NO-APPLY.
    END.
    DO i = 1 TO SELECT-grupos:NUM-ITEMS:
        IF SELECT-grupos:IS-SELECTED(i) = TRUE THEN DO:
            IF lista-seleccion = "" THEN
                ASSIGN lista-seleccion = SELECT-grupos:ENTRY(i).
            ELSE
                ASSIGN lista-seleccion = lista-seleccion + "," +
                SELECT-grupos:ENTRY(i).
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_selec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_selec F-grupos
ON CHOOSE OF Btn_selec IN FRAME F-grupos /* Seleccionar Todo */
DO:
    ASSIGN SELECT-grupos:SCREEN-VALUE = SELECT-grupos:LIST-ITEMS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-grupos 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    ASSIGN SELECT-grupos:LIST-ITEMS IN FRAME F-grupos = PF-G003.Grupos.
    IF SELECT-grupos:LIST-ITEMS <> ? THEN
        ASSIGN SELECT-grupos:SCREEN-VALUE = SELECT-grupos:ENTRY(1).
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-grupos _DEFAULT-DISABLE
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
  HIDE FRAME F-grupos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-grupos _DEFAULT-ENABLE
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
  DISPLAY SELECT-grupos 
      WITH FRAME F-grupos.
  ENABLE RECT-7 SELECT-grupos Btn_selec Btn_deselec Btn_OK Btn_Cancel 
      WITH FRAME F-grupos.
  VIEW FRAME F-grupos.
  {&OPEN-BROWSERS-IN-QUERY-F-grupos}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


