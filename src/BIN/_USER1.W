&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
/* Procedure Description
"Consulta de Cias para el registro de usuarios"
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME F-cias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-cias 
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
    DEFINE INPUT  PARAMETER cias-usuario AS CHARACTER.
    DEFINE OUTPUT PARAMETER cias-asigna  AS CHARACTER.
&ELSE
    DEFINE VARIABLE cias-usuario AS CHARACTER INITIAL "001".
    DEFINE VARIABLE cias-asigna  AS CHARACTER.
&ENDIF

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE lista-cias AS CHARACTER NO-UNDO.
DEFINE VARIABLE i          AS INTEGER NO-UNDO.
DEFINE VARIABLE OK         AS LOGICAL NO-UNDO.
OK = SESSION:SET-WAIT-STATE("").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-cias

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-7 SELECT-cias Btn_selec Btn_deselec ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS SELECT-cias 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 13.14 BY .88
     BGCOLOR 8 .

DEFINE BUTTON Btn_deselec 
     LABEL "&Deseleccionar" 
     SIZE 13.14 BY .88
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Aceptar" 
     SIZE 13.14 BY .88
     BGCOLOR 8 .

DEFINE BUTTON Btn_selec 
     LABEL "&Seleccionar Todo" 
     SIZE 13.14 BY .88
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39.86 BY 6.65.

DEFINE VARIABLE SELECT-cias AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 37.72 BY 6.08 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-cias
     SELECT-cias AT ROW 1.5 COL 2.43 NO-LABEL
     Btn_selec AT ROW 1.69 COL 42.72
     Btn_deselec AT ROW 2.65 COL 42.72
     Btn_OK AT ROW 5.58 COL 42.72
     Btn_Cancel AT ROW 6.54 COL 42.72
     RECT-7 AT ROW 1.19 COL 1.43
     SPACE(15.70) SKIP(0.19)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Compañías del Sistema"
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
/* SETTINGS FOR DIALOG-BOX F-cias
                                                                        */
ASSIGN 
       FRAME F-cias:SCROLLABLE       = FALSE
       FRAME F-cias:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-cias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-cias F-cias
ON WINDOW-CLOSE OF FRAME F-cias /* Compañías del Sistema */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_deselec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_deselec F-cias
ON CHOOSE OF Btn_deselec IN FRAME F-cias /* Deseleccionar */
DO:
    ASSIGN SELECT-cias:SCREEN-VALUE = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK F-cias
ON CHOOSE OF Btn_OK IN FRAME F-cias /* Aceptar */
DO:
    IF SELECT-cias:SCREEN-VALUE = ? THEN DO:
        BELL.
        MESSAGE "No hay selección alguna" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO SELECT-cias.
        RETURN NO-APPLY.
    END.
    DO i = 1 TO SELECT-cias:NUM-ITEMS:
        IF SELECT-cias:IS-SELECTED(i) = TRUE THEN DO:
            IF cias-asigna = "" THEN
                ASSIGN cias-asigna = SUBSTR(SELECT-cias:ENTRY(i),1,3).
            ELSE
                ASSIGN cias-asigna = cias-asigna + "," +
                SUBSTR(SELECT-cias:ENTRY(i),1,3).
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_selec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_selec F-cias
ON CHOOSE OF Btn_selec IN FRAME F-cias /* Seleccionar Todo */
DO:
    ASSIGN SELECT-cias:SCREEN-VALUE = SELECT-cias:LIST-ITEMS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-cias 


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
    RUN inicializa.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-cias _DEFAULT-DISABLE
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
  HIDE FRAME F-cias.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-cias _DEFAULT-ENABLE
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
  DISPLAY SELECT-cias 
      WITH FRAME F-cias.
  ENABLE RECT-7 SELECT-cias Btn_selec Btn_deselec Btn_OK Btn_Cancel 
      WITH FRAME F-cias.
  VIEW FRAME F-cias.
  {&OPEN-BROWSERS-IN-QUERY-F-cias}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inicializa F-cias 
PROCEDURE inicializa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    FOR EACH GN-CIAS NO-LOCK:
        IF LOOKUP(STRING(GN-CIAS.CodCia, "999"), cias-usuario) = 0 THEN DO:
            IF lista-cias = "" THEN
                ASSIGN
                    lista-cias = STRING(GN-CIAS.CodCia, "999")
                    + " " + GN-CIAS.NomCia.
            ELSE
                ASSIGN
                    lista-cias = lista-cias + "," + STRING(GN-CIAS.CodCia, "999")
                    + " " + GN-CIAS.NomCia.
        END.
    END.

    ASSIGN SELECT-cias:LIST-ITEMS IN FRAME F-Cias = lista-cias.

    IF SELECT-cias:LIST-ITEMS <> ? THEN
        ASSIGN SELECT-cias:SCREEN-VALUE = SELECT-cias:ENTRY(1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


