&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
/* Procedure Description
"Control de Impresión"
*/
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME F-impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS F-impresion 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 22/01/97 -  6:23 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Definici¢n de Variables Compartidas  ---                             */
DEFINE SHARED VARIABLE s-pagina-final     AS INTEGER.
DEFINE SHARED VARIABLE s-pagina-inicial   AS INTEGER.
DEFINE SHARED VARIABLE s-salida-impresion AS INTEGER.
DEFINE SHARED VARIABLE s-printer-name     AS CHARACTER.
DEFINE VARIABLE s-port-name        AS CHARACTER.
DEFINE SHARED VARIABLE s-print-file       AS CHARACTER.
DEFINE SHARED VARIABLE s-nro-copias       AS INTEGER.
DEFINE SHARED VARIABLE s-orientacion      AS INTEGER.

DEFINE VARIABLE OKpressed     AS LOGICAL.
DEFINE VARIABLE imp-predet    AS LOGICAL.
DEFINE VARIABLE printer-count AS INTEGER.
DEFINE VARIABLE printer-port  AS CHARACTER.
DEFINE VARIABLE printer-list  AS CHARACTER.
DEFINE VARIABLE port-list     AS CHARACTER.

RUN aderb/_prlist.p(
    OUTPUT printer-list,
    OUTPUT port-list,
    OUTPUT printer-count).

IF s-printer-name = "" THEN DO:
    RUN aderb/_prdef.p(
        OUTPUT s-printer-name,
        OUTPUT printer-port,
        OUTPUT imp-predet).
    IF imp-predet <> TRUE THEN DO:
        BELL.
        MESSAGE "No existe una impresora" SKIP
            "predeterminada en WINDOWS"
            VIEW-AS ALERT-BOX WARNING.
    END.
END.
ELSE DO:
    IF LOOKUP(s-printer-name, printer-list) = 0 THEN DO:
        RUN aderb/_prdef.p(
            OUTPUT s-printer-name,
            OUTPUT printer-port,
            OUTPUT imp-predet).
        IF imp-predet <> TRUE THEN DO:
            BELL.
            MESSAGE "No existe una impresora" SKIP
                "predeterminada en WINDOWS"
                VIEW-AS ALERT-BOX WARNING.
        END.
    END.
END.

IF s-nro-copias <= 0 THEN s-nro-copias = 1.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-impresion

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 CMB-impresora Btn-aceptar ~
Btn-cancelar 
&Scoped-Define DISPLAYED-OBJECTS CMB-impresora FILL-IN-puerto 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-aceptar AUTO-GO 
     LABEL "&Aceptar" 
     SIZE 10 BY .88.

DEFINE BUTTON Btn-cancelar AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY .88.

DEFINE VARIABLE CMB-impresora AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "","" 
     SIZE 32 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-puerto AS CHARACTER FORMAT "X(256)":U 
     LABEL "En" 
     VIEW-AS FILL-IN 
     SIZE 29.86 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 37 BY 2.35.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-impresion
     CMB-impresora AT ROW 2 COL 6.57 COLON-ALIGNED NO-LABEL
     FILL-IN-puerto AT ROW 2.92 COL 8.72 COLON-ALIGNED
     Btn-aceptar AT ROW 1.96 COL 50
     Btn-cancelar AT ROW 2.96 COL 50
     RECT-2 AT ROW 1.62 COL 6
     " Impresora:" VIEW-AS TEXT
          SIZE 8.14 BY .5 AT ROW 1.38 COL 20.86
     SPACE(36.85) SKIP(3.07)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 4
         TITLE "Configuración de Impresión"
         DEFAULT-BUTTON Btn-aceptar.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX F-impresion
   Custom                                                               */
ASSIGN 
       FRAME F-impresion:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN FILL-IN-puerto IN FRAME F-impresion
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX F-impresion
/* Query rebuild information for DIALOG-BOX F-impresion
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX F-impresion */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Btn-aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-aceptar F-impresion
ON CHOOSE OF Btn-aceptar IN FRAME F-impresion /* Aceptar */
DO:
    ASSIGN
        FILL-IN-PUERTO
        CMB-impresora.

        IF printer-list = "" THEN DO:
            BELL.
            MESSAGE "No existen impresoras configuradas" VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO CMB-impresora.
            RETURN NO-APPLY.
        END.

    ASSIGN
        s-printer-name               = CMB-impresora
        s-port-name                  = FILL-IN-puerto
        s-salida-impresion           = 2
        s-print-file                 = ""
        s-nro-copias                 = 1
        s-orientacion                = 1.

        ASSIGN
            s-pagina-inicial = 1
            s-pagina-final   = 99999.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-cancelar F-impresion
ON CHOOSE OF Btn-cancelar IN FRAME F-impresion /* Cancelar */
DO:
    ASSIGN s-salida-impresion = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB-impresora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB-impresora F-impresion
ON VALUE-CHANGED OF CMB-impresora IN FRAME F-impresion
DO:
    IF printer-list <> "" THEN
        ASSIGN FILL-IN-puerto = ENTRY(LOOKUP(INPUT CMB-impresora, printer-list), port-list).
    ELSE ASSIGN FILL-IN-puerto = "".
    DISPLAY FILL-IN-puerto WITH FRAME F-impresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK F-impresion 


IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

MAIN-BLOCK:
DO ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    ASSIGN
        CMB-impresora:LIST-ITEMS = printer-list
        CMB-impresora = s-printer-name.

    RUN enable_UI.
    APPLY "VALUE-CHANGED" TO CMB-impresora.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.

    RUN disable_UI.

    RETURN.

END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-impresion _DEFAULT-DISABLE
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
  HIDE FRAME F-impresion.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-impresion _DEFAULT-ENABLE
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
  DISPLAY CMB-impresora FILL-IN-puerto 
      WITH FRAME F-impresion.
  ENABLE RECT-2 CMB-impresora Btn-aceptar Btn-cancelar 
      WITH FRAME F-impresion.
  {&OPEN-BROWSERS-IN-QUERY-F-impresion}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


