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
DEFINE SHARED VARIABLE s-port-name        AS CHARACTER.
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

/* CAPTURAMOS EL SISTEMA OPERATIVO */
DEF VAR x-OpSys AS CHAR NO-UNDO.

GET-KEY-VALUE SECTION 'Startup' KEY 'OpSysVersion' VALUE x-OpSys.

IF x-OpSys = 'WinNT' THEN port-list = printer-list.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-impresion

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS R-salida CMB-impresora FILL-IN-archivo ~
Btn-archivos R-rango FILL-IN-pagini Btn-up-1 Btn-down-1 FILL-IN-pagfin ~
Btn-up-2 Btn-down-2 FILL-IN-copias Btn-up-3 Btn-down-3 Btn-aceptar ~
Btn-cancelar Btn-ayuda RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS R-salida CMB-impresora FILL-IN-puerto ~
FILL-IN-archivo R-rango FILL-IN-pagini FILL-IN-pagfin FILL-IN-copias 

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

DEFINE BUTTON Btn-archivos 
     IMAGE-UP FILE "img\pvpegar":U
     LABEL "&Arc..." 
     SIZE 3.57 BY 1.

DEFINE BUTTON Btn-ayuda 
     LABEL "A&yuda" 
     SIZE 10 BY .88.

DEFINE BUTTON Btn-cancelar AUTO-END-KEY 
     LABEL "&Cancelar" 
     SIZE 10 BY .88.

DEFINE BUTTON Btn-down-1 
     IMAGE-UP FILE "img\b-down":U
     LABEL "" 
     SIZE 1.86 BY .5.

DEFINE BUTTON Btn-down-2 
     IMAGE-UP FILE "img\b-down":U
     LABEL "" 
     SIZE 1.86 BY .5.

DEFINE BUTTON Btn-down-3 
     IMAGE-UP FILE "img\b-down":U
     LABEL "" 
     SIZE 1.86 BY .5.

DEFINE BUTTON Btn-up-1 
     IMAGE-UP FILE "img\b-up":U
     LABEL "" 
     SIZE 1.86 BY .5.

DEFINE BUTTON Btn-up-2 
     IMAGE-UP FILE "img\b-up":U
     LABEL "" 
     SIZE 1.86 BY .5.

DEFINE BUTTON Btn-up-3 
     IMAGE-UP FILE "img\b-up":U
     LABEL "" 
     SIZE 1.86 BY .5.

DEFINE VARIABLE CMB-impresora AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 32 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-archivo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29.57 BY .81
     BGCOLOR 15 FGCOLOR 0 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-copias AS INTEGER FORMAT "ZZ9":U INITIAL 1 
     LABEL "Copias" 
     VIEW-AS FILL-IN 
     SIZE 4.57 BY .81
     BGCOLOR 15 FGCOLOR 0 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-pagfin AS INTEGER FORMAT "ZZZZ9":U INITIAL 1 
     LABEL "A" 
     VIEW-AS FILL-IN 
     SIZE 6.29 BY .81
     BGCOLOR 15 FGCOLOR 0 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-pagini AS INTEGER FORMAT "ZZZZ9":U INITIAL 1 
     LABEL "De" 
     VIEW-AS FILL-IN 
     SIZE 6.29 BY .81
     BGCOLOR 15 FGCOLOR 0 FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-puerto AS CHARACTER FORMAT "X(256)":U 
     LABEL "En" 
     VIEW-AS FILL-IN 
     SIZE 29.86 BY .81 NO-UNDO.

DEFINE VARIABLE R-rango AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todo", 1,
"Paginas", 2
     SIZE 8.72 BY 1.58 NO-UNDO.

DEFINE VARIABLE R-salida AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pantalla", 1,
"Impresora", 2,
"Archivo", 3
     SIZE 9.57 BY 3.23 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 13 BY 4.04.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 37 BY 2.35.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 37 BY 1.42.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 34 BY 2.31.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 2.31.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-impresion
     R-salida AT ROW 1.77 COL 3.86 NO-LABEL
     CMB-impresora AT ROW 1.77 COL 16.72 COLON-ALIGNED NO-LABEL
     FILL-IN-puerto AT ROW 2.69 COL 18.86 COLON-ALIGNED
     FILL-IN-archivo AT ROW 4.35 COL 16.72 COLON-ALIGNED NO-LABEL
     Btn-archivos AT ROW 4.23 COL 48.57
     R-rango AT ROW 6.12 COL 3.86 NO-LABEL
     FILL-IN-pagini AT ROW 6.88 COL 14.57 COLON-ALIGNED
     Btn-up-1 AT ROW 6.81 COL 22.86
     Btn-down-1 AT ROW 7.27 COL 22.86
     FILL-IN-pagfin AT ROW 6.88 COL 24.86 COLON-ALIGNED
     Btn-up-2 AT ROW 6.81 COL 33.14
     Btn-down-2 AT ROW 7.27 COL 33.14
     FILL-IN-copias AT ROW 6.46 COL 42.43 COLON-ALIGNED
     Btn-up-3 AT ROW 6.38 COL 49
     Btn-down-3 AT ROW 6.85 COL 49
     Btn-aceptar AT ROW 1.81 COL 54.57
     Btn-cancelar AT ROW 2.81 COL 54.57
     Btn-ayuda AT ROW 4.15 COL 54.57
     " Salida a:" VIEW-AS TEXT
          SIZE 7 BY .5 AT ROW 1.15 COL 3.57
     " Rango de Impresion:" VIEW-AS TEXT
          SIZE 15.14 BY .5 AT ROW 5.46 COL 3.57
     " Impresora:" VIEW-AS TEXT
          SIZE 8.14 BY .5 AT ROW 1.15 COL 17.86
     " Archivo:" VIEW-AS TEXT
          SIZE 6.86 BY .42 AT ROW 3.85 COL 17.86
     RECT-1 AT ROW 1.38 COL 2
     RECT-2 AT ROW 1.38 COL 16.14
     RECT-3 AT ROW 4 COL 16.14
     RECT-4 AT ROW 5.69 COL 2
     RECT-5 AT ROW 5.69 COL 37.14
     SPACE(12.71) SKIP(0.30)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 4
         TITLE "Configuracion de Impresion"
         DEFAULT-BUTTON Btn-aceptar.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX F-impresion
   FRAME-NAME Custom                                                    */
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
        R-salida
        R-rango
        FILL-IN-copias
        FILL-IN-pagini
        FILL-IN-pagfin
        FILL-IN-archivo
        FILL-IN-puerto
        CMB-impresora.


    IF R-rango = 1 THEN
        ASSIGN
            s-pagina-inicial = 1
            s-pagina-final   = 99999.
    ELSE
        ASSIGN
            s-pagina-inicial = FILL-IN-pagini
            s-pagina-final   = FILL-IN-pagfin.
   
    CASE R-Salida:
    WHEN 2 THEN
        IF printer-list = "" THEN DO:
            BELL.
            MESSAGE "No existen impresoras configuradas" VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO R-salida.
            RETURN NO-APPLY.
        END.
    WHEN 3 THEN DO:
        IF FILL-IN-archivo = "" THEN DO:
            BELL.
            MESSAGE "Ingrese nombre del archivo de impresión"
                VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO FILL-IN-archivo.
            RETURN NO-APPLY.
        END.
        ELSE DO:
            IF R-INDEX( FILL-IN-archivo, "\" ) = 0 THEN
                ASSIGN
                    FILL-IN-archivo = ".\txt\" + FILL-IN-archivo
                    FILE-INFO:FILE-NAME = FILL-IN-archivo.
            IF R-INDEX(FILL-IN-archivo, "\" ) = 0 THEN
                ASSIGN FILE-INFO:FILE-NAME = ".".
            ELSE
                ASSIGN
                    FILE-INFO:FILE-NAME =
                    SUBSTR( FILL-IN-archivo, 1,(R-INDEX(FILL-IN-archivo,"\") - 1)).
            IF FILE-INFO:FILE-TYPE = ? OR
                R-INDEX(FILE-INFO:FILE-TYPE, "D" ) = 0 THEN DO:
                BELL.
                MESSAGE "Inválido directorio a grabar" SKIP
                    FILE-INFO:FILE-NAME VIEW-AS ALERT-BOX ERROR.
                APPLY "ENTRY" TO FILL-IN-archivo.
                RETURN NO-APPLY.
            END.
            IF R-INDEX(FILE-INFO:FILE-TYPE, "W" ) = 0 THEN DO:
                BELL.
                MESSAGE "Acceso no autorizado" SKIP
                    "al directorio " FILE-INFO:FILE-NAME
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "ENTRY" TO FILL-IN-archivo.
                RETURN NO-APPLY.
            END.
            FILE-INFO:FILE-NAME = FILL-IN-archivo.
            IF FILE-INFO:PATHNAME <> ? THEN DO:                  
                BELL.
                MESSAGE "Archivo ya existe," SKIP
                    "Desea reemplazarlo?" VIEW-AS ALERT-BOX QUESTION
                    BUTTONS YES-NO UPDATE cfm AS LOGICAL.
                IF cfm NE TRUE THEN DO:
                    APPLY "ENTRY" TO FILL-IN-archivo.
                    RETURN NO-APPLY.
                END.
            END.
        END.
    END.
    END CASE.

    ASSIGN
        FILL-IN-archivo:SCREEN-VALUE = FILL-IN-archivo
        s-printer-name               = CMB-impresora
        s-port-name                  = FILL-IN-puerto
        s-salida-impresion           = R-salida
        s-print-file                 = FILL-IN-archivo
        s-nro-copias                 = FILL-IN-copias
        s-orientacion                = 1.
    s-port-name = REPLACE(s-port-name, ":", "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-archivos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-archivos F-impresion
ON CHOOSE OF Btn-archivos IN FRAME F-impresion /* Arc... */
DO:
    SYSTEM-DIALOG GET-FILE FILL-IN-archivo
    TITLE   "Archivo(s) de Impresión..."
    FILTERS
        "Archivo Impresión (*.txt)" "*.txt",
        "Todos (*.*)"               "*.*"
    INITIAL-DIR "./txt"
    /*MUST-EXIST*/
    USE-FILENAME
    UPDATE OKpressed.
    IF OKpressed = TRUE THEN FILL-IN-archivo:SCREEN-VALUE = FILL-IN-archivo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-ayuda F-impresion
ON CHOOSE OF Btn-ayuda IN FRAME F-impresion /* Ayuda */
OR HELP OF FRAME {&FRAME-NAME} DO:
    BELL.
    MESSAGE "Ayuda Para Archivo: {&FILE-NAME}":U VIEW-AS ALERT-BOX INFORMATION.
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


&Scoped-define SELF-NAME Btn-down-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-down-1 F-impresion
ON CHOOSE OF Btn-down-1 IN FRAME F-impresion
DO:
    IF INPUT R-rango = 1 THEN RETURN.
    IF INPUT FILL-IN-pagini - 1 <= 0 THEN RETURN NO-APPLY.
    ELSE DISPLAY INPUT FILL-IN-pagini - 1 @ FILL-IN-pagini WITH FRAME F-impresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-down-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-down-2 F-impresion
ON CHOOSE OF Btn-down-2 IN FRAME F-impresion
DO:
    IF INPUT R-rango = 1 THEN RETURN.
    IF INPUT FILL-IN-pagfin - 1 <= 0 THEN RETURN NO-APPLY.
    ELSE DISPLAY INPUT FILL-IN-pagfin - 1 @ FILL-IN-pagfin WITH FRAME F-impresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-down-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-down-3 F-impresion
ON CHOOSE OF Btn-down-3 IN FRAME F-impresion
DO:
    IF INPUT R-salida <> 2 THEN RETURN.
    IF INPUT FILL-IN-copias - 1 <= 0 THEN RETURN NO-APPLY.
    ELSE DISPLAY INPUT FILL-IN-copias - 1 @ FILL-IN-copias WITH FRAME F-impresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-up-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-up-1 F-impresion
ON CHOOSE OF Btn-up-1 IN FRAME F-impresion
DO:
    IF INPUT R-rango = 1 THEN RETURN.
    IF INPUT FILL-IN-pagini + 1 > 99999 THEN RETURN NO-APPLY.
    ELSE DISPLAY INPUT FILL-IN-pagini + 1 @ FILL-IN-pagini WITH FRAME F-impresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-up-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-up-2 F-impresion
ON CHOOSE OF Btn-up-2 IN FRAME F-impresion
DO:
    IF INPUT R-rango = 1 THEN RETURN.
    IF INPUT FILL-IN-pagfin + 1 > 99999 THEN RETURN NO-APPLY.
    ELSE DISPLAY INPUT FILL-IN-pagfin + 1 @ FILL-IN-pagfin WITH FRAME F-impresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-up-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-up-3 F-impresion
ON CHOOSE OF Btn-up-3 IN FRAME F-impresion
DO:
    IF INPUT R-salida <> 2 THEN RETURN.
    IF INPUT FILL-IN-copias + 1 > 999 THEN RETURN NO-APPLY.
    ELSE DISPLAY INPUT FILL-IN-copias + 1 @ FILL-IN-copias WITH FRAME F-impresion.
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


&Scoped-define SELF-NAME FILL-IN-archivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-archivo F-impresion
ON ANY-PRINTABLE OF FILL-IN-archivo IN FRAME F-impresion
DO:
    IF R-INDEX( "abcdefghijklmn¤opqrstuvwxyz1234567890:./\\" ,
        CAPS( CHR( LASTKEY ) ) ) = 0 THEN DO:  
        BELL.
        RETURN NO-APPLY.
    END.
    APPLY CAPS(CHR(LASTKEY)).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R-rango
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R-rango F-impresion
ON VALUE-CHANGED OF R-rango IN FRAME F-impresion
DO:
    IF INPUT R-rango = 1 THEN
        ASSIGN
            FILL-IN-pagfin:SENSITIVE = FALSE
            FILL-IN-pagini:SENSITIVE = FALSE
            Btn-down-1:SENSITIVE     = FALSE
            Btn-down-2:SENSITIVE     = FALSE
            Btn-up-1:SENSITIVE       = FALSE
            Btn-up-2:SENSITIVE       = FALSE.
    ELSE
        ASSIGN
            FILL-IN-pagfin:SENSITIVE = TRUE
            FILL-IN-pagini:SENSITIVE = TRUE
            Btn-down-1:SENSITIVE     = TRUE
            Btn-down-2:SENSITIVE     = TRUE
            Btn-up-1:SENSITIVE       = TRUE
            Btn-up-2:SENSITIVE       = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R-salida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R-salida F-impresion
ON VALUE-CHANGED OF R-salida IN FRAME F-impresion
DO:
    ASSIGN
        Btn-archivos:SENSITIVE = TRUE
        CMB-impresora:SENSITIVE = TRUE
        FILL-IN-archivo:SENSITIVE = TRUE
        FILL-IN-copias:SENSITIVE = TRUE
        Btn-down-3:SENSITIVE = FALSE
        Btn-up-3:SENSITIVE = FALSE.

    CASE INPUT R-Salida:
    WHEN 1 THEN DO:
        ASSIGN
            Btn-archivos:SENSITIVE    = FALSE
            CMB-impresora:SENSITIVE   = FALSE
            FILL-IN-Archivo:SENSITIVE = FALSE
            FILL-IN-Copias:SENSITIVE  = FALSE.
        DISPLAY 1 @ FILL-IN-Copias WITH FRAME F-impresion.
    END.
    WHEN 2 THEN DO:
        ASSIGN
            Btn-archivos:SENSITIVE    = FALSE
            FILL-IN-archivo:SENSITIVE = FALSE
            Btn-down-3:SENSITIVE = TRUE
            Btn-up-3:SENSITIVE = TRUE.
        DISPLAY s-nro-copias @ FILL-IN-Copias WITH FRAME F-impresion.
    END.
    WHEN 3 THEN DO:
        ASSIGN
            CMB-impresora:SENSITIVE  = FALSE
            FILL-IN-Copias:SENSITIVE = FALSE.
        DISPLAY 1 @ FILL-IN-Copias WITH FRAME F-impresion.
    END.
    END CASE.
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
    APPLY "VALUE-CHANGED" TO R-Salida.
    APPLY "VALUE-CHANGED" TO R-rango.
    APPLY "VALUE-CHANGED" TO CMB-impresora.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.

    RUN disable_UI.

    RETURN.

END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI F-impresion  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI F-impresion  _DEFAULT-ENABLE
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
  DISPLAY R-salida CMB-impresora FILL-IN-puerto FILL-IN-archivo R-rango 
          FILL-IN-pagini FILL-IN-pagfin FILL-IN-copias 
      WITH FRAME F-impresion.
  ENABLE R-salida CMB-impresora FILL-IN-archivo Btn-archivos R-rango 
         FILL-IN-pagini Btn-up-1 Btn-down-1 FILL-IN-pagfin Btn-up-2 Btn-down-2 
         FILL-IN-copias Btn-up-3 Btn-down-3 Btn-aceptar Btn-cancelar Btn-ayuda 
         RECT-1 RECT-2 RECT-3 RECT-4 RECT-5 
      WITH FRAME F-impresion.
  {&OPEN-BROWSERS-IN-QUERY-F-impresion}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

