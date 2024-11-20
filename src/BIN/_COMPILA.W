&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
/* Procedure Description
"Compilador de procedimientos
Rutina inicial en la instalación de la aplicación"
*/
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
DEFINE VARIABLE OK AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE APL-PROC
    FIELD APL-Nivel    AS INTEGER   FORMAT "99999" LABEL ""
    FIELD APL-IDENTIF  AS CHARACTER FORMAT "X(5)"  LABEL ""
    FIELD APL-FILTRO   AS CHARACTER FORMAT "X(3)"  LABEL ""
    FIELD APL-PATHNAME AS CHARACTER FORMAT "X(55)" LABEL "Directorio"
    FIELD APL-FILENAME AS CHARACTER FORMAT "X(33)" LABEL "Archivo"
    FIELD APL-FILETYPE AS CHARACTER FORMAT "X(4)"  LABEL "Tipo".
    
/* Capturando los directorios a compilar */
DEFINE VARIABLE s-IDENTIF  AS CHARACTER FORMAT "X(15)"      NO-UNDO.
DEFINE VARIABLE s-FILENAME AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE s-FULLNAME AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE s-FILETYPE AS CHARACTER FORMAT "X(1)"  NO-UNDO.
DEFINE VARIABLE s-FILEEXT  AS CHARACTER FORMAT "X(4)"  NO-UNDO.

DEFINE VARIABLE OK-PROCES   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE OK-STATUS   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE LINEA       AS CHARACTER NO-UNDO.
DEFINE VARIABLE DIRECTORIO  AS CHARACTER EXTENT 100.
DEFINE VARIABLE LISTA-DIR   AS CHARACTER NO-UNDO.
DEFINE VARIABLE DIR-POS-ACT AS INTEGER INITIAL 0.
DEFINE VARIABLE DIR-POS-MAX AS INTEGER INITIAL 0.
DEFINE VARIABLE error-index AS INTEGER.
DEFINE VARIABLE Nivel       AS INTEGER.
DEFINE VARIABLE Nro-Files   AS INTEGER.

DEFINE VARIABLE i           AS INTEGER.

DEFINE STREAM S-DIR.
DEFINE STREAM S-FILE.


/* Carga inicial */
DEFINE VARIABLE MENSAJE1 AS CHARACTER.
DEFINE VARIABLE MENSAJE  AS CHARACTER.
DEFINE VARIABLE H-ESPERA AS WIDGET-HANDLE.

MENSAJE1 = "Espera un momento por favor" + CHR(10) + "Procesando Directorio :".

FILE-INFO:FILE-NAME = ".\".
Nivel       = 0.
s-IDENTIF   = "".
Mensaje = MENSAJE1 + FILE-INFO:FULL-PATHNAME.
ASSIGN DIR-POS-MAX     = 1
       DIR-POS-ACT     = 1
       DIRECTORIO[ 1 ] = FILE-INFO:FULL-PATHNAME.

IF NOT VALID-HANDLE( H-ESPERA )
THEN DO:
    RUN BIN\_ESPERA.W  PERSISTENT SET H-ESPERA (MENSAJE).
    RUN dispatch IN H-ESPERA ('initialize':U).
END.

RUN Carga-archivos (FILE-INFO:FULL-PATHNAME).

s-IDENTIF   = "1".
DO WHILE DIR-POS-ACT <= DIR-POS-MAX:
    DIR-POS-ACT = DIR-POS-ACT + 1.
    FILE-INFO:FILE-NAME = DIRECTORIO[ DIR-POS-ACT ].
    IF FILE-INFO:FULL-PATHNAME <> ?
    THEN DO:
        Mensaje = MENSAJE1 + FILE-INFO:FULL-PATHNAME.
        IF VALID-HANDLE( H-ESPERA )
        THEN RUN MENSAJE IN H-ESPERA  (Mensaje).
        RUN Carga-archivos (FILE-INFO:FULL-PATHNAME).    
    END.
END.

/* Cargando directorios con programas unicamente */
IF VALID-HANDLE( H-ESPERA )
THEN RUN MENSAJE IN H-ESPERA  ("Filtrando unicamente directorios con archivos").

LISTA-DIR = "<Todos los directorios>".
DO DIR-POS-ACT = 1 TO DIR-POS-MAX:
    IF CAN-FIND(FIRST APL-PROC WHERE 
        APL-PROC.APL-PATHNAME = DIRECTORIO[ DIR-POS-ACT ] )
    THEN LISTA-DIR = LISTA-DIR + "," + DIRECTORIO[ DIR-POS-ACT ].
END.

IF VALID-HANDLE( H-ESPERA )
THEN DELETE PROCEDURE H-ESPERA.


DEFINE BUTTON BUTTON-DIR-ACEPTAR AUTO-GO 
     LABEL "Aceptar" 
     SIZE 9.72 BY .85.

DEFINE BUTTON BUTTON-DIR-CANCELAR AUTO-END-KEY 
     LABEL "Cancelar" 
     SIZE 9.72 BY .85.

DEFINE VARIABLE SELECT-1 AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 30.72 BY 6.31 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES APL-PROC

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 APL-PROC.APL-FILTRO APL-PROC.APL-PATHNAME APL-PROC.APL-FILENAME   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define FIELD-PAIRS-IN-QUERY-BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH APL-PROC BY APL-PROC.APL-IDENTIF.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 APL-PROC
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 APL-PROC


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 BUTTON-COMPILAR BUTTON-SELECCION ~
BUTTON-DESELEC BUTTON-AYUDA BUTTON-SALIR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-AYUDA 
     LABEL "A&yuda" 
     SIZE 11 BY .85.

DEFINE BUTTON BUTTON-COMPILAR 
     LABEL "&Compilar" 
     SIZE 11 BY .85.

DEFINE BUTTON BUTTON-DESELEC 
     LABEL "Deselecconar" 
     SIZE 11 BY .85.

DEFINE BUTTON BUTTON-SALIR 
     LABEL "&Salir" 
     SIZE 11 BY .85.

DEFINE BUTTON BUTTON-SELECCION 
     LABEL "Seleccionar" 
     SIZE 11 BY .85.

DEFINE BUTTON BUTTON-ACEPTAR 
     LABEL "&Aceptar" 
     SIZE 11 BY .81.

DEFINE BUTTON BUTTON-Cancelar 
     LABEL "&Cancelar" 
     SIZE 11 BY .81.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 74.14 BY 7.31 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 74.14 BY 1.92.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      APL-PROC SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Win _FREEFORM
  QUERY BROWSE-1 DISPLAY
      APL-PROC.APL-FILTRO
    APL-PROC.APL-PATHNAME
    APL-PROC.APL-FILENAME
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS MULTIPLE SIZE 69.29 BY 8.04
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-COMPILACION
     EDITOR-1 AT ROW 1.08 COL 1 NO-LABEL
     BUTTON-ACEPTAR AT ROW 8.92 COL 17
     BUTTON-Cancelar AT ROW 9 COL 46
     RECT-3 AT ROW 8.42 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 74.43 BY 9.5
         FONT 4.

DEFINE FRAME F-Main
     BROWSE-1 AT ROW 1.19 COL 3.57
     BUTTON-COMPILAR AT ROW 9.5 COL 3.86
     BUTTON-SELECCION AT ROW 9.5 COL 18.14
     BUTTON-DESELEC AT ROW 9.5 COL 32.43
     BUTTON-AYUDA AT ROW 9.5 COL 47.29
     BUTTON-SALIR AT ROW 9.5 COL 61.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         PAGE-TOP SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 74.43 BY 9.5
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
         TITLE              = "Compilador de procesos"
         HEIGHT             = 9.5
         WIDTH              = 74.43
         MAX-HEIGHT         = 9.5
         MAX-WIDTH          = 74.43
         VIRTUAL-HEIGHT     = 9.5
         VIRTUAL-WIDTH      = 74.43
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

IF NOT W-Win:LOAD-ICON("img\cfg":U) THEN
    MESSAGE "Unable to load icon: img\cfg"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* BROWSE-TAB BROWSE-1 1 F-Main */
/* SETTINGS FOR FRAME FRAME-COMPILACION
                                                                        */
ASSIGN 
       FRAME FRAME-COMPILACION:HIDDEN           = TRUE
       FRAME FRAME-COMPILACION:SENSITIVE        = FALSE.

/* SETTINGS FOR BUTTON BUTTON-ACEPTAR IN FRAME FRAME-COMPILACION
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-Cancelar IN FRAME FRAME-COMPILACION
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR EDITOR-1 IN FRAME FRAME-COMPILACION
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH APL-PROC BY APL-PROC.APL-IDENTIF
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-COMPILACION
/* Query rebuild information for FRAME FRAME-COMPILACION
     _Query            is NOT OPENED
*/  /* FRAME FRAME-COMPILACION */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{scr/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Compilador de procesos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Compilador de procesos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON ANY-KEY OF BROWSE-1 IN FRAME F-Main
DO:
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON MOUSE-SELECT-DBLCLICK OF BROWSE-1 IN FRAME F-Main
OR RETURN OF BROWSE-1
DO:
  IF APL-PROC.APL-FILTRO <> " +"
  THEN RETURN NO-APPLY.
  
  FILE-INFO:FILE-NAME = APL-PROC.APL-PATHNAME + "/" + APL-PROC.APL-FILENAME.
  S-IDENTIF   = APL-PROC.APL-IDENTIF.
  IF FILE-INFO:FILE-TYPE <> ? 
  THEN RUN Carga-archivos (FILE-INFO:FULL-PATHNAME).
  {&OPEN-QUERY-{&BROWSE-NAME}}
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Win
ON ROW-DISPLAY OF BROWSE-1 IN FRAME F-Main
DO:
  IF APL-PROC.APL-FILETYPE = "D"
  THEN APL-PROC.APL-FILENAME:BGCOLOR IN BROWSE BROWSE-1 = 8.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-COMPILACION
&Scoped-define SELF-NAME BUTTON-ACEPTAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-ACEPTAR W-Win
ON CHOOSE OF BUTTON-ACEPTAR IN FRAME FRAME-COMPILACION /* Aceptar */
DO:
   FRAME FRAME-COMPILACION:HIDDEN = YES.
   FRAME F-MAIN:HIDDEN            =  NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Cancelar W-Win
ON CHOOSE OF BUTTON-Cancelar IN FRAME FRAME-COMPILACION /* Cancelar */
DO:
  OK-PROCES = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-COMPILAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-COMPILAR W-Win
ON CHOOSE OF BUTTON-COMPILAR IN FRAME F-Main /* Compilar */
DO:
     DEFINE VARIABLE i AS INTEGER NO-UNDO.

     FRAME F-MAIN:HIDDEN    = YES.
     
     OK = EDITOR-1:LOAD-MOUSE-POINTER("WAIT") IN FRAME FRAME-COMPILACION.
     
     FRAME FRAME-COMPILACION:HIDDEN    = NO.
     EDITOR-1:SENSITIVE = YES.
     EDITOR-1:READ-ONLY = NO.
     DISPLAY BUTTON-ACEPTAR BUTTON-Cancelar EDITOR-1 RECT-3
        WITH FRAME FRAME-COMPILACION.

     ENABLE  BUTTON-Cancelar WITH FRAME FRAME-COMPILACION.
     DISABLE BUTTON-Aceptar WITH FRAME FRAME-COMPILACION.
     OK-PROCES = YES.
     
     DO i = 1 TO BROWSE-1:NUM-SELECTED-ROWS IN FRAME F-Main:
        IF BROWSE-1:FETCH-SELECTED-ROW(i)
        THEN DO:
        
            s-FILENAME = APL-PROC.APL-PATHNAME + "\" + APL-PROC.APL-FILENAME.
            
            LINEA = "Compilando : " + s-FILENAME + CHR(10).

            OK-STATUS = EDITOR-1:INSERT-STRING( LINEA ) IN FRAME FRAME-COMPILACION.
    
            RUN COMPILANDO( s-FILENAME ).   
 
        END.
        PROCESS EVENTS.
        IF NOT OK-PROCES THEN LEAVE.
     END.
     OK-STATUS = BROWSE-1:DESELECT-ROWS().

     IF OK-PROCES
     THEN LINEA = "Proceso completado" + CHR(10).
     ELSE LINEA = "Proceso cancelado por el usuario" + CHR(10).
     
     OK-PROCES = NO.
     OK-STATUS = EDITOR-1:INSERT-STRING( LINEA ) IN FRAME FRAME-COMPILACION.     
     EDITOR-1:READ-ONLY = YES.
     OK = EDITOR-1:LOAD-MOUSE-POINTER("").
     DISABLE BUTTON-Cancelar WITH FRAME FRAME-COMPILACION.
     ENABLE  BUTTON-Aceptar  WITH FRAME FRAME-COMPILACION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-DESELEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-DESELEC W-Win
ON CHOOSE OF BUTTON-DESELEC IN FRAME F-Main /* Deselecconar */
DO:
  OK-STATUS = BROWSE-1:DESELECT-ROWS() IN FRAME F-Main. 
  /*
  RUN DESELECCIONA.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SALIR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SALIR W-Win
ON CHOOSE OF BUTTON-SALIR IN FRAME F-Main /* Salir */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SELECCION
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SELECCION W-Win
ON CHOOSE OF BUTTON-SELECCION IN FRAME F-Main /* Seleccionar */
DO:
  RUN SELECCIONA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-COMPILACION
&Scoped-define SELF-NAME EDITOR-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR-1 W-Win
ON ENTRY OF EDITOR-1 IN FRAME FRAME-COMPILACION
DO:
  IF OK-PROCES THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */


 
{scr/adm/template/windowmn.i}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE carga-archivos W-Win 
PROCEDURE carga-archivos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER s-PATHNAME AS CHARACTER.
Nro-FILES = 0.

INPUT FROM OS-DIR( s-PATHNAME ).
REPEAT:
    IMPORT s-FILENAME s-FULLNAME s-FILETYPE.
    s-FILEEXT = "".
    IF LENGTH(s-FILENAME) > 2
    THEN s-FILEEXT = SUBSTR(s-FILENAME, LENGTH(s-FILENAME) - 1, 2).
    
    IF INDEX(s-FILETYPE, "F") = 0 AND INDEX(s-FILETYPE, "D") = 0
    THEN NEXT.
    
    IF INDEX(s-FILETYPE, "D") > 0 AND s-FILENAME = "."
    THEN NEXT.
    
    IF INDEX(s-FILETYPE, "D") > 0 AND s-FILENAME = ".." 
    THEN NEXT.
    
    IF INDEX(s-FILETYPE,"F") > 0 AND NOT ( s-FILEEXT = ".W" OR s-FILEEXT = ".P" )
    THEN NEXT.

    IF S-IDENTIF = "" AND INDEX(s-FILETYPE, "D") > 0
    THEN CASE S-FILENAME:
            WHEN "scr" THEN.
            WHEN "APL" THEN.
            OTHERWISE NEXT.
         END CASE.
         
    IF INDEX(s-FILETYPE, "F") > 0
    THEN DO:
        CREATE APL-PROC.
        ASSIGN APL-PROC.APL-NIVEL    = Nivel
               APL-PROC.APL-FILENAME = s-FILENAME
               APL-PROC.APL-PATHNAME = s-PATHNAME
               APL-PROC.APL-FILETYPE = s-FILETYPE
               APL-PROC.APL-FILTRO   = IF s-FILETYPE = "D" THEN "*" ELSE " "
               Nro-FILES = Nro-FILES + 1.
        END.
     ELSE ASSIGN DIR-POS-MAX               = DIR-POS-MAX + 1
                 DIRECTORIO[ DIR-POS-MAX ] = s-PATHNAME + "\" + s-FILENAME .

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE COMPILANDO W-Win 
PROCEDURE COMPILANDO :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER proc-name AS CHARACTER.
   LINEA = "".
   COMPILE VALUE(proc-name) SAVE NO-ERROR.
   IF COMPILER:ERROR THEN
   DO:
     DO error-index = 1 TO ERROR-STATUS:NUM-MESSAGES:
       LINEA = LINEA + "    ERROR : " + ERROR-STATUS:GET-MESSAGE(error-index) + CHR(10).
     END.
   END.
   ELSE IF COMPILER:WARNING THEN
   DO:
     DO error-index = 1 TO ERROR-STATUS:NUM-MESSAGES:
       LINEA = LINEA + "    ADVERTENCIA : " + ERROR-STATUS:GET-MESSAGE(error-index).
     END.

   END.

   OK-STATUS = EDITOR-1:INSERT-STRING( LINEA ) IN FRAME FRAME-COMPILACION.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DESELECCIONA W-Win 
PROCEDURE DESELECCIONA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE S-ROWID AS ROWID.
  DEFINE VARIABLE N       AS INTEGER.
  DEFINE FRAME FRAME-DIR
     SELECT-1 AT ROW 1.23 COL 2 NO-LABEL
     BUTTON-DIR-ACEPTAR  AT ROW 1.31 COL 33.43
     BUTTON-DIR-CANCELAR AT ROW 2.35 COL 33.43
    WITH VIEW-AS DIALOG-BOX 
         NO-LABELS NO-UNDERLINE THREE-D 
         AT COL 15.29 ROW 1.85
         SIZE 43.43 BY 7.69
         FONT 4
         TITLE "Deseleccionando ....".

  DO ON END-KEY UNDO WITH FRAME FRAME-DIR:
    SELECT-1:LIST-ITEMS = LISTA-DIR.
    UPDATE SELECT-1 BUTTON-DIR-ACEPTAR BUTTON-DIR-CANCELAR
        WITH FRAME FRAME-DIR.
    HIDE FRAME FRAME-DIR.
    IF LOOKUP("<Todos los directorios>", SELECT-1:SCREEN-VALUE) > 0
    THEN OK-STATUS = BROWSE-1:DESELECT-ROWS() IN FRAME F-Main. 
    ELSE DO i = 1 TO BROWSE-1:NUM-SELECTED-ROWS IN FRAME F-Main:
            IF BROWSE-1:FETCH-SELECTED-ROW(i)
            THEN IF LOOKUP(APL-PROC.APL-PATHNAME, SELECT-1:SCREEN-VALUE) > 0
                 THEN DO:
                    S-ROWID = ROWID( APL-PROC ).
                    REPOSITION BROWSE-1 TO ROWID s-ROWID.
                    N = BROWSE-1:GET-REPOSITIONED-ROW()   IN FRAME F-MAIN.
                    IF  BROWSE-1:DESELECT-SELECTED-ROW(N) IN FRAME F-MAIN
                    THEN I = 0.
                 END.
         END.
  END.
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
  ENABLE BROWSE-1 BUTTON-COMPILAR BUTTON-SELECCION BUTTON-DESELEC BUTTON-AYUDA 
         BUTTON-SALIR 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY EDITOR-1 
      WITH FRAME FRAME-COMPILACION IN WINDOW W-Win.
  ENABLE RECT-3 
      WITH FRAME FRAME-COMPILACION IN WINDOW W-Win.
  VIEW FRAME FRAME-COMPILACION IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-COMPILACION}
  FRAME FRAME-COMPILACION:SENSITIVE = NO.
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
  FRAME FRAME-COMPILACION:SENSITIVE = NO.
  FRAME FRAME-COMPILACION:VISIBLE   = NO.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE REPORTE W-Win 
PROCEDURE REPORTE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE DIRECTORIO AS CHARACTER NO-UNDO INITIAL "scr\BIN".
DEFINE VARIABLE ARCHIVO    AS CHARACTER FORMAT "X(25)" NO-UNDO.
DEFINE VARIABLE DETALLE    AS CHARACTER FORMAT "X(80)"NO-UNDO.
DEFINE VARIABLE LINEA      AS CHARACTER NO-UNDO.
DEFINE VARIABLE I          AS INTEGER   NO-UNDO.


DEFINE FRAME F-REP
    ARCHIVO    
    DETALLE 
    WITH FONT 2 WIDTH 132.


INPUT STREAM S-DIR FROM OS-DIR( DIRECTORIO ).
REPEAT WITH FRAME F-REP:
    IMPORT STREAM S-DIR ARCHIVO.
    IF NOT CAN-DO("*.i,*.w,*.p", ARCHIVO ) THEN NEXT.
    ARCHIVO = DIRECTORIO + "\" + ARCHIVO.
    DISPLAY ARCHIVO.  
    INPUT STREAM S-FILE FROM VALUE( ARCHIVO ).
    I =  0.
    DETALLE = "".
    REPEAT:
        I = I + 1. 
        IMPORT STREAM S-FILE UNFORMATTED LINEA.         
        IF I = 2 AND INDEX(Linea, "Procedure Description") = 0 THEN LEAVE.
        IF I > 2 AND Linea = "*" + "/" THEN LEAVE.
        IF I = 3 THEN DETALLE = RIGHT-TRIM(LINEA).
        IF I = 3 THEN LEAVE.
    END.                                

    IF DETALLE BEGINS '"' THEN DETALLE = SUBSTR(DETALLE, 2).
    IF LENGTH(DETALLE) > 0 AND SUBSTR(DETALLE, LENGTH(DETALLE), 1) = '"' AND LENGTH(DETALLE) > 0
            THEN DETALLE = SUBSTR(DETALLE, 1, LENGTH(DETALLE) - 1).
    
    DISPLAY DETALLE.
    INPUT STREAM S-FILE CLOSE.
END.
INPUT STREAM S-DIR CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SELECCIONA W-Win 
PROCEDURE SELECCIONA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE S-ROWID AS ROWID.
  DEFINE VARIABLE N       AS INTEGER.
  DEFINE FRAME FRAME-DIR
     SELECT-1 AT ROW 1.23 COL 2 NO-LABEL
     BUTTON-DIR-ACEPTAR  AT ROW 1.31 COL 33.43
     BUTTON-DIR-CANCELAR AT ROW 2.35 COL 33.43
    WITH VIEW-AS DIALOG-BOX 
         NO-LABELS NO-UNDERLINE THREE-D 
         DEFAULT-BUTTON BUTTON-DIR-ACEPTAR
         CANCEL-BUTTON BUTTON-DIR-CANCELAR
         AT COL 15.29 ROW 1.85
         SIZE 43.43 BY 7.69
         FONT 4
         TITLE "Seleccionando ....".

  DO :
      SELECT-1:LIST-ITEMS = LISTA-DIR.
      UPDATE SELECT-1 BUTTON-DIR-ACEPTAR BUTTON-DIR-CANCELAR
        WITH FRAME FRAME-DIR.
      HIDE FRAME FRAME-DIR.
      IF SELECT-1:SCREEN-VALUE = ? THEN LEAVE.
      
      FOR EACH APL-PROC:
        IF    LOOKUP(APL-PROC.APL-PATHNAME,     SELECT-1:SCREEN-VALUE) > 0
           OR LOOKUP("<Todos los directorios>", SELECT-1:SCREEN-VALUE) > 0
        THEN DO:
            S-ROWID = ROWID( APL-PROC ).
            REPOSITION BROWSE-1 TO ROWID s-ROWID.
            N = BROWSE-1:GET-REPOSITIONED-ROW()   IN FRAME F-MAIN.
            OK = BROWSE-1:SELECT-ROW(N) IN FRAME F-MAIN.
        END.

      END.
   END.    
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

  /* Define variables needed by this internal procedure.               */
  {scr/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {scr/adm/template/snd-list.i "APL-PROC"}

  /* Deal with any unexpected table requests before closing.           */
  {scr/adm/template/snd-end.i}

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


